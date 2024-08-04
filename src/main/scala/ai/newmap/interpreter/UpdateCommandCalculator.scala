package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}
import java.util.UUID

// Evaluates an expression that's already been type checked
object UpdateCommandCalculator {
  val pairT: NewMapType = IndexT(UIndex(2))
  val defaultUMap: UntaggedObject = UMap()

  def getDefaultValueOfCommandType(nType: NewMapType, env: Environment): Outcome[UntaggedObject, String] = {
    getDefaultValueOfCommandTypeFromEnv(nType.asUntagged, env).rescue(_ => {
      getDefaultValueOfCommandTypeHardcoded(nType, env)
    })
  }

  /*
   * This is getDefaultValueOfCommandType being slowly written into newmap code
   * It is a map that takes certain types (called command types) and outputs their default, or initial value
   * The initial value of Count is 0, for example
   */
  def getDefaultValueOfCommandTypeFromEnv(uType: UntaggedObject, env: Environment): Outcome[UntaggedObject, String] = {
    for {
      result <- Evaluator.findFieldValue(uType, UIdentifier("init"), env)
      func <- Evaluator.applyFunction(result, UIndex(0), env)

      value <- func match {
        case UCase(typeOfFunc, valueOfFunc) => Success(valueOfFunc)
        case _ => Failure("unexpected object: " + func)
      }
    } yield value
  }

  def getDefaultValueOfCommandTypeHardcoded(nType: NewMapType, env: Environment): Outcome[UntaggedObject, String] = {
    nType match {
      // TODO - start removing these in favor of newmap code!
      case IndexT(UIndex(i)) if i > 0 => Success(UIndex(0)) //REmove?
      case MapT(typeTransform, MapConfig(completeness, _, _, _, _)) => completeness match {
        case RequireCompleteness if (!typeTransformHasEmptyKey(typeTransform)) => {
          Failure(s"Can't start off map with key in typeTransform $typeTransform")
        }
        case _ => Success(defaultUMap)
      }
      case StructT(_, _, CommandOutput, _) => {
        Success(defaultUMap)
      }
      case StructT(params, _, _, _) if (params.getMapBindings().toOption.exists(_.isEmpty)) => {
        Success(defaultUMap)
      }
      case CharacterT => Success(UCharacter('\u0000'))
      case ArrayT(_) => Success(UCase(UIndex(0), UArray()))
      case CustomT(name, params, typeSystemId) => {
        for {
          underlying <- TypeChecker.getUnderlyingType(name, params, env, typeSystemId)
          result <- getDefaultValueOfCommandTypeHardcoded(underlying, env)
        } yield result
      }
      case SequenceT(parent, featureSet) => Success(UCase(UIndex(0), defaultUMap))
      case _ => {
        Failure(s"Type ${nType.displayString(env)} has no default value")
      }
    }
  }

  def typeTransformHasEmptyKey(typeTransform: TypeTransform): Boolean = {
    typeTransform.keyType match {
      case IndexT(UIndex(0)) | IndexT(UInit) => true
      case SubtypeT(m, _, _) => m.getMapBindings().toOption.exists(_.isEmpty)
      case _ => false // TODO - unimplemented
    }
  }

  def getCommandInputOfCommandType(
    nType: NewMapType,
    env: Environment
  ): Outcome[NewMapType, String] = {
    nType match {
      case CountT => Success(NewMapO.emptyStruct)
      case BooleanT => Success(pairT)
      case MapT(typeTransform, MapConfig(PartialMap, _, _, _, _)) => {
        // In this case, there must be a key expansion type
        // TODO: enforce this?

        // Key Expansion + requiredValue expansion
        // What if Key expansion is a case? (for now we don't allow this, only basic map)

        val keyT = typeTransform.keyType
        val requiredValuesT = typeTransform.valueType

        keyT match {
          case StructT(UMap(Vector()), _, _, _) => {
            // TODO - this is an ugly exception.. we need a better way to add fields to a struct
            // (particularly an empty struct like in this case)
            Success(requiredValuesT)
          }
          case _ => {
            Success(StructT(
              UArray(keyT.asUntagged, requiredValuesT.asUntagged),
              pairT
            ))
          }
        }
      }
      case MapT(typeTransform, _) => {
        // Now instead of giving the structT, we must give something else!!
        // we have typeTransform
        // we need a pair (a, b) that satisfies the type transform
        // This is a (pattern/expression) pairing
        // the type of the expression depends on the value of the pattern??
        // -- therefore, this is a binding!
        // -- should we make a binding a first class object, and should typeTransform just be a binding?
        for {
          outputCommandT <- getCommandInputOfCommandType(typeTransform.valueType, env)
        } yield {
          StructT(
            UArray(typeTransform.keyType.asUntagged, outputCommandT.asUntagged),
            pairT
          )
        }
      }
      case StructT(parameterList, parentFieldType, _, featureSet) => {
        // Change to CaseT because we are adding a single parameter!
        // TODO: rethink this, maybe the input should actually be a struct of the same type to overlay the old struct
        // - or not the same type, but the command type of each field!
        Success(CaseT(parameterList, parentFieldType, featureSet))
      }
      case ArrayT(nType) => Success(nType)
      case CustomT(typeName, params, typeSystemId) => {
        for {
          underlyingTypeInfo <- env.typeSystem.historicalUnderlyingType(typeName, typeSystemId)

          (underlyingPattern, underlyingExp) = underlyingTypeInfo

          patternMatchSubstitutions <- Evaluator.patternMatch(underlyingPattern, params, StandardMatcher, env)

          underlyingType = MakeSubstitution(underlyingExp.asUntagged, patternMatchSubstitutions)

          underlyingT <- underlyingType.asType
          result <- getCommandInputOfCommandType(underlyingT, env)
        } yield {
          result
        }
      }
      case SequenceT(parent, featureSet) => Success(parent)
      case _ => {
        Success(UndefinedT)
      }
    }
  }

  def updateVersionedObject(
    current: NewMapObject,
    command: UntaggedObject,
    env: Environment
  ): Outcome[UntaggedObject, String] = {
    for {
      uObject <- updateVersionedObjectUnnormalized(current, command, env)
      nObject <- TypeChecker.tagAndNormalizeObject(uObject, current.nType, env)
    } yield nObject.uObject
  }

  // Eventually, this will return an UntaggedObject, because we're not going to change the type
  private def updateVersionedObjectUnnormalized(
    current: NewMapObject,
    command: UntaggedObject,
    env: Environment
  ): Outcome[UntaggedObject, String] = {
    current.nType match {
      case nType@CountT => {
        for {
          c <- current.uObject match {
            case UIndex(i) => Success(i)
            case UInit => Success(0L)
            case _ => Failure(s"Couldn't interpret count value: ${current.uObject}")
          }
        } yield UCase(UIdentifier("Inc"), UIndex(c))
      }
      case BooleanT => {
        for {
          currentValue <- TypeChecker.normalizeCount(current.uObject)
          j <- TypeChecker.normalizeCount(command)
        } yield {
          val result = if (currentValue == 1 || j == 1) 1 else 0
          UIndex(result)
        }
      }
      case SequenceT(parentT, featureSet) => {
        for {
          result <- current.uObject match {
            case UCase(UIndex(i), uSeq) => {
              for {
                oldBindings <- uSeq.getMapBindings
              } yield {
                val newBindings = oldBindings :+ (UIndex(i) -> command)
                UCase(UIndex(i + 1), UMap(newBindings))
              }
            }
            case _ => Failure("Problem with sequence command: " + command)
          }
        } yield {
          result
        }
      }
      case MapT(typeTransform, MapConfig(PartialMap, features, _, _, _)) => {
        val keyT = typeTransform.keyType
        val requiredValuesT = typeTransform.valueType

        for {
          result <- keyT match {
            case StructT(UMap(Vector()), _, _, _) => {
              // TODO - this is an ugly exception.. we need a better way to add fields to a struct
              // (particularly an empty struct like in this case)
              Success((NewMapObject(UMap(), keyT), command))
            }
            case _ => {

              for {
                commandPatterns <- command.getMapBindings()
                keyField <- Evaluator.applyFunction(command, UIndex(0), env)
                valueExpression <- Evaluator.patternMatchInOrder(commandPatterns, UIndex(1), env)
                valueExpressionEval<- Evaluator(valueExpression, env)
              } yield {
                (NewMapObject(keyField, keyT), valueExpressionEval)
              }
            }
          }

          (keyExpansionCommand, valueExpansionExpression) = result
          
          // Really we're updating the key?? [review soon]
          //updateKeyTypeResponse <- TypeExpander.expandType(keyT, keyExpansionCommand.uObject, env)

          // TODO- we need to do something with updateKeyTypeResponse.converter
                  
          mapValues <- current.uObject.getMapBindings()

          prepNewValues = for {
            value <- mapValues

            // Remove old value
            if (value._1 != keyExpansionCommand.uObject)
          } yield (value._1 -> value._2)

          newMapValues = (keyExpansionCommand.uObject -> valueExpansionExpression) +: prepNewValues
        } yield UMap(newMapValues)
      }
      case mapT@MapT(typeTransform, _) => {
        val outputType = typeTransform.valueType

        for {
          input <- Evaluator.applyFunction(command, UIndex(0), env)
          commandForInput <- Evaluator.applyFunction(command, UIndex(1), env)

          currentResultForInput <- Evaluator.applyFunction(current.uObject, input, env)

          newResultForInput <- updateVersionedObject(
            NewMapObject(currentResultForInput, outputType),
            commandForInput,
            env
          )

          mapValues <- current.uObject.getMapBindings()

          newMapValues = (input -> newResultForInput) +: mapValues.filter(x => x._1 != input)
        } yield {
          UMap(newMapValues)
        }
      }
      case StructT(parameterList, parentFieldType, RequireCompleteness, featureSet) => {
        for {
          mapValues <- current.uObject.getMapBindings()

          nameOfField <- Evaluator.applyFunction(command, UIndex(0), env)
          valueOfField <- Evaluator.applyFunction(command, UIndex(1), env)
        } yield {
          val newMapValues = (nameOfField -> valueOfField) +: mapValues.filter(x => x._1 != nameOfField)
          UMap(newMapValues)
        }
      }
      case StructT(params, parentFieldType, CommandOutput, _) => {
        command match {
          case UCase(constructor, input) => {
            for {
              mapValues <- current.uObject.getMapBindings()
            } yield {
              val newMapValues = (constructor -> input) +: mapValues.filter(x => x._1 != constructor)
              UMap(newMapValues)
            }
          }
          case _ => {
            Failure(s"A) Structs as commands haven't been implemented yet -- $params -- $command")
          }
        }
      }
      case nType@ArrayT(_) => {
        for {
          untaggedResult <- current.uObject match {
            case UCase(UIndex(length), UArray(values)) => {
              Success(UCase(UIndex(length + 1), UArray(values :+ command)))
            }
            case UCase(UIndex(length), UMap(values)) => {
              Success(UCase(UIndex(length + 1), UMap(values :+ (UIndex(length), command))))
            }
            case _ => Failure(s"Unknown array data: $current")
          }
        } yield untaggedResult
      }
      case nType@CustomT(typeName, params, typeSystemId) => {
        val customResultOutcome = {
          command match {
            case UCase(name, value) => {
              for {
                // TODO - take into account the typeSystemId
                // TODO - what if this intercepts a field that's not a command?
                func <- Evaluator(AccessField(current.uObject, current.nType.asUntagged, name), env)
                afterCommand <- Evaluator.applyFunction(func, value, env, StandardMatcher)
              } yield afterCommand
            }
            case _ => Failure("command $command in the wrong format for custom result")
          }
        }

        customResultOutcome.rescue(f => {
          for {
            underlyingTypeInfo <- env.typeSystem.historicalUnderlyingType(typeName, typeSystemId)

            (underlyingPattern, underlyingExp) = underlyingTypeInfo

            patternMatchSubstitutions <- Evaluator.patternMatch(underlyingPattern, params, StandardMatcher, env)

            underlyingType = MakeSubstitution(underlyingExp.asUntagged, patternMatchSubstitutions)

            underlyingT <- underlyingType.asType
            currentResolved <- TypeChecker.tagAndNormalizeObject(current.uObject, underlyingT, env)

            result <- updateVersionedObject(currentResolved, command, env)
          } yield result
        })
      }
      case _ => {
        Failure(s"C) ${current.displayString(env)} is not a command type, error in type checker -- ${current.nType}")
      }
    }
  }
}