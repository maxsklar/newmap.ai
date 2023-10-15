package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.util.{Outcome, Success, Failure}

/**
 * Handles expanding a type (adding more values or patterns)
 * This includes making the conversion function from one type to the next
 */
object TypeExpander {

  val pairT = IndexT(UIndex(2))
  
  // This returns the type of command needed to expand the given nType
  def getTypeExpansionCommandInput(
    nType: NewMapType,
    typeSystem: NewMapTypeSystem
  ): Outcome[NewMapType, String] = {
    nType match {
      case IndexT(_) => Success(NewMapO.emptyStruct) // Where to insert the new value?
      case CaseT(_, parentType, _) => {
        Success(StructT(
          UArray(parentType.asUntagged, TypeT.asUntagged),
          pairT
        ))
      }
      case StructT(_, parentType, _, _) => {
        Success(StructT(
          UArray(parentType.asUntagged, SubtypeT(IsCommandFunc, TypeT).asUntagged),
          pairT
        ))
      }
      case SubtypeT(_, parentType, _) => Success(parentType)
      case TypeClassT(typeTransform, _) => {
        Success(StructT(typeTransform, TypeT, CommandOutput, BasicMap))
      }
      //case MapT(keyType, valueType, config) => getTypeExpansionCommandInput(valueType, typeSystem)
      case CustomT(name, _, _) => {
        for {
          currentUnderlyingType <- typeSystem.currentUnderlyingType(name)

          commandInput <- getTypeExpansionCommandInput(currentUnderlyingType._2, typeSystem)
        } yield commandInput
      }
      case _ => Failure(s"Unable to expand key: $nType")
    }
  }

  val untaggedIdentity: UntaggedObject = UMap(Vector(UWildcard("_") -> ParamId("_")))

  case class ExpandKeyResponse(
    newType: NewMapType,
    newValues: Seq[UntaggedObject],
    converter: UntaggedObject // This is a function that can convert from the old type to the new type
  )

  def expandType(
    nType: NewMapType,
    command: UntaggedObject,
    env: Environment
  ): Outcome[ExpandKeyResponse, String] = {
    nType match {
      case IndexT(UIndex(i)) => {
        val newType = IndexT(UIndex(i + 1))
        Success(ExpandKeyResponse(newType, Vector(UIndex(i)), untaggedIdentity))
      }
      case CaseT(cases, parentType, featureSet) => {
        for {
          caseBindings <- cases.getMapBindings()

          uConstructors = caseBindings.map(x => x._1 -> UIndex(1))
          constructorsSubtype = SubtypeT(UMap(uConstructors), parentType, featureSet)
          mapConfig = MapConfig(PartialMap, BasicMap)

          caseMap = NewMapObject(
            cases,
            MapT(
              TypeTransform(constructorsSubtype, TypeT), 
              mapConfig
            )
          )

          newCaseMap <- UpdateCommandCalculator.updateVersionedObject(caseMap, command, env)

          newCaseName <- Evaluator.applyFunction(command, UIndex(0), env)
        } yield {
          ExpandKeyResponse(
            CaseT(newCaseMap.uObject, parentType, featureSet),
            Vector(UCase(newCaseName, UWildcard("_"))),
            untaggedIdentity
          )
        }
      }
      case SubtypeT(isMember, parentType, featureSet) => {
        val isMemberMap = NewMapObject(isMember, MapT(
          TypeTransform(parentType, BooleanT),
          MapConfig(CommandOutput, BasicMap)
        ))

        val adjustedCommand = UArray(command, UIndex(1))

        for {
          newMembersMap <- UpdateCommandCalculator.updateVersionedObject(isMemberMap, adjustedCommand, env)
        } yield {
          ExpandKeyResponse(
            SubtypeT(newMembersMap.uObject, parentType, featureSet),
            Vector(command),
            untaggedIdentity
          )
        }
      }
      case TypeClassT(typeTransform, implementation) => {
        command match {
          case UMap(mappings) => {
            val keys = mappings.map(_._1)

            for {
              implementationBindings <- implementation.getMapBindings()
            } yield {
              val newImplementation = mappings ++ implementationBindings.filter(x => !keys.contains(x._1))

              ExpandKeyResponse(
                TypeClassT(typeTransform, UMap(newImplementation)),
                keys,
                untaggedIdentity
              )
            }
          }
          case _ => {
            Failure(s"Wrong input for typeClassT -- ${nType.displayString(env)} -- $command")
          }
        }
      }
      case CustomT(name, _, _) => {
        // This only occurs if we have a custom type within a custom type - so this won't be called for a while.
        // strategy: get underlying type from the type system, turn it into a NewMapType, and then call this on it!
        for {
          currentUnderlyingTypeInfo <- env.typeSystem.currentUnderlyingType(name)
          response <- expandType(currentUnderlyingTypeInfo._2, command, env)
        } yield response
      }
      case _ => Failure(s"Unable to expand key: $nType -- with command $command")
    }
  }
}