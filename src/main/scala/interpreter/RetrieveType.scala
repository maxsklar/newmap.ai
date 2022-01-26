package ai.newmap.interpreter

import ai.newmap.model._

object RetrieveType {
  def apply(nObject: NewMapObject): NewMapType = nObject match {
    case Index(_) => CountT
    case CountT => TypeT(0)
    case TypeT(i) => TypeT(i + 1)
    case IdentifierT => TypeT(0)
    case IdentifierInstance(s) => IdentifierT
    case RangeFunc(i) => MapT(CountT, NewMapO.rangeT(2), CommandOutput, BasicMap)
    case SubtypeT(isMember) => this(retrieveInputTypeFromFunction(isMember))
    case MapT(inputType, outputType, completeness, featureSet) => {
      // Does it matter if the depth of input or output type is greater?
      TypeT(0)
    }
    case MapInstance(values, mapT) => mapT
    case LambdaInstance(params, expression) => {
      val inputType = params match {
        case StructParams(params) => {
          val fieldType = {
            SubtypeT(
              MapInstance(
                params.map(x => IdentifierInstance(x._1) -> Index(1)),
                MapT(IdentifierT, NewMapO.rangeT(2), CommandOutput, BasicMap)
              )
            )
          }

          val structParams = MapInstance(
            params.map(x => IdentifierInstance(x._1) -> x._2),
            MapT(fieldType, TypeT(0), RequireCompleteness, BasicMap)
          )

          StructT(structParams)
        }
        case IdentifierParam(_, nType) => nType
      }

      val outputType = this(expression)

      MapT(inputType, outputType, RequireCompleteness, BasicMap)
    }
    case ApplyFunction(func, input) => getParentType(retrieveOutputTypeFromFunction(func))
    case AccessField(_, _) => {
      throw new Exception("Field access retrieve type must be implemented!!")
    }
    case ParameterObj(_, nType) => getParentType(nType)
    case ParameterFunc(_, inputT, outputT) => MapT(inputT, outputT, RequireCompleteness, SimpleFunction)
    case IsCommandFunc(i) => MapT(TypeT(i), NewMapO.rangeT(2), CommandOutput, SimpleFunction)
    case StructT(values) => {
      // Does it matter if the depth of input or output type is greater?
      TypeT(0)
    }
    case CaseT(cases) => {
      // Does it matter if the depth of input or output type is greater?
      TypeT(0)
    }
    case StructInstance(value, nType) => nType
    case CaseInstance(constructor, value, nType) => nType
    case SubstitutableT(s, nType) => getParentType(nType)
  }

  def retrieveInputTypeFromFunction(nFunction: NewMapFunction): NewMapSubtype = nFunction match {
    case MapInstance(values, MapT(inputType, outputType, completeness, _)) => {
      inputType
    }
    case LambdaInstance(params, expression) => {
      params match {
        case StructParams(params) => {
          val fieldType = {
            SubtypeT(
              MapInstance(
                params.map(x => IdentifierInstance(x._1) -> Index(1)),
                MapT(IdentifierT, NewMapO.rangeT(2), CommandOutput, BasicMap)
              )
            )
          }

          val structParams = MapInstance(
            params.map(x => IdentifierInstance(x._1) -> x._2),
            MapT(fieldType, TypeT(0), RequireCompleteness, BasicMap)
          )

          StructT(structParams)
        }
        case IdentifierParam(_, nType) => nType
      }
    }
    case ParameterFunc(s, inputType, outputType) => inputType
    case IsCommandFunc(i) => TypeT(i)
    case RangeFunc(i) => CountT
  }

  def retrieveOutputTypeFromFunction(nFunction: NewMapFunction): NewMapSubtype = nFunction match {
    case MapInstance(values, MapT(inputType, outputType, completeness, _)) => {
      outputType
    }
    case LambdaInstance(params, expression) => {
      // TODO: Work has to be done here to not break generics
      this(expression)
    }
    case ParameterFunc(s, inputType, outputType) => outputType
    case IsCommandFunc(i) => NewMapO.rangeT(2)
    case RangeFunc(i) => NewMapO.rangeT(2)
  }

  def getParentType(nType: NewMapSubtype): NewMapType = {
    nType match {
      case SubtypeT(isMember) => {
        getParentType(retrieveInputTypeFromFunction(isMember))
      }
    case t: NewMapType => t
    }
  }

  def isTermClosedLiteral(nObject: NewMapObject): Boolean = {
    // Check that there are no parameters or weird functions in here.
    // TODO: Implement
    true
  }
}