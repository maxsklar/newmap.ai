package ai.newmap.model

object ConvertNewMapTypeToObject {
  def apply(newMapType: NewMapType): NewMapObject = newMapType match {
    case IndexT(i: Long) => Index(i)
    case CountT => CountType
    case IdentifierT => IdentifierType
    case MapT(key, value, default) => MapType(this(key), this(value), default)
    case ReqMapT(key, value) => ReqMapType(this(key), this(value))
    case StructT(params: Vector[(String, NewMapType)]) => StructType(paramsToObject(params, Index(1)))
    case CaseT(params: Vector[(String, NewMapType)]) => CaseType(paramsToObject(params, Index(0)))
    case LambdaT(transformer) => LambdaType(transformer)
    case SubstitutableT(s: String) => ParameterObj(s)
    case TypeT => TypeType
    case Subtype(t: NewMapType) => SubtypeType(this(t))
    case SubtypeFromMapType(m) => SubtypeFromMap(m)
    case AppliedFunctionT(func, input) => ApplyFunction(func, input)
  }

  // TODO(max): These 2 methods should not be needed
  // Vector[(String, NewMapObject)] should not be stored, only type. We'll have to fix this somehow
  def paramsToObjectParams(params: Vector[(String, NewMapType)]): Vector[(String, NewMapObject)] = {
    params.map(param => (param._1 -> ConvertNewMapTypeToObject(param._2)))
  }

  def paramsToObject(
    params: Vector[(String, NewMapType)],
    default: NewMapObject
  ): MapInstance = {
    val paramsAsObjects: Vector[(NewMapObject, NewMapObject)] = for {
      (name, nmt) <- params
    } yield {
      IdentifierInstance(name) -> ConvertNewMapTypeToObject(nmt)
    }
    //// TODO
    MapInstance(paramsAsObjects, default)
  }
}