package ai.newmap.model

sealed abstract class NewMapTypeInfo

// In this case, the object was given an explicit type in code
// Therefore, it is always considered to be of this type
case class ExplicitlyTyped(nType: NewMapType) extends NewMapTypeInfo

// Here, the type wasn't given
// Every time the object is used, a required conversion happens to whatever type is needed at the time
// There could be multiple required conversions, but they must be compatible.
case class ImplicitlyTyped(
  requiredConversions: Vector[NewMapType]
) extends NewMapTypeInfo

object NewMapTypeInfo {
  val init: NewMapTypeInfo = ImplicitlyTyped(Vector.empty)
}

case class NewMapObjectWithType(
  nObject: NewMapObject,
  nTypeInfo: NewMapTypeInfo,
) {
  override def toString = PrintNewMapObject.applyObjectWithType(this)
}

object NewMapObjectWithType {
  def untyped(nObject: NewMapObject): NewMapObjectWithType = {
    NewMapObjectWithType(nObject, NewMapTypeInfo.init)
  }

  def withTypeI(nObject: NewMapObject, nType: NewMapType): NewMapObjectWithType = {
    NewMapObjectWithType(nObject, ImplicitlyTyped(Vector(nType)))
  }

  def withTypeE(nObject: NewMapObject, nType: NewMapType): NewMapObjectWithType = {
    NewMapObjectWithType(nObject, ExplicitlyTyped(nType))
  }
}