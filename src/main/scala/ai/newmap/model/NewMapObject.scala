package ai.newmap.model


/*
 * The objects in the NewMap Language
 */
case class NewMapObject(
  uObject: UntaggedObject,
  nType: NewMapType
) {
  override def toString = PrintNewMapObject(this, Environment.Base)
  def displayString(env: Environment) = PrintNewMapObject(this, env)
}

object NewMapO {
  def Index(i: Long): NewMapObject = NewMapObject(UIndex(i), CountT)
  
  // This is a subtype of TypeT, basically a newmap object with a command structure
  // - It has an initial value
  // - It has a command type
  // - You can give it commands to change the value
  // - You can potentially have versions available.

  def commandT: NewMapType = SubtypeT(IsCommandFunc, TypeT, SimpleFunction)

  def identifier(s: String): NewMapObject = NewMapObject(UIdentifier(s), IdentifierT)

  def emptyStruct: NewMapType = StructT(UMap(Vector.empty), IndexT(UIndex(0)), RequireCompleteness, BasicMap)

  def taggedObjectT: NewMapType = CaseT(
    UMap(Vector(UWildcardPattern("t") -> ParamId("t"))),
    fieldParentType = TypeT,
    featureSet = PatternMap,
  )
}