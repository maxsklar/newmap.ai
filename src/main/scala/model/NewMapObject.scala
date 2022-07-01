package ai.newmap.model

import java.util.UUID

/*
 * The objects in the NewMap Language
 */
sealed abstract class NewMapObject {
  override def toString = PrintNewMapObject(this, Environment.Base)
  def displayString(env: Environment) = PrintNewMapObject(this, env)
}

case class TaggedObject(
  uObject: UntaggedObject,
  nType: NewMapType
) extends NewMapObject

// The versionNumber and uuid uniquely define this versioned object within any environment
// (of course different environments might have updated the object differently)

case class VersionedObjectKey(
  versionNumber: Long,
  uuid: UUID
)

// This always points to the latest version of a versioned object
case class VersionedObjectLink(
  key: VersionedObjectKey
) extends NewMapObject

object NewMapO {
  def Index(i: Long): NewMapObject = TaggedObject(UIndex(i), CountT)
  
  // This is a subtype of TypeT, basically a newmap object with a command structure
  // - It has an initial value
  // - It has a command type
  // - You can give it commands to change the value
  // - You can potentially have versions available.

  def commandT: NewMapType = SubtypeT(IsCommandFunc, TypeT, SimpleFunction)

  def identifier(s: String): NewMapObject = TaggedObject(UIdentifier(s), IdentifierT)

  def emptyStruct: NewMapType = StructT(Vector.empty, IndexT(UIndex(0)), RequireCompleteness, BasicMap)
}