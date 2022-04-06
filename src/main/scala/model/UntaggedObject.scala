package ai.newmap.model

import java.util.UUID

/*
 * The objects in the NewMap Language
 */
sealed abstract class UntaggedObject {
  override def toString = PrintNewMapObject.untagged(this)
}

// Todo - replace with "user defined type" in prelude
case class UIdentifier(s: String) extends UntaggedObject

case class UMap(values: Vector[(NewMapPattern, NewMapExpression)]) extends UntaggedObject

// This can't be a map/struct because here the type of the input depends on the constructor
case class UCase(constructor: UntaggedObject, input: UntaggedObject) extends UntaggedObject

case class UIndex(i: Long) extends UntaggedObject