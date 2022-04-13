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


// Built in functions

// This takes as input a member of TypeT and returns true if it's a member
//  of the command typeclass (which means it has a default value and an update function)
// TODO: making this a basic class is temporary for now
case object IsCommandFunc extends UntaggedObject

// Temporary for now, this should be buildable as a map pattern in the future
case object IsSimpleFunction extends UntaggedObject

// A basic function to increment a count
case object IncrementFunc extends UntaggedObject