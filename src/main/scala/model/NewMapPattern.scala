package ai.newmap.model

/**
 * The patterns in the NewMapLanguage
 */
sealed abstract class NewMapPattern


case class ObjectPattern(
  nObject: NewMapObject
) extends NewMapPattern

case class TypePattern(
  name: String,
  nType: NewMapObject
) extends NewMapPattern

case class StructPattern(
  params: Vector[NewMapPattern]
) extends NewMapPattern

case class CasePattern(
  constructor: NewMapObject,
  input: NewMapPattern
) extends NewMapPattern