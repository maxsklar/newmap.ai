package ai.newmap.model

/**
 * The patterns in the NewMapLanguage
 */
sealed abstract class NewMapPattern

case class ObjectPattern(
  uObject: UntaggedObject
) extends NewMapPattern

case class WildcardPattern(
  name: String
) extends NewMapPattern

case class StructPattern(
  params: Vector[NewMapPattern]
) extends NewMapPattern

case class CasePattern(
  constructor: UntaggedObject,
  input: NewMapPattern
) extends NewMapPattern


// This is a temporary pattern until types are stored as cases, in which case we'll use casePattern
case class MapTPattern(
  input: NewMapPattern,
  outputType: NewMapPattern,
  config: MapConfig
) extends NewMapPattern