package ai.newmap.model

/**
 * The patterns in the NewMapLanguage
 */
sealed abstract class NewMapPattern


case class ObjectPattern(
  uObject: UntaggedObject
) extends NewMapPattern

case class WildcardPattern(name: String) extends NewMapPattern

case class StructPattern(
  params: Vector[NewMapPattern]
) extends NewMapPattern

case class CasePattern(
  constructor: UntaggedObject,
  input: NewMapPattern
) extends NewMapPattern

/*case class MapTPattern(
  keyType: NewMapPattern,
  valueType: NewMapPattern,
  config: MapConfig
) extends NewMapPattern*/

// Type Patterns

// Match if I am dealing with an index type
//case class IndexTPattern(name: String) extends NewMapPattern