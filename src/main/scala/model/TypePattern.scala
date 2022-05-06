package ai.newmap.model

/**
 * The patterns in the NewMapLanguage
 */
sealed abstract class TypePattern


case class ConstantTypePattern(
  nType: NewMapType
) extends TypePattern

case class WildcardTypePattern(name: String) extends TypePattern