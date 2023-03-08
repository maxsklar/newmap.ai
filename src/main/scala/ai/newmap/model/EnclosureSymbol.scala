package ai.newmap.model

sealed abstract class EnclosureSymbol

case object Paren extends EnclosureSymbol
case object SquareBracket extends EnclosureSymbol
case object CurlyBrace extends EnclosureSymbol
