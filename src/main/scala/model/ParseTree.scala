package ai.newmap.model

sealed abstract class StatementOrExpressionParse

sealed abstract class ParseTree

case class NaturalNumberParse(i: Long) extends ParseTree

case class IdentifierParse(
  s: String,
  force: Boolean = false // If this is true, the identifier is forced to be a raw identifier, and not a keyword or substitute
) extends ParseTree

case class Enclosure(
  symbol: EnclosureSymbol,
  values: Vector[(ParseTree, ParseTree)]
) extends ParseTree

// TODO: we should make this a list of expressions to apply in sequence
case class ApplyParse(
  func: ParseTree,
  input: ParseTree
) extends ParseTree

case class LambdaParse(
  params: Vector[(ParseTree, ParseTree)],
  expression: ParseTree
) extends ParseTree

case class StatementParse(
  prefix: StatementPrefix,
  identifier: IdentifierParse,
  typeExpression: ParseTree,
  expression: ParseTree
)

sealed abstract class StatementPrefix
case object ValStatement extends StatementPrefix