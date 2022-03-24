package ai.newmap.model

sealed abstract class StatementOrExpressionParse

sealed abstract class ParseTree

case class NaturalNumberParse(i: Long) extends ParseTree

case class IdentifierParse(
  s: String,
  force: Boolean = false // If this is true, the identifier is forced to be a raw identifier, and not a keyword or substitute
) extends ParseTree

case class BindingCommandItem(
  key: ParseTree,
  value: ParseTree
) extends ParseTree

case class CommandList(
  values: Vector[ParseTree]
) extends ParseTree

case class ApplyParse(
  function: ParseTree,
  input: ParseTree
) extends ParseTree

case class FieldAccessParse(
  struct: ParseTree,
  field: ParseTree
) extends ParseTree

case class LambdaParse(
  params: ParseTree,
  expression: ParseTree
) extends ParseTree

abstract class EnvStatementParse

case class FullStatementParse(
  prefix: StatementPrefix,
  identifier: IdentifierParse,
  typeExpression: ParseTree,
  expression: ParseTree
) extends EnvStatementParse

case class InferredTypeStatementParse(
  prefix: StatementPrefix,
  identifier: IdentifierParse,
  expression: ParseTree
) extends EnvStatementParse

case class ExpressionOnlyStatementParse(
  expression: ParseTree
) extends EnvStatementParse

case class NewVersionedStatementParse(
  identifier: IdentifierParse,
  expression: ParseTree // Must represent the versioned type
) extends EnvStatementParse

case class ForkedVersionedStatementParse(
  identifier: IdentifierParse,
  expression: IdentifierParse // Must represent a versioned object
) extends EnvStatementParse

case class ApplyCommandStatementParse(
  identifier: IdentifierParse,
  expression: ParseTree // Represents the command to apply
) extends EnvStatementParse

case class ApplyCommandsStatementParse(
  identifier: IdentifierParse,
  expression: ParseTree // Represents a list of commands to apply
) extends EnvStatementParse

sealed abstract class StatementPrefix
case object ValStatement extends StatementPrefix

sealed abstract class BinaryOpParse

case class CommaBinaryOpParse() extends BinaryOpParse
case class ColonBinaryOpParse() extends BinaryOpParse
case class ArrowBinaryOpParse() extends BinaryOpParse
case class PeriodBinaryOpParse() extends BinaryOpParse