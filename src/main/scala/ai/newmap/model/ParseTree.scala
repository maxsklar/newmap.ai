package ai.newmap.model

import ai.newmap.interpreter.Lexer.Identifier

sealed abstract class StatementOrExpressionParse

class ParseElement()
sealed abstract class ParseTree extends ParseElement

case class NaturalNumberParse(i: Long) extends ParseTree

case class IdentifierParse(
  s: String,
  force: Boolean = false // If this is true, the identifier is forced to be a raw identifier, and not a keyword or substitute
) extends ParseTree

case class KeyValueBinding(
  key: ParseTree,
  value: ParseTree
) extends ParseTree

sealed abstract class LiteralListType
case object ArrayType extends LiteralListType
case object MapType extends LiteralListType

case class LiteralListParse(
  values: Vector[ParseTree],
  llType: LiteralListType
) extends ParseTree

// TODO - this will be a list of commands enclosed by '{'
//case class LiteralCode()

case class ApplyParse(
  function: ParseTree,
  input: ParseTree
) extends ParseTree

case class ConstructCaseParse(
  first: ParseTree,
  second: ParseTree
) extends ParseTree

case class LambdaParse(
  params: ParseTree,
  expression: ParseTree
) extends ParseTree

case class CharacterParse(
  char: String
) extends ParseTree

case class StringParse(
  s: String
) extends ParseTree

case class EmptyParse() extends ParseTree

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
  identifier: Identifier,
  expression: Identifier // Must represent a versioned object
) extends EnvStatementParse

case class ApplyCommandStatementParse(
  identifier: IdentifierParse,
  expression: ParseTree // Represents the command to apply
) extends EnvStatementParse

case class ApplyCommandsStatementParse(
  identifier: IdentifierParse,
  expression: ParseTree // Represents a list of commands to apply
) extends EnvStatementParse

case class AddChannelParse(
  identifier: IdentifierParse,
  typeOfChannel: ParseTree
) extends EnvStatementParse

case class ConnectChannelParse(
  identifier: Identifier,
  obj: Identifier
) extends EnvStatementParse

case class DisconnectChannelParse(
  identifier: Identifier,
  obj: Identifier
) extends EnvStatementParse

case class WriteToChannelParse(
  identifier: IdentifierParse,
  expression: ParseTree
) extends EnvStatementParse

case class NewTypeStatementParse(
  identifier: IdentifierParse, // New Name of the type
  expression: ParseTree // Expression that evaluates to a type
) extends EnvStatementParse

case class NewParamTypeStatementParse(
  identifier: IdentifierParse, // New Name of the type
  parameters: ParseTree
) extends EnvStatementParse

case class NewTypeClassStatementParse(
  identifier: IdentifierParse,
  typeTransform: ParseTree
) extends EnvStatementParse

case class IterateIntoStatementParse(
  iterableObject: Identifier, // This should be a parseTree - but our parser doesn't allow this yet!
  destinationObject: Identifier
) extends EnvStatementParse

sealed abstract class StatementPrefix
case object ValStatement extends StatementPrefix
case object DefStatement extends StatementPrefix

sealed abstract class BinaryOpParse

case class CommaBinaryOpParse() extends BinaryOpParse
case class ColonBinaryOpParse() extends BinaryOpParse
case class ArrowBinaryOpParse() extends BinaryOpParse