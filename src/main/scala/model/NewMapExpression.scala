package ai.newmap.model

/**
 * The expressions in the NewMapLanguage
 */
sealed abstract class NewMapExpression

case class ObjectExpression(
  nObject: NewMapObject
) extends NewMapExpression

case class ApplyFunction(
  func: NewMapExpression,
  input: NewMapExpression
) extends NewMapExpression

case class AccessField(
  struct: NewMapExpression, // This must be an object that has fields (StructInstance, CaseT)
  input: NewMapObject // Note - input must be literal and free of parameters (might be able to do this with scala's type systen)
) extends NewMapExpression

case class BuildCase(
  constructor: NewMapObject,
  input: NewMapExpression,
  caseType: NewMapObject
) extends NewMapExpression

// Future: Replace with Buildcase when MapT is a Case Instance
case class BuildMapT(
  inputType: NewMapExpression,
  outputType: NewMapExpression,
  completeness: MapCompleteness,
  featureSet: MapFeatureSet
) extends NewMapExpression

case class BuildSeqT(
  underlyingType: NewMapExpression
) extends NewMapExpression

case class BuildTableT(
  expandingKeyType: NewMapExpression,
  requiredValues: NewMapExpression
) extends NewMapExpression

case class BuildCaseT(cases: NewMapExpression) extends NewMapExpression
case class BuildStructT(params: NewMapExpression) extends NewMapExpression
case class BuildSubtypeT(isMember: NewMapExpression) extends NewMapExpression

case class BuildMapInstance(
  values: Vector[(NewMapPattern, NewMapExpression)],
  mapType: NewMapObject
) extends NewMapExpression

case class BuildStructInstance(
  values: Vector[(NewMapPattern, NewMapExpression)],
  structType: NewMapObject
) extends NewMapExpression

case class BuildSeqInstance(
  values: Vector[NewMapExpression],
  sequenceT: NewMapObject
) extends NewMapExpression

case class BuildTableInstance(
  values: Vector[(NewMapPattern, NewMapExpression)],
  tableT: NewMapObject
) extends NewMapExpression


// This is an object that stands for something else in the environment
// Very important so that we don't repeat code
// TODO - perhaps in the future, every NewMapObject will actually be a pointer to a hash table, and these will be irrelevant
case class ParamId(name: String) extends NewMapExpression
