 package ai.newmap.model

/**
 * The expressions in the NewMapLanguage
 */
sealed abstract class NewMapExpression

case class ObjectExpression(
  uObject: UntaggedObject
) extends NewMapExpression

case class ApplyFunction(
  func: NewMapExpression,
  input: NewMapExpression
) extends NewMapExpression

case class BuildCase(
  constructor: UntaggedObject,
  input: NewMapExpression
) extends NewMapExpression

// Future: Replace with Buildcase when MapT is a Case Instance
case class BuildMapT(
  inputType: NewMapExpression,
  outputType: NewMapExpression,
  config: MapConfig
) extends NewMapExpression

case class BuildTableT(
  expandingKeyType: NewMapExpression,
  requiredValues: NewMapExpression
) extends NewMapExpression

case class BuildSubtypeT(
  isMember: NewMapExpression,
  parentType: NewMapExpression,
  featureSet: MapFeatureSet = BasicMap
) extends NewMapExpression

case class BuildCaseT(
  cases: NewMapExpression,
  parentFieldType: NewMapType,
  featureSet: MapFeatureSet
) extends NewMapExpression

case class BuildStructT(
  params: NewMapExpression,
  parentFieldType: NewMapType,
  completeness: MapCompleteness,
  featureSet: MapFeatureSet
) extends NewMapExpression
//case class BuildSubtypeT(isMember: NewMapExpression) extends NewMapExpression

case class BuildNewTypeClassT(
  params: NewMapExpression
) extends NewMapExpression

case class BuildMapInstance(
  values: Vector[(NewMapPattern, NewMapExpression)]
) extends NewMapExpression

// This is an object that stands for something else in the environment
// Very important so that we don't repeat code
// TODO - perhaps in the future, every NewMapObject will actually be a pointer to a hash table, and these will be irrelevant
case class ParamId(name: String) extends NewMapExpression
