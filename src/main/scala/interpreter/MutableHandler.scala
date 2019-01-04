package ai.newmap.interpreter

import ai.newmap.model._
import ai.newmap.interpreter.Evaluator._
import ai.newmap.util.{Outcome, Success, Failure}


object MutableHandler {
  def addCommand(
    mutable: MutableObject,
    mutableType: MutableT,
    command: NewMapObjectWithType,
    env: Environment
  ): Outcome[MutableObject, String] = {
    for {
      _ <- TypeChecker.isRawObjectConvertibleToType(command, mutableType.commandType, env)
      newStateAttempt <- Evaluator.applyFunctionAttempt(mutableType.updateFunction, mutable.currentState, env)

      newState = newStateAttempt match {
        case AbleToApplyFunction(nObject) => nObject
        case UnableToApplyDueToUnknownInput => ApplyFunction(mutableType.updateFunction, mutable.currentState)
      }
    } yield {
      MutableObject(mutable.commands :+ command.nObject, newState)
    }
  }

  def versionFromStart(
    mutable: MutableObject
  ): Long = mutable.commands.length.toLong

  def calculateStaticObject(
    commands: Vector[NewMapObject],
    mutableType: MutableT,
    env: Environment
  ): NewMapObject = {
    var state: NewMapObject = mutableType.init

    for {
      command <- commands
    } yield {
      val applyAttempt = Evaluator.applyFunctionAttempt(mutableType.updateFunction, state, env)
      applyAttempt match {
        case Success(AbleToApplyFunction(result)) => state = result
        case Success(UnableToApplyDueToUnknownInput) => state = ApplyFunction(mutableType.updateFunction, state)
        case Failure(reason) => () // TODO: this really needs to be a loud error (can we make it so this can't happen?)
      }
    }

    state
  }

  def revertToVersion(
    mutable: MutableObject,
    mutableType: MutableT,
    i: Long,
    env: Environment
  ): MutableObject = {
    val newCommandList = mutable.commands.take(i.toInt)
    val newState = calculateStaticObject(newCommandList, mutableType, env)

    MutableObject(newCommandList, newState)
  }
}