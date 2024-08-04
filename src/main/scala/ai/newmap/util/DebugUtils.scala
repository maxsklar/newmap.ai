package ai.newmap.util

object DebugUtils {
  def printStackTrace(): Unit = {
    val stackTrace = new Throwable().getStackTrace
    stackTrace.foreach { traceElement =>
      println(traceElement)
    }
  }
}