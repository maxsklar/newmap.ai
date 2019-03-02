package ai.newmap.scripting

import scala.io.Source
import util.control.Breaks._

import ai.newmap.interpreter.EnvironmentInterpreter
import ai.newmap.util.{Outcome, Success, Failure}

class Processor {
  def FileProcessor(filepath : String) = {
    val file = Source.fromFile(filepath)
    var output = ""
    var linenum = 1
    var breakflag = false
    val envInterp = new EnvironmentInterpreter
    breakable {
      for (line <- file.getLines()) {
        val result = envInterp(line)
        result match {
          case Failure(s) => {
            breakflag = true
            println(s"Error in line $linenum: $s")
            }
          case Success(s) => {
            linenum += 1
            output += result + "\n"
            }
          }
        if (breakflag) break
      }
      println(output)
    }
    file.close()
  }
}
