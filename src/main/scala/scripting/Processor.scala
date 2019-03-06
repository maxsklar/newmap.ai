package ai.newmap.scripting

import scala.io.Source
import util.control.Breaks._
import java.io.{FileNotFoundException, IOException}

import ai.newmap.interpreter.EnvironmentInterpreter
import ai.newmap.util.{Outcome, Success, Failure}

class Processor {
  def FileProcessor(filepath : String) = {
    try {
      val file = Source.fromFile(filepath)
      var output = ""
      var linenum = 1
      var breakflag = false
      val envInterp = new EnvironmentInterpreter
      breakable {
        val it = file.getLines
        while (it.hasNext) {
          var line = it.next()
          while (line.length == 0) {
            linenum += 1
            if (it.hasNext) line = it.next()
            else {
              println(output)
              break
            } // EOF
          }
          // further improvement for comments
          // while(line.startsWith("#/*"))
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
    catch {
      case e: FileNotFoundException => println("File not found!")
      case e: IOException => e.printStackTrace
    }
  }
}
