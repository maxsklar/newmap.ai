package ai.newmap.scripting

import scala.io.Source
import util.control.Breaks._
import java.io.{FileNotFoundException, IOException}
import java.net.{URL, MalformedURLException}

import ai.newmap.interpreter.EnvironmentInterpreter
import ai.newmap.util.{Outcome, Success, Failure}

class Processor {
  def FileProcessor(filepath : String) : String = {
    var response = ""
    try {
      val file = Source.fromFile(filepath)
      var output = ""
      var linenum = 1
      var breakflag = false
      var isCommentBlock = false
      val envInterp = new EnvironmentInterpreter
      breakable {
        val it = file.getLines
        while (it.hasNext) {
          var line = it.next()
          // empty line
          // block comment with /* and */
          while (line.length == 0 || line.startsWith("//") || line.startsWith("/*")) {
            if (line.startsWith("/*")) isCommentBlock = true
            if (isCommentBlock) {
              while(!line.endsWith("*/")) {
                line = it.next()
                linenum += 1
              }
              isCommentBlock = false
              linenum += 1
            }
            else {
              linenum += 1
            }
            // EOF
            if (it.hasNext) line = it.next()
            else {
              response  = output
              break
            }
          }
          // comment in the line
          if (line.contains("//")) {
            line = line.substring(0, line.indexOf("//"))
          }
          val result = envInterp(line)
          result match {
            case Failure(s) => {
              breakflag = true
              response = s"Error in line $linenum ($line): $s"
            }
            case Success(s) => {
              linenum += 1
              output += result + "\n"
            }
          }
          if (breakflag) break
        }
        response = output
      }
      file.close()
    }
    catch {
      case e: FileNotFoundException => {
        response = "File not found!"
      }
      case e: IOException => {
        response = "I/O exception!"
        e.printStackTrace
      }
    }
    response
  }

  def textBlockProcessor(textString : String) : String = {
    var response = ""
    var output = ""
    var linenum = 1
    var breakflag = false
    var isCommentBlock = false
    val it = textString.split("\n").toIterator
    val envInterp = new EnvironmentInterpreter
    breakable {
      while (it.hasNext) {
        var line = it.next()
        // empty line
        // block comment with /* and */
        while (line.length == 0 || line.startsWith("//") || line.startsWith("/*")) {
          if (line.startsWith("/*")) isCommentBlock = true
          if (isCommentBlock) {
            while(!line.endsWith("*/")) {
              line = it.next()
              linenum += 1
            }
            isCommentBlock = false
            linenum += 1
          }
          else {
            linenum += 1
          }
          // EOF
          if (it.hasNext) line = it.next()
          else {
            response  = output
            break
          }
        }
        // comment in the line
        if (line.contains("//")) {
          line = line.substring(0, line.indexOf("//"))
        }
        val result = envInterp(line)
        result match {
          case Failure(s) => {
            breakflag = true
            response = s"Error in line ($line): $s"
          }
          case Success(s) => {
            linenum += 1
            output += result + "\n"
          }
        }
        if (breakflag) break
      }
      response = output
    }
    response
  }
}