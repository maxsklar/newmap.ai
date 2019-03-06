package ai.newmap.environment


import java.io.File
import scala.io.Source
import java.io.PrintWriter
import java.nio.file.{Paths, Files}

object envReader {


	def envLogIn(chanName: String, envName: String, envAccessCode: String): Boolean = {
		val fileName: String = "EnvScript/"+chanName+"_"+envName+"_Env.txt"
		println(fileName)
		if(!Files.exists(Paths.get(fileName))){return false}

		// write chanName, envName, envAccessCode into CACHE
		// TODO: the cache will be named in channel name and user name
		// overwrite each time
		val cacheFile = new PrintWriter(new File("EnvScript/Sys/CACHE.txt"))
		cacheFile.write(chanName+","+envName+","+envAccessCode)
		cacheFile.close()

		val linesArray = Source.fromFile(fileName).getLines.toArray
		println(linesArray(0).stripPrefix("AC: "))
		for(i <- 0 until linesArray.size){
			println(linesArray(i))
		}
		true
	}



}
