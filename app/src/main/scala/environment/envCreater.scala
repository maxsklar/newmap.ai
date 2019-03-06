package ai.newmap.environment


import java.io.File
import java.io.PrintWriter
import java.nio.file.{Paths, Files}

object envCreater {


	def envCreate(chanName: String, envName: String, envAccessCode: String): Boolean = {
		val fileName: String = "EnvScript/"+chanName+"_"+envName+"_Env.txt"
		if(Files.exists(Paths.get(fileName))){return false}
		val envFile = new PrintWriter(new File(fileName))

		envFile.write("AC: "+envAccessCode+"\n")
		envFile.close()
		true
	}



}
