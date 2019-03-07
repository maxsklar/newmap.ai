package ai.newmap.environment


import java.io.File
import scala.io.Source
import java.io.PrintWriter
import java.nio.file.{Paths, Files}

object envReader {


<<<<<<< HEAD
	def envLogIn(chanName: String, envName: String, envAccessCode: String): Boolean = {
		val fileName: String = "EnvScript/"+chanName+"_"+envName+"_Env.txt"
		println(fileName)
		if(!Files.exists(Paths.get(fileName))){return false}
=======
		val fileName: String = chanName+"_"+envName+"_Env.txt"
		//println(fileName)
		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, fileName)) {return false}
		val obj = amazonS3Client.getObject(BUCKET_NAME, fileName)
  		val reader = new BufferedReader(new InputStreamReader(obj.getObjectContent()))
  		var line = reader.readLine
		println(line.stripPrefix("AC: "))

		// check accesscode is correct or not
		if(!envAccessCode.equals(line.stripPrefix("AC: "))){return false}
>>>>>>> 9c5eba0... correct env switching logic and deploy it to heroku

		// write chanName, envName, envAccessCode into CACHE
		// TODO: the cache will be named in channel name and user name
		// overwrite each time
<<<<<<< HEAD
		val cacheFile = new PrintWriter(new File("EnvScript/Sys/CACHE.txt"))
=======
		val cacheFileName = chanName+"_"+userName+"_CACHE.txt"
		val c_file = new File(cacheFileName)
		val cacheFile = new PrintWriter(c_file)
>>>>>>> 9c5eba0... correct env switching logic and deploy it to heroku
		cacheFile.write(chanName+","+envName+","+envAccessCode)
		cacheFile.close()

		val linesArray = Source.fromFile(fileName).getLines.toArray
		println(linesArray(0).stripPrefix("AC: "))
		for(i <- 0 until linesArray.size){
			println(linesArray(i))
		}
		true
	}

<<<<<<< HEAD
=======
	def envRead(chanName:String, userName: String, msg: String):String = {

		var envInterp = new EnvironmentInterpreter()
		envInterp.setChanName(chanName)
		envInterp.setUserName(userName)
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		val cacheFileName = chanName+"_"+userName+"_CACHE.txt"
		//val cacheLinesArray = Source.fromFile(cacheFileName).getLines.toArray
		println("******"+cacheFileName+"******")
		val obj1 = amazonS3Client.getObject(BUCKET_NAME, cacheFileName)
  		val reader1 = new BufferedReader(new InputStreamReader(obj1.getObjectContent()))
  		var cache_line = reader1.readLine
		// there should be only one line in cache
		val cont: Array[String] = cache_line.split(",")
		val envName = cont(1)
		val envAccessCode = cont(2)

		val fileName: String = chanName+"_"+envName+"_Env.txt"
		val obj2 = amazonS3Client.getObject(BUCKET_NAME, fileName)
  		val reader2 = new BufferedReader(new InputStreamReader(obj2.getObjectContent()))
  		var line = reader2.readLine
  		line = reader2.readLine
		while (line!=null){
			println("***"+line+"***")
			envInterp(line)
			line = reader2.readLine
		}
		val response = envInterp(msg)
		response match {
        	case Success(s) => {
          		val file:File = new File(fileName)
				val fileWriter:FileWriter = new FileWriter(file, true)
				val bufferedWriter:BufferedWriter = new BufferedWriter(fileWriter);
				bufferedWriter.write(msg+"\n")
				bufferedWriter.close();
				amazonS3Client.putObject(BUCKET_NAME, fileName, file)
        	}
        	case Failure(s) =>{
>>>>>>> 9c5eba0... correct env switching logic and deploy it to heroku


}
