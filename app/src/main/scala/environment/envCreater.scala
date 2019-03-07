package ai.newmap.environment


import java.io.File
import java.io.PrintWriter
import java.nio.file.{Paths, Files}

object envCreater {
<<<<<<< HEAD
=======
	val BUCKET_NAME = envConstant.BUCKET_NAME
	val AWS_ACCESS_KEY = envConstant.AWS_ACCESS_KEY
	val AWS_SECRET_KEY = envConstant.AWS_SECRET_KEY
>>>>>>> 1a42f2f... solve AWS security issue

<<<<<<< HEAD
=======
	def envCreate(chanName: String, userName: String, envName: String, envAccessCode: String): Boolean = {
		
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)
>>>>>>> 3a04edc... remove redundent comment

<<<<<<< HEAD
	def envCreate(chanName: String, envName: String, envAccessCode: String): Boolean = {
		val fileName: String = "EnvScript/"+chanName+"_"+envName+"_Env.txt"
		if(Files.exists(Paths.get(fileName))){return false}
		val envFile = new PrintWriter(new File(fileName))

=======
		val fileName: String = chanName+"_"+envName+"_Env.txt"
		if(amazonS3Client.doesObjectExist(BUCKET_NAME, fileName)) {return false}

		// write chanName, envName, envAccessCode into CACHE
		// the cache will be named in channel name and user name
		// overwrite each time
		val cacheFileName = chanName+"_"+userName+"_CACHE.txt"
		val c_file = new File(cacheFileName)
		c_file.createNewFile();
		val cacheFile = new PrintWriter(c_file)
		cacheFile.write(chanName+","+envName+","+envAccessCode)
		cacheFile.close()
		amazonS3Client.putObject(BUCKET_NAME, cacheFileName, c_file)

		val e_file = new File(fileName)
		e_file.createNewFile();
		val envFile = new PrintWriter(e_file)
>>>>>>> 9c5eba0... correct env switching logic and deploy it to heroku
		envFile.write("AC: "+envAccessCode+"\n")
		envFile.close()
		true
	}



}
