package ai.newmap.environment


import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import scala.io.Source
import java.io.PrintWriter
import java.nio.file.{Paths, Files}

import ai.newmap.interpreter._
import ai.newmap.model._
import ai.newmap.interpreter.TypeChecker._
import ai.newmap.util.{Outcome, Success, Failure}

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.AmazonClientException
import com.amazonaws.AmazonServiceException
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileOutputStream
import org.apache.commons.io.IOUtils

object envReader {

	val BUCKET_NAME = envConstant.BUCKET_NAME
	val AWS_ACCESS_KEY = envConstant.AWS_ACCESS_KEY
	val AWS_SECRET_KEY = envConstant.AWS_SECRET_KEY

	val S3_EnvFileName_Prefix = "EnvScript/"
	val S3_CacheFileName_Prefix = "CACHE/"

	// return 1: environment not exsit
	// return 2: wrong password
	// return 0: loged in
	def envLogIn(chanName: String, userName: String, envName: String, envAccessCode: String): Int = {

		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		val fileName: String = chanName+"_"+envName+"_Env.txt"
		//println(fileName)
		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_EnvFileName_Prefix+fileName)) {return 1}
		val obj = amazonS3Client.getObject(BUCKET_NAME, S3_EnvFileName_Prefix+fileName)
  		val reader = new BufferedReader(new InputStreamReader(obj.getObjectContent()))
  		var line = reader.readLine
		println(line.stripPrefix("AC: "))

		// check accesscode is correct or not
		if(!envAccessCode.equals(line.stripPrefix("AC: "))){return 2}

		// write chanName, envName, envAccessCode into CACHE
		// TODO: the cache will be named in channel name and user name
		// overwrite each time
		val cacheFileName = chanName+"_"+userName+"_CACHE.txt"
		val c_file = new File(cacheFileName)
		val cacheFile = new PrintWriter(c_file)
		cacheFile.write(chanName+","+envName+","+envAccessCode)
		cacheFile.close()

		amazonS3Client.putObject(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName, c_file)
		0
	}

	def envRead(chanName:String, userName: String, msg: String):String = {
		var envInterp = new EnvironmentInterpreter()
		envInterp.setChanName(chanName)
		envInterp.setUserName(userName)
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		val cacheFileName = chanName+"_"+userName+"_CACHE.txt"
		//val cacheLinesArray = Source.fromFile(cacheFileName).getLines.toArray
		println("******"+cacheFileName+"******")
		val obj1 = amazonS3Client.getObject(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName)
  		val reader1 = new BufferedReader(new InputStreamReader(obj1.getObjectContent()))
  		var cache_line = reader1.readLine
		// there should be only one line in cache
		val cont: Array[String] = cache_line.split(",")
		val envName = cont(1)
		val envAccessCode = cont(2)

		val fileName: String = chanName+"_"+envName+"_Env.txt"

		val file:File = new File(fileName)
		val fileWriter:FileWriter = new FileWriter(file, false)
		val bufferedWriter:BufferedWriter = new BufferedWriter(fileWriter);

		val obj2 = amazonS3Client.getObject(BUCKET_NAME, S3_EnvFileName_Prefix+fileName)
  		val reader2 = new BufferedReader(new InputStreamReader(obj2.getObjectContent()))
  		var line = reader2.readLine
  		bufferedWriter.write(line+"\n")
  		line = reader2.readLine
		while (line!=null){
			println("***"+line+"***")
			envInterp(line)
			bufferedWriter.write(line+"\n")
			line = reader2.readLine
		}
		val response = envInterp(msg)
		response match {
        	case Success(s) => {
				bufferedWriter.write(msg+"\n")
        	}
        	case Failure(s) =>{
        	}
        }
        bufferedWriter.close();
		amazonS3Client.putObject(BUCKET_NAME, S3_EnvFileName_Prefix+fileName, file)
        ""+response
    }

}
