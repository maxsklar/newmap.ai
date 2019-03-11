package ai.newmap.environment

import java.io.File
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.AmazonClientException
import com.amazonaws.AmazonServiceException
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileOutputStream
import org.apache.commons.io.IOUtils 

object envPrinter {
	val BUCKET_NAME = envConstant.BUCKET_NAME
	val AWS_ACCESS_KEY = envConstant.AWS_ACCESS_KEY
	val AWS_SECRET_KEY = envConstant.AWS_SECRET_KEY

	val S3_EnvFileName_Prefix = "EnvScript/"
	val S3_CacheFileName_Prefix = "CACHE/"

	def envPrint(chanName: String, userName: String):String = {
		val cacheFileName = chanName+"_"+userName+"_CACHE.txt"

		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName)) {
  			return "Please log in first"
  		}

  		val cache_obj = amazonS3Client.getObject(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName)
  		val cache_reader = new BufferedReader(new InputStreamReader(cache_obj.getObjectContent()))
  		val cache_line = cache_reader.readLine
  		val cont: Array[String] = cache_line.split(",")
  		val envName = cont(1)

  		val fileName: String = chanName+"_"+envName+"_Env.txt"

  		val obj = amazonS3Client.getObject(BUCKET_NAME, S3_EnvFileName_Prefix+fileName)
  		val reader = new BufferedReader(new InputStreamReader(obj.getObjectContent()))
  		var str = "\nEnvironment "+envName+": \n"
  		var line = reader.readLine
  		line = reader.readLine
  		while (line!=null){
			println("***"+line+"***")
			if(line.startsWith("val")){
				str += line+"\n"
			}
			line = reader.readLine
		}
		str
	}

	def envsPrint(chanName: String):String = {
		val envsFileName:String = chanName+"_Envs.txt"

		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_EnvFileName_Prefix+envsFileName)) {
  			return "No environment exist in this channel"
  		}

  		val envsObj = amazonS3Client.getObject(BUCKET_NAME, S3_EnvFileName_Prefix+envsFileName)
  		val envsReader = new BufferedReader(new InputStreamReader(envsObj.getObjectContent()))
  		var line = envsReader.readLine
  		var str = "\n"
  		while(line!=null){
  			str += line+"\n"
  			line = envsReader.readLine
  		}
  		str
	}

}