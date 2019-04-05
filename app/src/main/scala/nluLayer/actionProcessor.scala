package ai.newmap.nluLayer

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import scala.io.Source
import java.io.PrintWriter
import java.nio.file.{Paths, Files}

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.AmazonClientException
import com.amazonaws.AmazonServiceException
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.FileOutputStream
import org.apache.commons.io.IOUtils
import scala.collection.JavaConversions._
import scala.collection.mutable.ListMap

import ai.newmap.environment.envConstant
import ai.newmap.nluLayer.argParser._

object actionProcessor {
	val BUCKET_NAME = envConstant.BUCKET_NAME
	val AWS_ACCESS_KEY = envConstant.AWS_ACCESS_KEY
	val AWS_SECRET_KEY = envConstant.AWS_SECRET_KEY

	val S3_ModelFileName_Prefix = "Model/"
	val S3_CacheFileName_Prefix = "CACHE/"

	val CreateIndMap:ListMap[String, String] = ListMap.empty[String, String]
	val AccessEnvIndMap:ListMap[String, String] = ListMap.empty[String, String]
	val PrintIndMap:ListMap[String, String] = ListMap.empty[String, String]

	def processCreateAct(msg: String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		loadCreateIndModel
		val cont = msg.toLowerCase.split("\\s+")
		var actObjectType = ""
		var gotActObjectType = false
		for(tok <- cont if !gotActObjectType){
			if(CreateIndMap.contains(tok)){
				actObjectType = CreateIndMap(tok)
				//println("*** action object type: "+actObjectType+" ***")
				gotActObjectType = true
			}
		}
		if(!gotActObjectType){
			nluInterpreter.writeToCache("create ", nluCacheFileName)
			//println("*** no action object type in message, please tell me what do u want to create env or data structure ***")
			return "*Didn't recognize action object in message, please tell me what do u want to create, env or data structure*"
		}else{
			if(actObjectType.equals("env")){
				val ret = parseCreateEnvArg(msg, nluCacheFileName)
				return "*I understand you want to create a/an "+actObjectType+"*\n"+ret
			}else{
				// TODO: parse create data structure arguments
				return "*I understand you want to create a/an "+actObjectType+"*"+"\nInterprete finished. *"
			}
		}
	}

	def processPrintAct(msg: String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		loadPrintIndModel
		val cont = msg.toLowerCase.split("\\s+")
		var printType = ""
		var gotPrintType = false
		for(tok <- cont if !gotPrintType){
			if(PrintIndMap.contains(tok)){
				printType = PrintIndMap(tok)
				gotPrintType = true
			}
		}

		if(!gotPrintType){
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			return "*couldn't identify what u want to print"
		}else if(printType.equals("envs")) {
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			return "*I understand you want to print the envs in this channel* \nInterpret finished."
		}else if(printType.equals("env")) {
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			return "*I understand you want to print the content in this env* \nInterpret finished."
		}else if(printType.equals("log")) {
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			return "*I understand you want to print the log history in this env* \nInterpret finished."
		}else if(printType.equals("commit")){
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			val ret = parseCheckOutArg(msg, nluCacheFileName)
			return "*I understand you want to check the content of a previous commmit in this env* \n"+ret
		}else{
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			return "*system logic error*"
		}
	}

	def processAccessEnvAct(msg: String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		loadAccessEnvIndModel
		val cont = msg.toLowerCase.split("\\s+")
		var accessEnvType = ""
		var gotAccessEnvType = false
		for(tok <- cont if !gotAccessEnvType){
			if(AccessEnvIndMap.contains(tok)){
				accessEnvType = AccessEnvIndMap(tok)
				gotAccessEnvType = true
			}
		}

		if(!gotAccessEnvType){
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			return "* couldn't identify log in env or log off *"
		}else if(accessEnvType.equals("in")){
			val ret = parseLogInArg(msg, nluCacheFileName)
			return "*I understand you want to log in an environment*\n"+ret
		}else{
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			return "*I understand you want to log off an environment* \nInterpret finished"
		}
	}

	def loadCreateIndModel() = {
  		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		val createIndFileName = "create_model.txt"
  		val createIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+createIndFileName)
  		val createIndReader = new BufferedReader(new InputStreamReader(createIndObj.getObjectContent()))
  		var createIndLine = createIndReader.readLine
  		while(createIndLine != null){
  			val cont = createIndLine.split(",")
  			CreateIndMap += (cont(0) -> cont(1))
  			createIndLine = createIndReader.readLine
  		}

  		println("*** create map ***")
  		CreateIndMap.forEach{case (key, value) => println (key + "-->" + value)}
  	}

  	def loadAccessEnvIndModel() = {
 		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		val accessEnvIndFileName = "access_env_model.txt"
  		val accessEnvIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+accessEnvIndFileName)
  		val accessEnvIndReader = new BufferedReader(new InputStreamReader(accessEnvIndObj.getObjectContent()))
  		var accessEnvIndLine = accessEnvIndReader.readLine
  		while(accessEnvIndLine != null){
  			val cont = accessEnvIndLine.split(",")
  			AccessEnvIndMap += (cont(0) -> cont(1))
  			accessEnvIndLine = accessEnvIndReader.readLine
  		}

  		println("*** access env map ***")
  		AccessEnvIndMap.forEach{case (key, value) => println (key + "-->" + value)}

 	}

 	def loadPrintIndModel() = {
 		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		val printIndFileName = "print_model.txt"
  		val printIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+printIndFileName)
  		val printIndReader = new BufferedReader(new InputStreamReader(printIndObj.getObjectContent()))
  		var printIndLine = printIndReader.readLine
  		while(printIndLine != null){
  			val cont = printIndLine.split(",")
  			PrintIndMap += (cont(0) -> cont(1))
  			printIndLine = printIndReader.readLine
  		}

  		println("*** print map ***")
  		PrintIndMap.forEach{case (key, value) => println (key + "-->" + value)}
 	}

}
