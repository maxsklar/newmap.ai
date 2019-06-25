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
import scala.collection.immutable.ListMap

import ai.newmap.environment.envConstant
import ai.newmap.nluLayer.argParser._
import ai.newmap.nluLayer.baseLayerInterpreter.interp
import ai.newmap.nluLayer.nluInterpreter.preProcess
import ai.newmap.nluLayer.nluInterpreter.OriginalMessage
import ai.newmap.nluLayer.nluInterpreter.generateRegularJsonRespond
import ai.newmap.logger.adminLogger
import ai.newmap.nluLayer.onBoardConstant.PrintConstant

object actionProcessor {
	val BUCKET_NAME = envConstant.BUCKET_NAME
	val AWS_ACCESS_KEY = envConstant.AWS_ACCESS_KEY
	val AWS_SECRET_KEY = envConstant.AWS_SECRET_KEY

	val S3_ModelFileName_Prefix = "Model/"
	val S3_CacheFileName_Prefix = "CACHE/"

	var CreateIndMap:ListMap[String, String] = ListMap.empty[String, String]
	var AccessEnvIndMap:ListMap[String, String] = ListMap.empty[String, String]
	var PrintIndMap:ListMap[String, String] = ListMap.empty[String, String]
	var HardResetIndMap:ListMap[String, String] = ListMap.empty[String, String]

	def processCreateAct(chanName: String, userName: String, msg: String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		loadCreateIndModel
		val cont = msg.toLowerCase().split("\\s+")
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
			adminLogger.log(chanName, userName, "*Didn't recognize action object in this message, please tell me what do u want to create, env or data structure*")
			return generateRegularJsonRespond(">> "+OriginalMessage+"""\n"""+"*Didn't recognize action object in this message, please tell me what do u want to create, env or data structure*")
		}else{
			if(actObjectType.equals("env")){
				return parseCreateEnvArg(chanName, userName, msg, nluCacheFileName)
			}else{
				return parseCreateDSArg(chanName, userName, msg, nluCacheFileName)
			}
		}
	}

	def processPrintAct(chanName: String, userName: String, msg: String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		loadPrintIndModel
		val cont = msg.toLowerCase().split("\\s+")
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
			// TODO
			adminLogger.log(chanName, userName, "*Couldn't identify what u want me to show u*\n"+PrintConstant)
			return generateRegularJsonRespond(">> "+OriginalMessage+"""\n"""+"""*Couldn't identify what u want me to show u*\n"""+PrintConstant)
		}else if(printType.equals("envs")) {
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			val cmd = ":envs"
			//return "*I understand you want to print the envs in this channel* \nInterpret finished.\n"+
			//	   "generate newmap script cmd: "+cmd
			val ret = interp(chanName, userName, cmd)
			nluInterpreter.retJsonFormatFlag =false
			adminLogger.log(chanName, userName, ret)
			return ">> "+OriginalMessage+"\n"+ret
		}else if(printType.equals("env")) {
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			val cmd = ":printEnv"
			//return "*I understand you want to print the content in this env* \nInterpret finished.\n"+
			//	   "generate newmap script cmd: "+cmd
			val ret = interp(chanName, userName, cmd)
			nluInterpreter.retJsonFormatFlag =false
			adminLogger.log(chanName, userName, ret)
			return ">> "+OriginalMessage+"\n"+ret
		}else if(printType.equals("log")) {
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			val cmd = ":printLog"
			//return "*I understand you want to print the log history in this env* \nInterpret finished.\n"+
			//	   "generate newmap script cmd: "+cmd
			val ret = interp(chanName, userName, cmd)
			nluInterpreter.retJsonFormatFlag = false
			adminLogger.log(chanName, userName, ret)
			return ">> "+OriginalMessage+"\n"+ret
		}else if(printType.equals("commit")){
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			return parseCheckOutArg(chanName, userName, msg, nluCacheFileName)
		}else{
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			adminLogger.log(chanName, userName, "*system logic error*")
			return generateRegularJsonRespond(">> "+OriginalMessage+"""\n"""+"*system logic error*")
		}
	}

	def processAccessEnvAct(chanName: String, userName: String, msg: String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		loadAccessEnvIndModel
		val cont = msg.toLowerCase().split("\\s+")
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
			adminLogger.log(chanName, userName, "*Couldn't identify log in env or log off *")
			return generateRegularJsonRespond(">> "+OriginalMessage+"""\n"""+"*Couldn't identify log in env or log off *")
		}else if(accessEnvType.equals("in")){
			return parseLogInArg(chanName, userName, msg, nluCacheFileName)
		}else{
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			val cmd = ":log off"
			//return "*I understand you want to log off an environment* \nInterpret finished"
			val ret = interp(chanName, userName, cmd)
			adminLogger.log(chanName, userName, ret)
			return generateRegularJsonRespond(">> "+OriginalMessage+"""\n"""+ret)
		}
	}

	def processResetAct(chanName: String, userName: String, msg: String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		loadHardResetIndMode
  		val cont = msg.toLowerCase().split("\\s+")
		var gotHardResetType = false
		for(tok <- cont if !gotHardResetType){
			if(HardResetIndMap.contains(tok)){
				gotHardResetType = true
			}
		}

		if(!gotHardResetType) {
			return parseResetArg(chanName, userName, msg, nluCacheFileName, false)
		}else{
			return parseResetArg(chanName, userName, msg, nluCacheFileName, true)
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

 	def loadHardResetIndMode() = {
 		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		val hardResetIndFileName = "hard_reset_model.txt"
  		val hardResetIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+hardResetIndFileName)
  		val hardResetIndReader = new BufferedReader(new InputStreamReader(hardResetIndObj.getObjectContent()))
  		var hardResetIndLine = hardResetIndReader.readLine
  		while(hardResetIndLine != null) {
  			val cont = hardResetIndLine.split(",")
  			HardResetIndMap += (cont(0) -> cont(1))
  			hardResetIndLine = hardResetIndReader.readLine
  		}

  		println("*** reset map ***")
  		HardResetIndMap.forEach{case (key, value) => println (key + "-->" + value)}
 	}

}
