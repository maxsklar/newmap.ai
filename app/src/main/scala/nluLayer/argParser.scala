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
import ai.newmap.nluLayer.baseLayerInterpreter.interp
import ai.newmap.nluLayer.nluInterpreter.preProcess
import ai.newmap.nluLayer.nluChecker.generateJsonString
import ai.newmap.nluLayer.nluInterpreter.generateRegularJsonRespond
import ai.newmap.nluLayer.nluInterpreter.OriginalMessage

object argParser {
	val BUCKET_NAME = envConstant.BUCKET_NAME
	val AWS_ACCESS_KEY = envConstant.AWS_ACCESS_KEY
	val AWS_SECRET_KEY = envConstant.AWS_SECRET_KEY

	val S3_ModelFileName_Prefix = "Model/"
	val S3_CacheFileName_Prefix = "CACHE/"

	var CreateEnvIndMap:ListMap[String, String] = ListMap.empty[String,String]
	var CreateDsIndMap:ListMap[String, String] = ListMap.empty[String,String]

	var LogInIndMap:ListMap[String, String] = ListMap.empty[String, String]

	var CheckOutIndMap:ListMap[String, String] = ListMap.empty[String, String]

	var CommentEnvIndMap:ListMap[String, String] = ListMap.empty[String, String]

	var CommitIndMap:ListMap[String, String] = ListMap.empty[String, String]

	var ResetIndMap:ListMap[String, String] = ListMap.empty[String, String]

	var AppendIndMap:ListMap[String, String] = ListMap.empty[String, String]

	var CopyIndMap:ListMap[String, String] = ListMap.empty[String, String]

	def parseCommentEnvArg(chanName: String, userName: String, msg: String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		loadCommentEnvModel

		val cont = msg.toLowerCase().split("\\s+").toList
		val orig_cont = msg.split("\\s+").toList
		var arg1 = ""	// env name
		var gotArg1 = false
		var arg2 = ""	// password
		var gotArg2 = false
		var arg3 = ""   // comment String
		var gotArg3 = false
/*
		for(i <- 0 to cont.size-1 if (!gotArg1 || !gotArg2 || !gotArg3)){
			val tok = cont(i)
			if(CommentEnvIndMap.contains(tok)){
				val tmp = CommentEnvIndMap(tok)
				if(!gotArg1 && tmp.equals("1") && i < cont.size-1){
					arg1 = cont(i+1)
					gotArg1 = true
				}
				if(!gotArg2 && tmp.equals("2") && i < cont.size-1){
					arg2 = cont(i+1)
					gotArg2 = true
				}
				if(!gotArg3 && tmp.equals("3") && i < cont.size-1){
					var index: Int = i+1
					while(index < cont.size && cont(index) != "#"){
						arg3 += cont(index)+" "
						index += 1
					}
					gotArg3 = true
				}
			}
		}
*/		
		
		//println(CommentEnvIndMap)
		for((k,v) <- CommentEnvIndMap if(!gotArg1 || !gotArg2 || !gotArg3)) {
			if(cont.contains(k)){
				val index = cont.indexOf(k)
				if(!gotArg1 && v.equals("1") && index < cont.size-1) {
					arg1 = orig_cont(index+1)
					gotArg1 = true
				}
				if(!gotArg2 && v.equals("2") && index < cont.size-1) {
					arg2 = orig_cont(index+1)
					gotArg2 = true
				}
				if(!gotArg3 && v.equals("3") && index < cont.size-1) {
					var i: Int = index + 1
					while(i < cont.size && !cont(i).equals("$")){
						arg3 += orig_cont(i)+" "
						i += 1
					}
					gotArg3 = true
				}
			} 
		}
		if(!gotArg1){
			nluInterpreter.writeToCache("comment on env ", nluCacheFileName)
			return "*I understand u want to comment, but missing env name, Please tell me a env name*"
		}else if(!gotArg3){
			nluInterpreter.writeToCache("comment on  env "+arg1+" with ", nluCacheFileName)
			return "I understand u want to comment, but missing comment string, Please tell me what u want to comment*"
		}else if(!gotArg2){
			nluInterpreter.writeToCache("comment on env "+arg1+" with "+arg3+" $ password ", nluCacheFileName)
			return "*Please tell me the password for this env*"
		}else{
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			val cmd = ":comment on "+arg1+" "+arg2+" ("+arg3+")"
			
			//return "*I understand u want to comment on "+arg1+" with comment: "+arg3+". \nInterpret finished.\n"+
			//	   "generate newmap script cmd: "+cmd

			val ret = interp(chanName, userName, cmd)
			return ret
		}

	}

	def parseLogInArg(chanName: String, userName: String, msg:String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		loadLogInIndModel

		val cont = msg.toLowerCase().split("\\s+").toList
		val orig_cont = msg.split("\\s+").toList
		var arg1 = ""	// env name
		var gotArg1 = false
		var arg2 = "" 	// password
		var gotArg2 = false
/*
		for(i <- 0 to cont.size-1 if (!gotArg1 || !gotArg2)) {
			val tok = cont(i)
			if(LogInIndMap.contains(tok) && i < cont.size-1){
				val tmp = LogInIndMap(tok)
				if(!gotArg1 && tmp.equals("1") && i < cont.size-1){
					arg1 = cont(i+1)
					gotArg1 = true
				}
				if(!gotArg2 && tmp.equals("2") && i < cont.size-1){
					arg2 = cont(i+1)
					gotArg2 = true
				}
			}
		}
*/
		for((k,v) <- LogInIndMap if(!gotArg1 || !gotArg2)) {
			if(cont.contains(k)){
				val index = cont.indexOf(k)
				val tmp = LogInIndMap(k)
				if(!gotArg1 && tmp.equals("1") && index < cont.size-1) {
					arg1 = orig_cont(index+1)
					gotArg1 = true
				}
				if(!gotArg2 && tmp.equals("2") && index < cont.size-1) {
					arg2 = orig_cont(index+1)
					gotArg2 = true
				}
			} 
		}

		if(!gotArg1){
			nluInterpreter.writeToCache("access ", nluCacheFileName)
			return "*missing env name, Please tell me a env name*"
		}else if(!gotArg2){
			nluInterpreter.writeToCache("access "+arg1+" with password ", nluCacheFileName)
			return "*missing password, Please tell me the password for this env.*"
		}else{
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			val cmd = ":log in "+arg1+" "+arg2
			//return "*got env name: "+arg1+". got password "+arg2+". * \nInterpret finished.\n"+
			//	   "generate newmap script cmd: "+cmd

			val ret = interp(chanName, userName, cmd)
			return ret
		}	
	}


	def parseCreateEnvArg(chanName: String, userName: String, msg: String, nluCacheFileName: String): String = {	// needs two args

		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		loadCreateEnvIndModel

		val cont = msg.toLowerCase().split("\\s+").toList
		val orig_cont = msg.split("\\s+").toList
		var arg1 = ""	// env name
		var gotArg1 = false
		var arg2 = ""	// password
		var gotArg2 = false
/*
		for(i <- 0 to cont.size-1 if (!gotArg1 || !gotArg2)){
			val tok = cont(i)
			if(CreateEnvIndMap.contains(tok)){
				val tmp = CreateEnvIndMap(tok)
				if(!gotArg1 && tmp.equals("1") && i < cont.size-1){
					arg1 = cont(i+1)
					gotArg1 = true
				}
				if(!gotArg2 && tmp.equals("2") && i < cont.size-1){
					arg2 = cont(i+1)
					gotArg2 = true
				}
			}
		}
*/

		for((k,v) <- CreateEnvIndMap if(!gotArg1 || !gotArg2)) {
			if(cont.contains(k)){
				val index = cont.indexOf(k)
				val tmp = CreateEnvIndMap(k)
				if(!gotArg1 && tmp.equals("1") && index < cont.size-1) {
					arg1 = orig_cont(index+1)
					gotArg1 = true
				}
				if(!gotArg2 && tmp.equals("2") && index < cont.size-1) {
					arg2 = orig_cont(index+1)
					gotArg2 = true
				}
			} 
		}

		if(!gotArg1){
			nluInterpreter.writeToCache("create env called ", nluCacheFileName)
			return generateJsonString(">> "+OriginalMessage+"""\n*I understand u want to create an environment*""", "*missing env name. Please tell me a env name*")	
		}else if(!gotArg2){
			nluInterpreter.writeToCache("create env called " + arg1 + " password ", nluCacheFileName)
			return generateJsonString(">> "+OriginalMessage+"""\n*I understand u want to create an env named """+arg1+"*", "*missing password. Please tell me a password*")
		}else{
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			val cmd = ":create "+arg1+" "+arg2
			//ret+"\nInterpret finished. *\n"+"generate newmap script cmd: "+cmd
			val ret = interp(chanName, userName, cmd)
			return generateRegularJsonRespond(ret)
		}
	}

	def parseCheckOutArg(chanName: String, userName: String, msg: String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		loadCheckOutIndModel

		val cont = msg.toLowerCase().split("\\s+").toList
		val orig_cont = msg.split("\\s+").toList
		var arg = ""
		var gotArg = false

		for((k,v) <- CheckOutIndMap if !gotArg){
			if(cont.contains(k) && cont.indexOf(k) < cont.size-1){
				arg = orig_cont(cont.indexOf(k)+1)
				gotArg = true
			}
		}

		if(!gotArg){
			nluInterpreter.writeToCache("show me the content with commit num ", nluCacheFileName)
			return "*missing the commit number you want to check, Please tell me the commit number \n(you can type 'what's in the log' to check all commit numbers)*"
		}else{
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			val cmd = ":checkout "+arg
			//return "*Got commit number "+arg+".* \nInterpret finished.\n"+
			//	   "generate newmap script cmd: "+cmd
			val ret = interp(chanName, userName, cmd)
			return ret
		}

	}

	def parseCommitArg(chanName: String, userName: String, msg: String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		loadCommitIndModel

  		val cont = msg.toLowerCase().split("\\s+").toList
  		val orig_cont = msg.split("\\s+").toList
		var arg = ""
		var gotArg = false

		for((k,v) <- CommitIndMap if !gotArg){
			if(cont.contains(k) && cont.indexOf(k) < cont.size-1){
				var i = cont.indexOf(k)+1
				while(i < cont.size) {
					arg += orig_cont(i)+" "
					i += 1
				}
				gotArg = true
			}
		}

		if(!gotArg){
			nluInterpreter.writeToCache("commit ", nluCacheFileName)
			return "*I understand u want to commit, but missing the commit message, Please tell me the commit message *"
		}else{
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			val cmd = ":commit "+arg
			//return "*I understand u want to commit current env. \nGot commit message: "+arg+".* \nInterpret finished.\n"+
			//	   "generate newmap script cmd: "+cmd
			val ret = interp(chanName, userName, cmd)
			return ret
		}
	}

	def parseResetArg(chanName: String, userName: String, msg: String, nluCacheFileName: String, hardFlag: Boolean): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		loadResetIndModel

  		val cont = msg.toLowerCase().split("\\s+").toList
  		val orig_cont = msg.split("\\s+").toList
  		var arg1 = ""	// commit id
  		var gotArg1 = false
  		var arg2 = ""	// parseword
  		var gotArg2 = false

  		for((k,v) <- ResetIndMap if (!gotArg1 || !gotArg2)) {
			if(cont.contains(k) && cont.indexOf(k) < cont.size-1){
				if(v.equals("1") && !gotArg1) {
					arg1 = orig_cont(cont.indexOf(k)+1)
					gotArg1 = true
				}
				if(v.equals("2") && !gotArg2) {
					arg2 = orig_cont(cont.indexOf(k)+1)
					gotArg2 = true
				}
			}
		}

		if(!gotArg1) {
			if(hardFlag){
				nluInterpreter.writeToCache("hard reset to commit id ", nluCacheFileName)
			}else{
				nluInterpreter.writeToCache("reset to commit id ", nluCacheFileName)
			}
			return "but missing commit id, Please tell me the commit id u want to reset. *"
		}else if(!gotArg2) {
			if(hardFlag){
				nluInterpreter.writeToCache("hard reset to commit id "+arg1+" password ", nluCacheFileName)
			}else{
				nluInterpreter.writeToCache("reset to commit id "+arg1+" password ", nluCacheFileName)
			}
			return "\nplease tell me the password of this env. *"
		}else {
			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
			var cmd = ""
			if(hardFlag){
				cmd += ":hard reset "+arg1+" "+arg2
			}else{
				cmd += ":reset "+arg1+" "+arg2
			}
			//return "\nGot commit id: "+arg1+".* \nInterpret finished.\n"+
			//	   "generate newmap script cmd: "+cmd
			val ret = interp(chanName, userName, cmd)
			return ret
		}

	}

	def parseCreateDSArg(chanName: String, userName: String, msg: String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		loadCreateDsIndModel

  		val cont = msg.toLowerCase().split("\\s+").toList
  		val orig_cont = msg.split("\\s+").toList
  		var arg1 = ""	// ds name
  		var gotArg1 = false
  		var arg2 = ""	// variable
  		var gotArg2 = false
  		var arg3 = ""	// type
  		var gotArg3 = false
  		var varMap:ListMap[String, String] = ListMap.empty[String,String]

  		for((k,v) <- CreateDsIndMap if !gotArg1){
  			if(cont.contains(k) && cont.indexOf(k) < cont.size-1){
  				if(v.equals("1") && !gotArg1) {
					arg1 = cont(cont.indexOf(k)+1)
					gotArg1 = true
				}
  			}
  		}

  		for(i <- 0 to cont.size-1 ){
			val tok = cont(i)
			if(CreateDsIndMap.contains(tok)){
				val tmp = CreateDsIndMap(tok)
				if(!gotArg2 && tmp.equals("2") && i < cont.size-1){
					arg2 = orig_cont(i+1)
					gotArg2 = true
				}
				if(gotArg2 && tmp.equals("3") && i < cont.size-1){
					arg3 = orig_cont(i+1)
					gotArg3 = true
				}
				if(gotArg2 && gotArg3){
					varMap += (arg2 -> arg3)
					gotArg2 = false
					gotArg3 = false
				}
			}
		}
		//println(varMap)

  		if(!gotArg1) {
  			nluInterpreter.writeToCache("create data structure called ", nluCacheFileName)
  			return "missing data structure name, Please tell me the name of the data structure you want to create."
  		}else if(varMap.isEmpty) {
  			nluInterpreter.writeToCache("create data structure called "+arg1+" ", nluCacheFileName)
  			return "missing content in this data structure "+arg1+" ,\nPlease tell me the variable(s) and the type(s) in 'variable <variable name> type <type>' syntax."
  		}else{
  			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
  			var cmd = "val "+arg1+":Map Identifier Identifier String = ("
  			for((k,v) <- varMap) {
  				cmd += k+": "+v+", "
  			}
  			cmd = cmd.stripSuffix(", ")
  			cmd += ")"
  			//return "create var map: "+varMap.toString.stripPrefix("ListMap")+"\nInterpret finished.\n"+
  			//	   "generate newmap script cmd: "+cmd
  			val ret = interp(chanName, userName, cmd)
			return ret
  		}

	}

	def parseAppendArg(chanName: String, userName: String, msg: String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		loadAppendIndModel

  		val cont = msg.toLowerCase().split("\\s+").toList
  		val orig_cont = msg.split("\\s+").toList
  		var arg1 = ""	// ds name
  		var gotArg1 = false
  		var arg2 = ""	// variable 
  		var gotArg2 = false
  		var arg3 = ""	// type
  		var gotArg3 = false
  		var varMap:ListMap[String, String] = ListMap.empty[String,String]

  		for((k,v) <- AppendIndMap if !gotArg1){
  			if(cont.contains(k) && cont.indexOf(k) < cont.size-1){
  				if(v.equals("1") && !gotArg1) {
					arg1 = orig_cont(cont.indexOf(k)+1)
					gotArg1 = true
				}
  			}
  		}

  		for(i <- 0 to cont.size-1 ){
			val tok = cont(i)
			if(AppendIndMap.contains(tok)){
				val tmp = AppendIndMap(tok)
				if(!gotArg2 && tmp.equals("2") && i < cont.size-1){
					arg2 = orig_cont(i+1)
					gotArg2 = true
				}
				if(gotArg2 && tmp.equals("3") && i < cont.size-1){
					arg3 = orig_cont(i+1)
					gotArg3 = true
				}
				if(gotArg2 && gotArg3){
					varMap += (arg2 -> arg3)
					gotArg2 = false
					gotArg3 = false
				}
			}
		}

		val ret: String = "I understand u want to append on data structure "

		if(!gotArg1) {
  			nluInterpreter.writeToCache("append on data structure ", nluCacheFileName)
  			return ret+"\nbut missing data structure name, Please tell me the name of the data structure you want to create."
  		}else if(varMap.isEmpty) {
  			nluInterpreter.writeToCache("append on data structure "+arg1+" ", nluCacheFileName)
  			return ret+arg1+"\nbut missing content in this data structure "+arg1+" ,\nPlease tell me the variable(s) and the type(s) in 'variable <variable name> type <type>' syntax."
  		}else{
  			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
  			var cmd = "val "+arg1+" = appendMap Identifier Identifier String "+arg1+" ("
  			for((k,v) <- varMap) {
  				cmd += k+": "+v+", "
  			}
  			cmd = cmd.stripSuffix(", ")
  			cmd += ")"
  			//return ret+arg1+"\nappend var map: "+varMap.toString.stripPrefix("ListMap")+"\nInterpret finished.\n"+
  			//	   "generate newmap script cmd: "+cmd
  			val ret = interp(chanName, userName, cmd)
			return ret
  		}
	}

	def parseCopyArg(chanName: String, userName: String, msg: String, nluCacheFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		loadCopyIndModel

  		val cont = msg.toLowerCase().split("\\s+").toList
  		val orig_cont = msg.split("\\s+").toList
  		var arg1 = ""	// from channel name
  		var gotArg1 = false
  		var arg2 = ""	// from env name
  		var gotArg2 = false
  		var arg3 = ""	// from env password
  		var gotArg3 = false
  		var arg4 = ""	// new env name
  		var gotArg4 = false
  		var arg5 = ""	// new env password
  		var gotArg5 = false

  		for((k,v) <- CopyIndMap if (!gotArg1 || !gotArg2 || !gotArg3 || !gotArg4 || !gotArg5) ){
  			if(cont.contains(k) && cont.indexOf(k) < cont.size-1){
  				if(v.equals("1") && !gotArg1) {
					arg1 = orig_cont(cont.indexOf(k)+1)
					gotArg1 = true
				}
				if(v.equals("2") && !gotArg2) {
					arg2 = orig_cont(cont.indexOf(k)+1)
					gotArg2 = true
				}
				if(v.equals("3") && !gotArg3) {
					arg3 = orig_cont(cont.indexOf(k)+1)
					gotArg3 = true
				}
				if(v.equals("4") && !gotArg4) {
					arg4 = orig_cont(cont.indexOf(k)+1)
					gotArg4 = true
				}
				if(v.equals("5") && !gotArg5) {
					arg5 = orig_cont(cont.indexOf(k)+1)
					gotArg5 = true
				}
  			}
  		}

  		val tmp = "I understand you want to copy a environment.\n"
  		if(!gotArg1){
  			nluInterpreter.writeToCache("copy from channel ", nluCacheFileName)
  			return tmp+"But missing the channel name you want to copy from, Please tell me the channel name."
  		}else if(!gotArg2){
  			nluInterpreter.writeToCache("copy from channel "+arg1+" env ", nluCacheFileName)
  			return tmp+"But missing the environment name you want to copy from, Please tell me the environment name."
  		}else if(!gotArg3){
  			nluInterpreter.writeToCache("copy from channel "+arg1+" env "+arg2+" password ", nluCacheFileName)
  			return tmp+"But Please tell me the password of the environment "+arg2+" from channel "+arg1+"."
  		}else if(!gotArg4){
  			nluInterpreter.writeToCache("copy from channel "+arg1+" env "+arg2+" password "+arg3+" newenv ", nluCacheFileName)
  			return tmp+"Please tell me the name of the new environment that you want to store the coppied content."
  		}else if(!gotArg5){
  			nluInterpreter.writeToCache("copy from channel "+arg1+" env "+arg2+" password "+arg3+" newenv "+arg4+" newpassword ", nluCacheFileName)
  			return tmp+"Please tell me the password of the new environment that you want to store the coppied content."
  		}else{
  			amazonS3Client.deleteObject(BUCKET_NAME, S3_CacheFileName_Prefix+nluCacheFileName)
  			val cmd = ":copy "+arg1+" "+arg2+" "+arg3+" "+arg4+" "+arg5
  			//return "I understand you want to copy from "+arg1+" env "+arg2+" with password "+arg3+", and create a new env "+arg4+" with password "+arg5+"\nInterpret finished."+
  			//	   "generate newmap script cmd: "+cmd
  			val ret = interp(chanName, userName, cmd)
			return ret
  		}

	}

	def loadCreateEnvIndModel() = {
  		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		val CreateEnvIndFileName = "create_env_model.txt"
  		val CreateEnvIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+CreateEnvIndFileName)
  		val CreateEnvIndReader = new BufferedReader(new InputStreamReader(CreateEnvIndObj.getObjectContent()))
  		var CreateEnvIndLine = CreateEnvIndReader.readLine
  		while(CreateEnvIndLine != null){
  			val cont = CreateEnvIndLine.split(",")
  			CreateEnvIndMap += (cont(0) -> cont(1))
  			CreateEnvIndLine = CreateEnvIndReader.readLine
  		}

  		println("*** create env arg map ***")
  		CreateEnvIndMap.forEach{case (key, value) => println (key + "-->" + value)}

  	}

  	def loadCreateDsIndModel() = {
  		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		val CreateDsIndFileName = "create_ds_model.txt"
  		val CreateDsIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+CreateDsIndFileName)
  		val CreateDsIndReader = new BufferedReader(new InputStreamReader(CreateDsIndObj.getObjectContent()))
  		var CreateDsIndLine = CreateDsIndReader.readLine
  		while(CreateDsIndLine != null){
  			val cont = CreateDsIndLine.split(",")
  			CreateDsIndMap += (cont(0) -> cont(1))
  			CreateDsIndLine = CreateDsIndReader.readLine
  		}

  		println("*** create ds arg map ***")
  		CreateDsIndMap.forEach{case (key, value) => println (key + "-->" + value)}
  	}

  	def loadLogInIndModel() = {
 		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		val logInIndFileName = "log_in_model.txt"
  		val logInIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+logInIndFileName)
  		val logInIndReader = new BufferedReader(new InputStreamReader(logInIndObj.getObjectContent()))
  		var logInIndLine = logInIndReader.readLine
  		while(logInIndLine != null){
  			val cont = logInIndLine.split(",")
  			LogInIndMap += (cont(0) -> cont(1))
  			logInIndLine = logInIndReader.readLine
  		}

  		println("*** log in arg map ***")
  		LogInIndMap.forEach{case (key, value) => println (key + "-->" + value)}
 	}

 	def loadCommentEnvModel() = {
 		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		val commentEnvIndFileName = "comment_env_model.txt"
  		val commentEnvIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+commentEnvIndFileName)
  		val commentEnvIndReader = new BufferedReader(new InputStreamReader(commentEnvIndObj.getObjectContent()))
  		var commentEnvIndLine = commentEnvIndReader.readLine
  		while(commentEnvIndLine != null){
  			val cont = commentEnvIndLine.split(",")
  			CommentEnvIndMap += (cont(0) -> cont(1))
  			commentEnvIndLine = commentEnvIndReader.readLine
  		}

  		println("*** commentEnv arg map ***")
  		CommentEnvIndMap.forEach{case (key, value) => println (key + "-->" + value)}
 	}

 	def loadCheckOutIndModel() = {
 		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		val checkOutIndFileName = "check_out_model.txt"
  		val checkOutIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+checkOutIndFileName)
  		val checkOutIndReader = new BufferedReader(new InputStreamReader(checkOutIndObj.getObjectContent()))
  		var checkOutIndLine = checkOutIndReader.readLine
  		while(checkOutIndLine != null){
  			val cont = checkOutIndLine.split(",")
  			CheckOutIndMap += (cont(0) -> cont(1))
  			checkOutIndLine = checkOutIndReader.readLine
  		}

  		println("*** check out arg map ***")
  		CheckOutIndMap.forEach{case (key, value) => println (key + "-->" + value)}
 	}

 	def loadCommitIndModel() = {
 		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		val commitIndFileName = "commit_model.txt"
  		val commitIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+commitIndFileName)
  		val commitIndReader = new BufferedReader(new InputStreamReader(commitIndObj.getObjectContent()))
  		var commitIndLine = commitIndReader.readLine
  		while(commitIndLine != null){
  			val cont = commitIndLine.split(",")
  			CommitIndMap += (cont(0) -> cont(1))
  			commitIndLine = commitIndReader.readLine
  		}

  		println("*** commit arg map ***")
  		CommitIndMap.forEach{case (key, value) => println (key + "-->" + value)}
 	}

 	def loadResetIndModel() = {
 		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		val resetIndFileName = "reset_model.txt"
  		val resetIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+resetIndFileName)
  		val resetIndReader = new BufferedReader(new InputStreamReader(resetIndObj.getObjectContent()))
  		var resetIndLine = resetIndReader.readLine
  		while(resetIndLine != null) {
  			//println("**"+resetIndLine+"**")
  			val cont = resetIndLine.split(",")
  			ResetIndMap += (cont(0) -> cont(1))
  			resetIndLine = resetIndReader.readLine
  		}

  		println("*** reset arg map ***")
  		ResetIndMap.forEach{case (key, value) => println (key + "-->" + value)}
 	}

 	def loadAppendIndModel() = {
 		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		val appendIndFileName = "append_model.txt"
  		val appendIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+appendIndFileName)
  		val appendIndReader = new BufferedReader(new InputStreamReader(appendIndObj.getObjectContent()))
  		var appendIndLine = appendIndReader.readLine
  		while(appendIndLine != null) {
  			//println("**"+appendIndLine+"**")
  			val cont = appendIndLine.split(",")
  			AppendIndMap += (cont(0) -> cont(1))
  			appendIndLine = appendIndReader.readLine
  		}

  		println("*** append arg map ***")
  		AppendIndMap.forEach{case (key, value) => println (key + "-->" + value)} 
 	}

 	def loadCopyIndModel() = {
 		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		val copyIndFileName = "copy_model.txt"
  		val copyIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+copyIndFileName)
  		val copyIndReader = new BufferedReader(new InputStreamReader(copyIndObj.getObjectContent()))
  		var copyIndLine = copyIndReader.readLine
  		while(copyIndLine != null) {
  			//println("**"+copyIndLine+"**")
  			val cont = copyIndLine.split(",")
  			CopyIndMap += (cont(0) -> cont(1))
  			copyIndLine = copyIndReader.readLine
  		}

  		println("*** copy arg map ***")
  		CopyIndMap.forEach{case (key, value) => println (key + "-->" + value)}
 	}

}
