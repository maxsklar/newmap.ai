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

import scala.collection.mutable.HashSet
import util.control.Breaks._

object envCommiter {
	val BUCKET_NAME = envConstant.BUCKET_NAME
	val AWS_ACCESS_KEY = envConstant.AWS_ACCESS_KEY
	val AWS_SECRET_KEY = envConstant.AWS_SECRET_KEY

	val S3_EnvFileName_Prefix = "EnvScript/"
	val S3_CacheFileName_Prefix = "CACHE/"
	val S3_uuidPoolFileName_Prefix = "Pool/"
	val S3_versionFileName_prefix = "Version/"

	val uuidPoolFileName = "uuidPool.txt"

	val uuidDigit = 6

	// private
	// currently set to six digit
	def generateUUID():String = {

		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		val uuidPoolFile = new File(uuidPoolFileName)
		val fileWriter:FileWriter = new FileWriter(uuidPoolFileName, false)
		val bufferedWriter:BufferedWriter = new BufferedWriter(fileWriter);

		val uuidPool: HashSet[String] = HashSet()

		if(amazonS3Client.doesObjectExist(BUCKET_NAME, S3_uuidPoolFileName_Prefix + uuidPoolFileName)){
			val uuidPoolObj = amazonS3Client.getObject(BUCKET_NAME, S3_uuidPoolFileName_Prefix + uuidPoolFileName)
			val uuidPoolReader = new BufferedReader(new InputStreamReader(uuidPoolObj.getObjectContent()))
			var uuidPoolLine = uuidPoolReader.readLine
			while(uuidPoolLine != null){
				uuidPool += uuidPoolLine
				bufferedWriter.write(uuidPoolLine+"\n")
				uuidPoolLine = uuidPoolReader.readLine
			}
		}

		// generate new uuid
		val res = new StringBuilder
		val r = scala.util.Random
		for(c <- for (i <- 1 to uuidDigit) yield getNextNameableChar(r)) {
			res += c
		}

		while(uuidPool(res.toString)){
			res.clear
			for(c <- for (i <- 1 to uuidDigit) yield getNextNameableChar(r)) {
				res += c
			}
		}
		bufferedWriter.write(res.toString + "\n")
		bufferedWriter.close();
		amazonS3Client.putObject(BUCKET_NAME, S3_uuidPoolFileName_Prefix + uuidPoolFileName, uuidPoolFile)
		res.toString

	}

	// ret 1: env didn't log in
	// ret 2: env didn't making any progress
	def envCommit(chanName: String, userName: String, commitMessage: String): Int = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		// check log in or not
		val cacheFileName = chanName+"_"+userName+"_CACHE.txt"
		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName)){return 1}

		// get the environement file
			// read cache file, get the env name
		val cacheObj = amazonS3Client.getObject(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName)
		val cacheReader = new BufferedReader(new InputStreamReader(cacheObj.getObjectContent()))
		var cacheLine = cacheReader.readLine
		val cont: Array[String] = cacheLine.split(",")
		val envName = cont(1)
		val envFileName = chanName+"_"+envName+"_Env.txt"

		// check is making any progress
		val versionFileName = chanName+"_"+envName+"_Ver.txt"
			// check tree file exist or not
		if(amazonS3Client.doesObjectExist(BUCKET_NAME, S3_versionFileName_prefix+versionFileName)){
			// get uuid of HEAD of tree
			val verObj = amazonS3Client.getObject(BUCKET_NAME, S3_versionFileName_prefix+versionFileName)
			val verReader = new BufferedReader(new InputStreamReader(verObj.getObjectContent()))
			val verLine = verReader.readLine
			val verHead = verLine.stripPrefix("HEAD:")

			// get the file uuid.txt
			val comFileName = verHead+".txt"
			val comObj = amazonS3Client.getObject(BUCKET_NAME, S3_versionFileName_prefix+comFileName)
			val comReader = new BufferedReader(new InputStreamReader(comObj.getObjectContent()))
			var comLine = comReader.readLine

			// compare diff between _env.txt and uuid.txt
			var same:Boolean = true
			val envObj = amazonS3Client.getObject(BUCKET_NAME, S3_EnvFileName_Prefix+envFileName)
			val envReader = new BufferedReader(new InputStreamReader(envObj.getObjectContent()))
			var envLine = envReader.readLine
			envLine = envReader.readLine
			comLine = comReader.readLine
			while((envLine != null || comLine != null) && same){
				println(envLine+"***"+comLine)
				if(envLine != null && comLine == null){
					same = false
				}else if(comLine != null && envLine == null){
					same = false
				}else if(!comLine.equals(envLine)){
					same = false
				}
				envLine = envReader.readLine
				comLine = comReader.readLine
			}
			if(same){return 2}
		}

		// generate the uuid for this envCommit
		val uuid = generateUUID()

		// write commit file to local and upload to S3
		val commitFileName = uuid+".txt"
		val commitFile = new File(commitFileName)
		val commitFileWriter = new FileWriter(commitFile, false)
		val commitBufferedWriter = new BufferedWriter(commitFileWriter)

			// get current environment file
		val envObj = amazonS3Client.getObject(BUCKET_NAME, S3_EnvFileName_Prefix+envFileName)
		val envReader = new BufferedReader(new InputStreamReader(envObj.getObjectContent()))
		var envLine = envReader.readLine
		commitBufferedWriter.write(envName+":"+envLine.stripPrefix("AC: ")+"\n")
		envLine = envReader.readLine
		while(envLine != null){
			commitBufferedWriter.write(envLine+"\n")
			envLine = envReader.readLine
		}
		commitBufferedWriter.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_versionFileName_prefix+commitFileName, commitFile)

		// update the version file
		val versionFile = new File(versionFileName)
		val versionFileWriter = new FileWriter(versionFile, false)
		val versionBufferedWriter = new BufferedWriter(versionFileWriter)
		versionBufferedWriter.write("HEAD:"+uuid+"\n")
			//get current version file
		if(amazonS3Client.doesObjectExist(BUCKET_NAME, S3_versionFileName_prefix+versionFileName)){
			val versionObj = amazonS3Client.getObject(BUCKET_NAME, S3_versionFileName_prefix+versionFileName)
			val versionReader = new BufferedReader(new InputStreamReader(versionObj.getObjectContent()))
			var versionLine = versionReader.readLine
			versionLine = versionReader.readLine
			while(versionLine != null){
				versionBufferedWriter.write(versionLine+"\n")
				versionLine = versionReader.readLine
			}
		}

		versionBufferedWriter.write(uuid+"\t//"+commitMessage+" // @author "+userName+"\n")
		versionBufferedWriter.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_versionFileName_prefix+versionFileName, versionFile)
		0
	}

	// function to print commit log
	def printLog(chanName: String, userName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		// check if logged in or not
		val cacheFileName = chanName+"_"+userName+"_CACHE.txt"
		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName)){
			return "*Please log in first*"
		}

		// get the envName
		val cacheObj = amazonS3Client.getObject(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName)
		val cacheReader = new BufferedReader(new InputStreamReader(cacheObj.getObjectContent()))
		var cacheLine = cacheReader.readLine
		val cont: Array[String] = cacheLine.split(",")
		val envName = cont(1)

		// read Version/chanN_envN_ver.txt
		val versionFileName = chanName+"_"+envName+"_Ver.txt"
			// if not exist 
		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_versionFileName_prefix+versionFileName)){
			return "*No log for this envrionment, please commit first*"
		}

		val res = new StringBuilder
		val verObj = amazonS3Client.getObject(BUCKET_NAME, S3_versionFileName_prefix+versionFileName)
		val verReader = new BufferedReader(new InputStreamReader(verObj.getObjectContent()))
		var verLine = verReader.readLine
		while(verLine != null){
			res ++= verLine+"\n"
			verLine = verReader.readLine
		}
		res.toString
	}

	// checkout and print the env of given uuid
	def checkout(chanName: String, userName: String, uuid: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		// check if logged in or not
  		val cacheFileName = chanName+"_"+userName+"_CACHE.txt"
		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName)){
			return "*Please log in first*"
		}

		// get the envName
		val cacheObj = amazonS3Client.getObject(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName)
		val cacheReader = new BufferedReader(new InputStreamReader(cacheObj.getObjectContent()))
		var cacheLine = cacheReader.readLine
		val cont: Array[String] = cacheLine.split(",")
		val envName = cont(1)

		// check uuid is valid or not
		val versionFileName = uuid+".txt"
		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_versionFileName_prefix+versionFileName)){
			return "*Invalid uuid, please use :printLog to check valid uuid*"
		}

		// check is this version file belongs to this envrionment
		val verObj = amazonS3Client.getObject(BUCKET_NAME, S3_versionFileName_prefix+versionFileName)
		val verReader = new BufferedReader(new InputStreamReader(verObj.getObjectContent()))
		val thisEnvName = verReader.readLine.split(":")(0)
		if(!thisEnvName.equals(envName)){
			return "*Wrong environment, please use :PrintLog to check valid uuid*"
		}

		// read version file
		var envInterp = new EnvironmentInterpreter()
		envInterp.setChanName(chanName)
		envInterp.setUserName(userName)
		val envSet: HashSet[String] = HashSet()
		val str = new StringBuilder
  		str ++= "*Environment "+envName+", commit id "+uuid+":* \n"
  		var verLine = verReader.readLine
  		if(verLine == null){return "*There is no content in this commit of "+envName+"* \n"}
  		while (verLine!=null){
			//println("***"+verLine+"***")
			envInterp(verLine)
			if(verLine.startsWith("val")){
				//str ++= verLine+"\n"
				envSet += envPrinter.parseVal(verLine)
			}
			verLine = verReader.readLine
		}
		for(element:String <- envSet){
			//println("******"+element+"******")
			val response = envInterp(element)
			str ++= "\t"+element+" = "+envPrinter.prettyPrinter(""+response) + "\n"
		}
		str.toString
	}

	// reset functionality, which won't delete commit file, will add a new commit file
	def reset(chanName: String, userName: String, uuid: String, envAccessCode: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		// check if logged in or not
		val cacheFileName = chanName+"_"+userName+"_CACHE.txt"
		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName)){
			return "*Please log in first*"
		}

		// get env name
		val cacheObj = amazonS3Client.getObject(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName)
		val cacheReader = new BufferedReader(new InputStreamReader(cacheObj.getObjectContent()))
		var cacheLine = cacheReader.readLine
		val cont: Array[String] = cacheLine.split(",")
		val envName = cont(1)

		// get uuid.txt file
			// check env name exist or not
		val commitFileName = uuid+".txt"
		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_versionFileName_prefix+commitFileName)){
			return "*Could not reset, Invalid uuid, please use :printLog to check valid uuid*"
		}
			// check if belongs to this env name or not
		val comObj = amazonS3Client.getObject(BUCKET_NAME, S3_versionFileName_prefix+commitFileName)
		val comReader = new BufferedReader(new InputStreamReader(comObj.getObjectContent()))
		var comLine = comReader.readLine
		val thisEnvName = comLine.split(":")(0)
		if(!thisEnvName.equals(envName)){
			return "*Could not reset, Wrong environment, please use :PrintLog to check valid uuid*"
		}

			// check access code 
		val thisAcessCode = comLine.split(":")(1)
		if(!thisAcessCode.equals(envAccessCode)){
			return "*Could not reset, Wrong password for environment "+envName+"*"
		}

		// write uuid.txt content to _Env file and new uuid .txt
			// get new uuid
		val new_uuid = generateUUID()
		val newComFileName = new_uuid+".txt"
		val newComFile = new File(newComFileName)
		val newComFileWriter = new FileWriter(newComFile, false)
		val newComBufferedWriter = new BufferedWriter(newComFileWriter)

		val envFileName = chanName+"_"+envName+"_Env.txt"
		val envFile = new File(envFileName)
		val envFileWriter:FileWriter = new FileWriter(envFile, false)
		val envBufferedWriter:BufferedWriter = new BufferedWriter(envFileWriter)
		envBufferedWriter.write("AC: "+envAccessCode+"\n")
		newComBufferedWriter.write(envName+":"+envAccessCode+"\n")
		comLine = comReader.readLine
		while(comLine != null){
			envBufferedWriter.write(comLine+"\n")
			newComBufferedWriter.write(comLine+"\n")
			comLine = comReader.readLine
		}

		// update version file
		val versionFileName = chanName+"_"+envName+"_Ver.txt"
		val versionObj = amazonS3Client.getObject(BUCKET_NAME, S3_versionFileName_prefix+versionFileName)
		val versionReader = new BufferedReader(new InputStreamReader(versionObj.getObjectContent()))
		var versionLine = versionReader.readLine

		val versionFile = new File(versionFileName)
		val versionFileWriter = new FileWriter(versionFile, false)
		val versionBufferedWriter = new BufferedWriter(versionFileWriter)
			// change HEAD 
		versionBufferedWriter.write("HEAD:"+new_uuid+"\n")
		versionLine = versionReader.readLine
			// remove aftwards commit file
		var found = false
		while(versionLine != null){
			versionBufferedWriter.write(versionLine+"\n")
			if(versionLine.startsWith(uuid)){
				found = true
			}
			versionLine = versionReader.readLine
		}
		versionBufferedWriter.write(new_uuid+"\t// reset to "+uuid+" // @author "+userName+"\n")
		if(!found){
			return "*System Logic fault*" //which shouldn't exist	
		}

		versionBufferedWriter.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_versionFileName_prefix+versionFileName, versionFile)

		envBufferedWriter.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_EnvFileName_Prefix+envFileName, envFile)
		newComBufferedWriter.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_versionFileName_prefix+newComFileName, newComFile)
		return "*Reset success*"
	}

	// reset HARD functionality
	def resetHard(chanName: String, userName: String, uuid: String, envAccessCode: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		// check if logged in or not
		val cacheFileName = chanName+"_"+userName+"_CACHE.txt"
		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName)){
			return "*Please log in first*"
		}

		// get env name
		val cacheObj = amazonS3Client.getObject(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName)
		val cacheReader = new BufferedReader(new InputStreamReader(cacheObj.getObjectContent()))
		var cacheLine = cacheReader.readLine
		val cont: Array[String] = cacheLine.split(",")
		val envName = cont(1)

		// get uuid.txt file
			// check env name exist or not
		val commitFileName = uuid+".txt"
		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_versionFileName_prefix+commitFileName)){
			return "*Could not reset, Invalid uuid, please use :printLog to check valid uuid*"
		}
			// check if belongs to this env name or not
		val comObj = amazonS3Client.getObject(BUCKET_NAME, S3_versionFileName_prefix+commitFileName)
		val comReader = new BufferedReader(new InputStreamReader(comObj.getObjectContent()))
		var comLine = comReader.readLine
		val thisEnvName = comLine.split(":")(0)
		if(!thisEnvName.equals(envName)){
			return "*Could not reset, Wrong environment, please use :PrintLog to check valid uuid*"
		}

			// check access code 
		val thisAcessCode = comLine.split(":")(1)
		if(!thisAcessCode.equals(envAccessCode)){
			return "*Could not reset, Wrong password for environment "+envName+"*"
		}

		// write uuid.txt content to _Env file
		val envFileName = chanName+"_"+envName+"_Env.txt"
		val envFile = new File(envFileName)
		val envFileWriter:FileWriter = new FileWriter(envFile, false)
		val envBufferedWriter:BufferedWriter = new BufferedWriter(envFileWriter)
		envBufferedWriter.write("AC: "+envAccessCode+"\n")
		comLine = comReader.readLine
		while(comLine != null){
			envBufferedWriter.write(comLine+"\n")
			comLine = comReader.readLine
		}

		// update version file
		val versionFileName = chanName+"_"+envName+"_Ver.txt"
		val versionObj = amazonS3Client.getObject(BUCKET_NAME, S3_versionFileName_prefix+versionFileName)
		val versionReader = new BufferedReader(new InputStreamReader(versionObj.getObjectContent()))
		var versionLine = versionReader.readLine

		val versionFile = new File(versionFileName)
		val versionFileWriter = new FileWriter(versionFile, false)
		val versionBufferedWriter = new BufferedWriter(versionFileWriter)
			// change HEAD 
		versionBufferedWriter.write("HEAD:"+uuid+"\n")
		versionLine = versionReader.readLine
			// remove aftwards commit file
		var found = false
		while(versionLine != null && !found){
			versionBufferedWriter.write(versionLine+"\n")
			if(versionLine.startsWith(uuid)){
				found = true
			}
			versionLine = versionReader.readLine
		}
		while(versionLine != null){
			// TODO: remove useless uuid from Pool
			val dep_uuid = versionLine.split("\\s+")(0)
			val depComFileName = dep_uuid+".txt"
			amazonS3Client.deleteObject(BUCKET_NAME, S3_versionFileName_prefix+depComFileName)
			versionLine = versionReader.readLine
		}
		if(!found){
			return "*Could not reset, Invalid uuid, please use :printLog to check valid uuid*"	
		}

		versionBufferedWriter.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_versionFileName_prefix+versionFileName, versionFile)

		envBufferedWriter.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_EnvFileName_Prefix+envFileName, envFile)
		return "*Hard reset success*"
	}

	// private
	def getNextNameableChar(r: scala.util.Random): Char = {
		var c = r.nextPrintableChar
		while(!c.isDigit && !c.isLetter){
			c = r.nextPrintableChar
		}
		c
	}
}

