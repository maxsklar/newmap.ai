package ai.newmap.environment

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
			while(envLine != null || comLine != null){
				//println(envLine+"***"+comLine)
				if(envLine != null && !envLine.equals(comLine)){
					same = false
					break
				}
				if(comLine != null && !comLine.equals(envLine)){
					same = false
					break
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

	// private
	def getNextNameableChar(r: scala.util.Random): Char = {
		var c = r.nextPrintableChar
		while(!c.isDigit && !c.isLetter){
			c = r.nextPrintableChar
		}
		c
	}
}

