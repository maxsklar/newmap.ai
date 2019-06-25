package ai.newmap.logger

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

import ai.newmap.environment.envConstant

object adminLogger {

	val BUCKET_NAME = envConstant.BUCKET_NAME
	val AWS_ACCESS_KEY = envConstant.AWS_ACCESS_KEY
	val AWS_SECRET_KEY = envConstant.AWS_SECRET_KEY

	val S3_LogFileName_Prefix = "LOG/"

	def log(chanName: String, userName: String, cont: String) = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		// read prev log from aws
		val logFileName = chanName+"_"+userName+"_"+"log.txt"
		var prev = ""
		if(amazonS3Client.doesObjectExist(BUCKET_NAME, S3_LogFileName_Prefix+logFileName)){
			val logObj = amazonS3Client.getObject(BUCKET_NAME, S3_LogFileName_Prefix+logFileName)
  			val logReader = new BufferedReader(new InputStreamReader(logObj.getObjectContent()))
  			var logLine = logReader.readLine
  			while(logLine != null){
  				prev += logLine+"\n"
  				logLine = logReader.readLine
  			}
		}
		prev += cont

		// write from aws
		val logFile = new File(logFileName)
		val logFileWriter = new FileWriter(logFile, false)
		val logBufferedWriter = new BufferedWriter(logFileWriter)
		logBufferedWriter.write(prev)
		logBufferedWriter.close()

		amazonS3Client.putObject(BUCKET_NAME, S3_LogFileName_Prefix+logFileName, logFile)
	}

	def adminPrint(): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		// get log file names
		var res = ""
  		val logFileNames = amazonS3Client.listObjects(BUCKET_NAME, S3_LogFileName_Prefix).getObjectSummaries()
  		for(str <- logFileNames.toList){
  			val fileName = str.getKey()
  			val tmp = fileName.split("_")
  			val chanName = tmp(0).stripPrefix("LOG/")
  			val userName = tmp(1)
  			res += """***** """+"*Log of Channel: "+chanName+", User: "+userName+"*"+""" *****"""+"\n"
  			res += print(fileName)
  			println("****** Log of Channel: "+chanName+", User: "+userName+" ******\n")
  			println(print(fileName))
  		}
  		res
	}

	def print(logFileName: String): String = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		var res = ""
  		val logObj = amazonS3Client.getObject(BUCKET_NAME, logFileName)
  		val logReader = new BufferedReader(new InputStreamReader(logObj.getObjectContent()))
  		var logLine = logReader.readLine
  		while(logLine != null){
 			res += logLine+"\n"
  			logLine = logReader.readLine
  		}
  		res += "_______________________________________________________\n"
  		res
	}
}