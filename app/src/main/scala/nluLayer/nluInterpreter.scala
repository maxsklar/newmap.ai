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
import scala.collection.mutable.HashMap

import ai.newmap.environment.envConstant

object nluInterpreter {
	val BUCKET_NAME = envConstant.BUCKET_NAME
	val AWS_ACCESS_KEY = envConstant.AWS_ACCESS_KEY
	val AWS_SECRET_KEY = envConstant.AWS_SECRET_KEY

	val S3_ModelFileName_Prefix = "Model/"

	val ActionMap:HashMap[String, String] = HashMap.empty[String,String]
	val CreateIndMap:HashMap[String, String] = HashMap.empty[String, String]
	val CreateEnvIndMap:HashMap[String, String] = HashMap.empty[String,String]
	val CreateDsIndMap:HashMap[String, String] = HashMap.empty[String,String]

	def loadModel() = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		/* // get model file names
  		val modelFileNames = amazonS3Client.listObjects(BUCKET_NAME, S3_ModelFileName_Prefix).getObjectSummaries()
  		for(str <- modelFileNames.toList){
  			println("******"+str.getKey()+"******")
  		}
  		*/

  		val actionModFileName = "action_model.txt"
  		val actionModObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+actionModFileName)
  		val actionModReader = new BufferedReader(new InputStreamReader(actionModObj.getObjectContent()))
  		var actionModLine = actionModReader.readLine
  		while(actionModLine != null){
  			val cont = actionModLine.split(",")
  			ActionMap += (cont(0) -> cont(1))
  			actionModLine = actionModReader.readLine
  		}

  		val createIndFileName = "create_model.txt"
  		val createIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+createIndFileName)
  		val createIndReader = new BufferedReader(new InputStreamReader(createIndObj.getObjectContent()))
  		var createIndLine = createIndReader.readLine
  		while(createIndLine != null){
  			val cont = createIndLine.split(",")
  			CreateIndMap += (cont(0) -> cont(1))
  			createIndLine = createIndReader.readLine
  		}

  		val CreateEnvIndFileName = "create_env_model.txt"
  		val CreateEnvIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+CreateEnvIndFileName)
  		val CreateEnvIndReader = new BufferedReader(new InputStreamReader(CreateEnvIndObj.getObjectContent()))
  		var CreateEnvIndLine = CreateEnvIndReader.readLine
  		while(CreateEnvIndLine != null){
  			val cont = CreateEnvIndLine.split(",")
  			CreateEnvIndMap += (cont(0) -> cont(1))
  			CreateEnvIndLine = CreateEnvIndReader.readLine
  		}

  		val CreateDsIndFileName = "create_ds_model.txt"
  		val CreateDsIndObj = amazonS3Client.getObject(BUCKET_NAME, S3_ModelFileName_Prefix+CreateDsIndFileName)
  		val CreateDsIndReader = new BufferedReader(new InputStreamReader(CreateDsIndObj.getObjectContent()))
  		var CreateDsIndLine = CreateDsIndReader.readLine
  		while(CreateDsIndLine != null){
  			val cont = CreateDsIndLine.split(",")
  			CreateDsIndMap += (cont(0) -> cont(1))
  			CreateDsIndLine = CreateDsIndReader.readLine
  		}

  		println("*** action map ***")
  		ActionMap.forEach{case (key, value) => println (key + "-->" + value)}
  		println("*** create map ***")
  		CreateIndMap.forEach{case (key, value) => println (key + "-->" + value)}
  		println("*** create env map ***")
  		CreateEnvIndMap.forEach{case (key, value) => println (key + "-->" + value)}
  		println("*** create ds map ***")
  		CreateDsIndMap.forEach{case (key, value) => println (key + "-->" + value)}
 
	}
}
