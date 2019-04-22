package ai.newmap.environment


import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
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

object envCreater {

	val BUCKET_NAME = envConstant.BUCKET_NAME
	val AWS_ACCESS_KEY = envConstant.AWS_ACCESS_KEY
	val AWS_SECRET_KEY = envConstant.AWS_SECRET_KEY

	val S3_EnvFileName_Prefix = "EnvScript/"
	val S3_CacheFileName_Prefix = "CACHE/"

	def envCreate(chanName: String, userName: String, envName: String, envAccessCode: String): Boolean = {
		
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

  		// local env file name
		val fileName: String = chanName+"_"+envName+"_Env.txt"
		// check if that's exits in AWS S3
		if(amazonS3Client.doesObjectExist(BUCKET_NAME, S3_EnvFileName_Prefix+fileName)) {return false}

		// write envName into a chanName_envs.txt file
		val envsFileName:String = chanName+"_Envs.txt"
		val envsFile:File = new File(envsFileName)
		val fileWriter:FileWriter = new FileWriter(envsFile, false)
		val bufferedWriter:BufferedWriter = new BufferedWriter(fileWriter)

		if(amazonS3Client.doesObjectExist(BUCKET_NAME, S3_EnvFileName_Prefix+envsFileName)) {
			val env_obj = amazonS3Client.getObject(BUCKET_NAME, S3_EnvFileName_Prefix+envsFileName)
  			val env_reader = new BufferedReader(new InputStreamReader(env_obj.getObjectContent()))
  			var env_line = env_reader.readLine
  			while (env_line!=null) {
   				bufferedWriter.write(env_line+"\n")
    			env_line = env_reader.readLine
  			}
  		}
  		//update
  		val e_file = new File(newFileName)
		e_file.createNewFile();
		val envFile = new PrintWriter(e_file)

		envFile.write("AC: "+newAccessCode+"\n")
		line = reader.readLine
		while(line != null){
			envFile.write(line+"\n")
			line = reader.readLine