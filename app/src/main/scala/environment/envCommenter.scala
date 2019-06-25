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

object envCommenter {
	
	val BUCKET_NAME = envConstant.BUCKET_NAME
	val AWS_ACCESS_KEY = envConstant.AWS_ACCESS_KEY
	val AWS_SECRET_KEY = envConstant.AWS_SECRET_KEY

	val S3_EnvFileName_Prefix = "EnvScript/"
	val S3_CacheFileName_Prefix = "CACHE/"

	def envComment(chanName: String, userName: String, envName: String, envAccessCode: String, comment: String):Int = {
		
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		val fileName: String = chanName+"_"+envName+"_Env.txt"
		// check env exist or not
		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_EnvFileName_Prefix+fileName)) {return 1}

		// check access code
		val obj = amazonS3Client.getObject(BUCKET_NAME, S3_EnvFileName_Prefix+fileName)
  		val reader = new BufferedReader(new InputStreamReader(obj.getObjectContent()))
  		var line = reader.readLine
		if(!envAccessCode.equals(line.stripPrefix("AC: "))){return 2}

		// read <chanName>_envs.txt file
		val envsFileName: String = chanName+"_Envs.txt"
		val envsObj = amazonS3Client.getObject(BUCKET_NAME, S3_EnvFileName_Prefix+envsFileName)
		val envsReader = new BufferedReader(new InputStreamReader(envsObj.getObjectContent()))

		// write comments back and upload to Cload storage
		val envsFile = new File(envsFileName)
		val fileWriter:FileWriter = new FileWriter(envsFile, false)
		val bufferedWriter:BufferedWriter = new BufferedWriter(fileWriter);

		var env_line = envsReader.readLine
		while(env_line != null){
			if(env_line.startsWith(envName)){
				bufferedWriter.write(env_line+"\t//"+comment+" by "+userName+" \n")
			}else{
				bufferedWriter.write(env_line+"\n")
			}
			env_line = envsReader.readLine
		}
		bufferedWriter.close();
		amazonS3Client.putObject(BUCKET_NAME, S3_EnvFileName_Prefix+envsFileName, envsFile)
		0
	}
}
