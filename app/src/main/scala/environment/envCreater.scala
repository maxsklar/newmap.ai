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
		val bufferedWriter:BufferedWriter = new BufferedWriter(fileWriter);

		if(amazonS3Client.doesObjectExist(BUCKET_NAME, S3_EnvFileName_Prefix+envsFileName)) {
			val env_obj = amazonS3Client.getObject(BUCKET_NAME, S3_EnvFileName_Prefix+envsFileName)
  			val env_reader = new BufferedReader(new InputStreamReader(env_obj.getObjectContent()))
  			var env_line = env_reader.readLine
  			while (env_line!=null) {
   				bufferedWriter.write(env_line+"\n")
    			env_line = env_reader.readLine
  			}
  		}

		bufferedWriter.write(envName+"\n")
		bufferedWriter.close();
		amazonS3Client.putObject(BUCKET_NAME, S3_EnvFileName_Prefix+envsFileName, envsFile)

		// write chanName, envName, envAccessCode into CACHE
		// the cache will be named in channel name and user name
		// overwrite each time
		val cacheFileName = chanName+"_"+userName+"_CACHE.txt"
		val c_file = new File(cacheFileName)
		c_file.createNewFile();
		val cacheFile = new PrintWriter(c_file)
		cacheFile.write(chanName+","+envName+","+envAccessCode)
		cacheFile.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName, c_file)

		val e_file = new File(fileName)
		e_file.createNewFile();
		val envFile = new PrintWriter(e_file)

		envFile.write("AC: "+envAccessCode+"\n")
		envFile.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_EnvFileName_Prefix+fileName, e_file)
		true
	}

	// ret 1: copied environment not exist
	// ret 2: wrong access code 
	// ret 3: new environment name already exist
	// ret 0: coppied success
	def envCopy(toChanName: String, fromChanName: String, userName: String, envName: String, envAccessCode: String, newEnvName: String, newAccessCode: String):Int = {

		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		val fileName: String = fromChanName+"_"+envName+"_Env.txt"
		// check environment exist or not
		if(!amazonS3Client.doesObjectExist(BUCKET_NAME, S3_EnvFileName_Prefix+fileName)) {return 1}

		val obj = amazonS3Client.getObject(BUCKET_NAME, S3_EnvFileName_Prefix+fileName)
  		val reader = new BufferedReader(new InputStreamReader(obj.getObjectContent()))
  		var line = reader.readLine
		println(line.stripPrefix("AC: "))

		// check accesscode is correct or not
		if(!envAccessCode.equals(line.stripPrefix("AC: "))){return 2}

		//check if can create new environment
		val newFileName:String = toChanName+"_"+newEnvName+"_Env.txt"
		if(amazonS3Client.doesObjectExist(BUCKET_NAME, S3_EnvFileName_Prefix+newFileName)) {return 3}

		// write to new env file
		val e_file = new File(newFileName)
		e_file.createNewFile();
		val envFile = new PrintWriter(e_file)

		envFile.write("AC: "+newAccessCode+"\n")
		line = reader.readLine
		while(line != null){
			envFile.write(line+"\n")
			line = reader.readLine
		}
		envFile.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_EnvFileName_Prefix+newFileName, e_file)

		// write envName into a chanName_envs.txt file
		val envsFileName:String = toChanName+"_Envs.txt"

		val envsFile:File = new File(envsFileName)
		val fileWriter:FileWriter = new FileWriter(envsFile, false)
		val bufferedWriter:BufferedWriter = new BufferedWriter(fileWriter);

		if (amazonS3Client.doesObjectExist(BUCKET_NAME, S3_EnvFileName_Prefix+envsFileName)){
			val env_obj = amazonS3Client.getObject(BUCKET_NAME, S3_EnvFileName_Prefix+envsFileName)
  			val env_reader = new BufferedReader(new InputStreamReader(env_obj.getObjectContent()))
  			var env_line = env_reader.readLine
  			while (env_line!=null) {
   				bufferedWriter.write(env_line+"\n")
    			env_line = env_reader.readLine
  			}
  		}

		bufferedWriter.write(newEnvName+"\n")
		bufferedWriter.close();
		amazonS3Client.putObject(BUCKET_NAME, S3_EnvFileName_Prefix+envsFileName, envsFile)

		// write chanName, envName, envAccessCode into CACHE
		// the cache will be named in channel name and user name
		// overwrite each time
		val cacheFileName = toChanName+"_"+userName+"_CACHE.txt"
		val c_file = new File(cacheFileName)
		c_file.createNewFile();
		val cacheFile = new PrintWriter(c_file)
		cacheFile.write(toChanName+","+newEnvName+","+newAccessCode)
		cacheFile.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_CacheFileName_Prefix+cacheFileName, c_file)
		0

	}


}
