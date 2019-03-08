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

	def envCreate(chanName: String, userName: String, envName: String, envAccessCode: String): Boolean = {
		
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		val fileName: String = chanName+"_"+envName+"_Env.txt"
		if(amazonS3Client.doesObjectExist(BUCKET_NAME, fileName)) {return false}

		// write chanName, envName, envAccessCode into CACHE
		// the cache will be named in channel name and user name
		// overwrite each time
		val cacheFileName = chanName+"_"+userName+"_CACHE.txt"
		val c_file = new File(cacheFileName)
		c_file.createNewFile();
		val cacheFile = new PrintWriter(c_file)
		cacheFile.write(chanName+","+envName+","+envAccessCode)
		cacheFile.close()
		amazonS3Client.putObject(BUCKET_NAME, cacheFileName, c_file)

		val e_file = new File(fileName)
		e_file.createNewFile();
		val envFile = new PrintWriter(e_file)

		envFile.write("AC: "+envAccessCode+"\n")
		envFile.close()
		amazonS3Client.putObject(BUCKET_NAME, fileName, e_file)
		true
	}



}
