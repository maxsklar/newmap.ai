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
import scala.collection.mutable.HashMap

import ai.newmap.environment.envConstant

object trainer {
	val BUCKET_NAME = envConstant.BUCKET_NAME
	val AWS_ACCESS_KEY = envConstant.AWS_ACCESS_KEY
	val AWS_SECRET_KEY = envConstant.AWS_SECRET_KEY

	val S3_TrainFileName_Prefix = "Train/"
	val S3_ModelFileName_Prefix = "Model/"

	val ActionMap:HashMap[String, String] = HashMap.empty[String,String]
	val CreateIndMap:HashMap[String, String] = HashMap.empty[String, String]
	val CreateEnvIndMap:HashMap[String, String] = HashMap.empty[String,String]
	val CreateDsIndMap:HashMap[String, String] = HashMap.empty[String,String]

	def train() = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)
		act_create_train()

		val actionModFileName = "action_model.txt"
		val actionModFile = new File(actionModFileName)
		val actionModFileWriter = new FileWriter(actionModFile, false)
		val actionModBufferedWriter = new BufferedWriter(actionModFileWriter)
		for ((k,v) <- ActionMap){
			actionModBufferedWriter.write(k+","+v+"\n")
		}
		actionModBufferedWriter.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_ModelFileName_Prefix+actionModFileName, actionModFile)

		val createIndFileName = "create_model.txt"
		val createIndFile = new File(createIndFileName)
		val createIndFileWriter = new FileWriter(createIndFile, false)
		val createIndBufferedWriter = new BufferedWriter(createIndFileWriter)
		for ((k,v) <- CreateIndMap){
			createIndBufferedWriter.write(k+","+v+"\n")
		}
		createIndBufferedWriter.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_ModelFileName_Prefix+createIndFileName, createIndFile)

		val createEnvIndFileName = "create_env_model.txt"
		val createEnvIndFile = new File(createEnvIndFileName)
		val createEnvIndFileWriter = new FileWriter(createEnvIndFile, false)
		val createEnvIndBufferedWriter = new BufferedWriter(createEnvIndFileWriter)
		for ((k,v) <- CreateEnvIndMap){
			createEnvIndBufferedWriter.write(k+","+v+"\n")
		}
		createEnvIndBufferedWriter.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_ModelFileName_Prefix+createEnvIndFileName, createEnvIndFile)

		val createDsIndFileName = "create_ds_model.txt"
		val createDsIndFile = new File(createDsIndFileName)
		val createDsIndFileWriter = new FileWriter(createDsIndFile, false)
		val createDsIndBufferedWriter = new BufferedWriter(createDsIndFileWriter)
		for ((k,v) <- CreateDsIndMap){
			createDsIndBufferedWriter.write(k+","+v+"\n")
		}
		createDsIndBufferedWriter.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_ModelFileName_Prefix+createDsIndFileName, createDsIndFile)

	}

	// for action create
	def act_create_train() = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		// read from training doc labeled create action
		val createActTrainFileName = "create_act_train.txt"

		// interpret train file for each specific action
		val createActObj = amazonS3Client.getObject(BUCKET_NAME, S3_TrainFileName_Prefix+createActTrainFileName)
		val createActReader = new BufferedReader(new InputStreamReader(createActObj.getObjectContent()))
		var createActLine = createActReader.readLine
		createActLine = createActReader.readLine

			// create environment
		while(!createActLine.equals("## create data structure")){
			for(word <- createActLine.split("\\s+")){
				val cont = act_interp(word).split("_")
				//println("***"+word+"***")
				val i  = cont(0)
				val tok = cont(1)
				//println("***"+i+","+tok+"***")
				if(i.equals("$")){
					ActionMap += (tok -> "create")
				}else if(i.equals("0")){
					CreateIndMap += (tok -> "env")
				}else if(!i.equals("F")){
					CreateEnvIndMap += (tok -> i)
				}
			}
			createActLine = createActReader.readLine
		}

			// create data structure
		while(createActLine != null){
			for(word <- createActLine.split("\\s+")){
				val cont = act_interp(word).split("_")
				//println("***"+word+"***")
				val i  = cont(0)
				val tok = cont(1)
				//println("***"+i+","+tok+"***")
				if(i.equals("$")){
					ActionMap += (tok -> "create")
				}else if(i.equals("0")){
					CreateIndMap += (tok -> "ds")
				}else if(!i.equals("F")){
					CreateDsIndMap += (tok -> i)
				}
			}
			createActLine = createActReader.readLine
		}
	}

	// ret -1	: _$_
	// ret 0	: _0_
	// ret 1    : _1_
	// ret 2	: _2_
	def act_interp(word: String): String = {
		word match {
			case word if word.startsWith("_$_") => {
				val tok = word.stripPrefix("_$_").stripSuffix("_$_").replace("_", " ")
				return "$_"+tok
			}
			case word if word.startsWith("_0_") => {
				val tok = word.stripPrefix("_0_").stripSuffix("_0_").replace("_", " ")
				return "0_"+tok
			}
			case word if word.startsWith("_1_") => {
				val tok = word.stripPrefix("_1_").stripSuffix("_1_").replace("_", " ")
				return "1_"+tok
			}
			case word if word.startsWith("_2_") => {
				val tok = word.stripPrefix("_2_").stripSuffix("_2_").replace("_", " ")
				return "2_"+tok
			}
			case _ =>{
				return "F_"+word
			}
		}
	}
}
