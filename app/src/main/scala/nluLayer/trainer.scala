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

	// private
	val Max_Priority = 2

	val ActionMap:HashMap[String, String] = HashMap.empty[String,String]
	val CreateIndMap:HashMap[String, String] = HashMap.empty[String, String]
	val CreateEnvIndMap:HashMap[String, String] = HashMap.empty[String,String]
	val CreateDsIndMap:HashMap[String, String] = HashMap.empty[String,String]

	val AccessEnvIndMap:HashMap[String, String] = HashMap.empty[String, String]
	val LogInIndMap:HashMap[String, String] = HashMap.empty[String, String]
	// val LogOffIndMap:HashMap[String, String] = HashMap.empty[String, String] // no use because log off dont take argument

	val PrintIndMap:HashMap[String, String] = HashMap.empty[String, String]
	val CheckOutIndMap:HashMap[String, String] = HashMap.empty[String, String]

	val CommentEnvIndMap:HashMap[String, String] = HashMap.empty[String, String]

	val CommitIndMap:HashMap[String, String] = HashMap.empty[String, String]

	val HardResetIndMap:HashMap[String, String] = HashMap.empty[String, String]
	val ResetIndMap:HashMap[String, String] = HashMap.empty[String, String]

	val AppendIndMap:HashMap[String, String] = HashMap.empty[String, String]

	def train() = {
		act_create_train()
		act_access_env_train()
		act_print_train()
		act_comment_train()
		act_commit_train()
		act_reset_train()
		act_append_train()

		val actionModFileName = "action_model.txt"
		write_into_AWS(actionModFileName, ActionMap)

		val createIndFileName = "create_model.txt"
		write_into_AWS(createIndFileName, CreateIndMap)

		val createEnvIndFileName = "create_env_model.txt"
		write_into_AWS(createEnvIndFileName, CreateEnvIndMap)

		val createDsIndFileName = "create_ds_model.txt"
		write_into_AWS(createDsIndFileName, CreateDsIndMap)

		val accessEnvIndFileName = "access_env_model.txt"
		write_into_AWS(accessEnvIndFileName, AccessEnvIndMap)

		val logInIndFileName = "log_in_model.txt"
		write_into_AWS(logInIndFileName, LogInIndMap)

		val printIndFileName = "print_model.txt"
		write_into_AWS(printIndFileName, PrintIndMap)

		val checkOutIndFileName = "check_out_model.txt"
		write_into_AWS(checkOutIndFileName, CheckOutIndMap)

		val commentEnvIndFileName = "comment_env_model.txt"
		write_into_AWS(commentEnvIndFileName, CommentEnvIndMap)

		val commitIndFileName = "commit_model.txt"
		write_into_AWS(commitIndFileName, CommitIndMap)

		val resetIndFileName = "reset_model.txt"
		write_into_AWS(resetIndFileName, ResetIndMap)

		val hardResetIndFileName = "hard_reset_model.txt"
		write_into_AWS(hardResetIndFileName, HardResetIndMap)

		val appendIndFileName = "append_model.txt"
		write_into_AWS(appendIndFileName, AppendIndMap)

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
				val tok = cont(2)+"_"+cont(1)
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
				val tok = cont(2)+"_"+cont(1)
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

	// for access env action
	def act_access_env_train() = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		// read from training doc labeled access env
		val accessEnvActTrainFileName = "access_env_act_train.txt"

		// interpret train file for each specific action
		val accessEnvActObj = amazonS3Client.getObject(BUCKET_NAME, S3_TrainFileName_Prefix+accessEnvActTrainFileName)
		val accessEnvActReader = new BufferedReader(new InputStreamReader(accessEnvActObj.getObjectContent()))
		var accessEnvActLine = accessEnvActReader.readLine
		accessEnvActLine = accessEnvActReader.readLine

			// log in env
		while(!accessEnvActLine.equals("## log off")){
			for(word <- accessEnvActLine.split("\\s+")){
				val cont = act_interp(word).split("_")

				val i  = cont(0)
				val tok = cont(2)+"_"+cont(1)

				if(i.equals("$")){
					ActionMap += (tok -> "accessEnv")
				}else if(i.equals("0")){
					AccessEnvIndMap += (tok -> "in")
				}else if(!i.equals("F")){
					LogInIndMap += (tok -> i)
				}

			}
			accessEnvActLine = accessEnvActReader.readLine
		}

			// log off env 	// log off dont have argument
		while(accessEnvActLine != null){
			for(word <- accessEnvActLine.split("\\s+")){
				val cont = act_interp(word).split("_")

				val i  = cont(0)
				val tok = cont(2)+"_"+cont(1)

				if(i.equals("$")){
					ActionMap += (tok -> "accessEnv")
				}else if(i.equals("0")){
					AccessEnvIndMap += (tok -> "off")
				}

			}
			accessEnvActLine = accessEnvActReader.readLine
		}

	}

	// for print action
	def act_print_train() = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		// read from training doc labeled print
		val printActTrainFileName = "print_act_train.txt"

		// interpret train file for each specific action
		val printActObj = amazonS3Client.getObject(BUCKET_NAME, S3_TrainFileName_Prefix+printActTrainFileName)
		val printActReader = new BufferedReader(new InputStreamReader(printActObj.getObjectContent()))
		var printActLine = printActReader.readLine
		printActLine = printActReader.readLine

			// print envs 
		while(!printActLine.equals("## print env")){
			for(word <- printActLine.split("\\s+")){
				val cont = act_interp(word).split("_")

				val i  = cont(0)
				val tok = cont(2)+"_"+cont(1)

				if(i.equals("$")){
					ActionMap += (tok -> "print")
				}else if(i.equals("0")){
					PrintIndMap += (tok -> "envs")
				}
			}
			printActLine = printActReader.readLine
		}

			// print env
		while(!printActLine.equals("## print log")){
			for(word <- printActLine.split("\\s+")){
				val cont = act_interp(word).split("_")

				val i  = cont(0)
				val tok = cont(2)+"_"+cont(1)

				if(i.equals("$")){
					ActionMap += (tok -> "print")
				}else if(i.equals("0")){
					PrintIndMap += (tok -> "env")
				}
			}
			printActLine = printActReader.readLine
		}
			// print log
		while(!printActLine.equals("## check out")){
			for(word <- printActLine.split("\\s+")){
				val cont = act_interp(word).split("_")

				val i  = cont(0)
				val tok = cont(2)+"_"+cont(1)

				if(i.equals("$")){
					ActionMap += (tok -> "print")
				}else if(i.equals("0")){
					PrintIndMap += (tok -> "log")
				}
			}
			printActLine = printActReader.readLine
		}

			// check out
		while(printActLine != null){
			for(word <- printActLine.split("\\s+")){
				val cont = act_interp(word).split("_")

				val i  = cont(0)
				val tok = cont(2)+"_"+cont(1)

				if(i.equals("$")){
					ActionMap += (tok -> "print")
				}else if(i.equals("0")){
					PrintIndMap += (tok -> "commit")
				}else if(!i.equals("F")){
					CheckOutIndMap += (tok -> i)
				}
			}
			printActLine = printActReader.readLine
		}
	}

	// for comment action
	def act_comment_train() = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		// read from training doc labeled comment
		val commentActTrainFileName = "comment_act_train.txt"

		// interpret train file for each specific action
		val commentActObj = amazonS3Client.getObject(BUCKET_NAME, S3_TrainFileName_Prefix+commentActTrainFileName)
		val commentActReader = new BufferedReader(new InputStreamReader(commentActObj.getObjectContent()))
		var commentActLine = commentActReader.readLine

		while(commentActLine != null){
			for(word <- commentActLine.split("\\s+")){
				val cont = act_interp(word).split("_")

				val i  = cont(0)
				val tok = cont(2)+"_"+cont(1)

				if(i.equals("$")){
					ActionMap += (tok -> "comment")
				}else if(!i.equals("F")){
					CommentEnvIndMap += (tok -> i)
				}
			}
			commentActLine = commentActReader.readLine
		}
	}

	// for commit action
	def act_commit_train() = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		// read from training doc labeled commit
		val commitActTrainFileName = "commit_act_train.txt"

		// interpret train file for each specific action
		val commitActObj = amazonS3Client.getObject(BUCKET_NAME, S3_TrainFileName_Prefix+commitActTrainFileName)
		val commitActReader = new BufferedReader(new InputStreamReader(commitActObj.getObjectContent()))
		var commitActLine = commitActReader.readLine

		while(commitActLine != null){
			for(word <- commitActLine.split("\\s+")) {
				val cont = act_interp(word).split("_")

				val i = cont(0)
				val tok = cont(2)+"_"+cont(1)

				if(i.equals("$")){
					ActionMap += (tok -> "commit")
				}else if(!i.equals("F")){
					CommitIndMap += (tok -> i)
				}
			}
			commitActLine = commitActReader.readLine
		}
	}

	// for reset action 
	def act_reset_train() = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		// read from training doc labeled reset
		val resetActTrainFileName = "reset_act_train.txt"

		// interpret train file for each specific action
		val resetActObj = amazonS3Client.getObject(BUCKET_NAME, S3_TrainFileName_Prefix+resetActTrainFileName)
		val resetActReader = new BufferedReader(new InputStreamReader(resetActObj.getObjectContent()))
		var resetActLine = resetActReader.readLine

		while(!resetActLine.equals("## hard reset")){
			for(word <- resetActLine.split("\\s+")) {
				val cont = act_interp(word).split("_")

				val i = cont(0)
				val tok = cont(2)+"_"+cont(1)

				if(i.equals("$")){
					ActionMap += (tok -> "reset")
				}else if(!i.equals("F")){
					ResetIndMap += (tok -> i)
				}
			}
			resetActLine = resetActReader.readLine
		}
		while(resetActLine != null) {
			for(word <- resetActLine.split("\\s+")) {
				val cont = act_interp(word).split("_")

				val i = cont(0)
				val tok = cont(2)+"_"+cont(1)

				if(i.equals("$")){
					ActionMap += (tok -> "reset")
				}else if(i.equals("0")){
					HardResetIndMap += (tok -> "hard")
				}else if(!i.equals("F")){
					ResetIndMap += (tok -> i)
				}
			}
			resetActLine = resetActReader.readLine
		}
	}

	def act_append_train() = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)

		// read from training doc labeled reset
		val appendActTrainFileName = "append_act_train.txt"

		// interpret train file for each specific action
		val appendActObj = amazonS3Client.getObject(BUCKET_NAME, S3_TrainFileName_Prefix+appendActTrainFileName)
		val appendActReader = new BufferedReader(new InputStreamReader(appendActObj.getObjectContent()))
		var appendActLine = appendActReader.readLine

		while(appendActLine != null) {
			for(word <- appendActLine.split("\\s+")){
				val cont = act_interp(word).split("_")

				val i = cont(0)
				val tok = cont(2)+"_"+cont(1)

				if(i.equals("$")){
					ActionMap += (tok -> "append")
				}else if(!i.equals("F")){
					AppendIndMap += (tok -> i)
				}
			}

			appendActLine = appendActReader.readLine
		}
	}

	def write_into_AWS(modelFileName: String, modelMap: HashMap[String, String]) = {
		val awsCredentials = new BasicAWSCredentials(AWS_ACCESS_KEY, AWS_SECRET_KEY)
  		val amazonS3Client = new AmazonS3Client(awsCredentials)
		val modelFile = new File(modelFileName)
		val modelFileWriter = new FileWriter(modelFile, false)
		val modelBufferedWriter = new BufferedWriter(modelFileWriter)
		for(i <- 0 to Max_Priority){
			val priority = (Max_Priority - i).toString
			for ((k,v) <- modelMap if k.startsWith(priority)){
				//println(k+" "+v)
				modelBufferedWriter.write(k.stripPrefix(priority+"_")+","+v+"\n")
			}
		}
		modelBufferedWriter.close()
		amazonS3Client.putObject(BUCKET_NAME, S3_ModelFileName_Prefix+modelFileName, modelFile)
	}

	// ret -1	: _$_
	// ret 0	: _0_
	// ret 1    : _1_
	// ret 2	: _2_
	def act_interp(word: String): String = {
		word match {
			case word if word.startsWith("_$_") => {
				val cont = word.stripPrefix("_$_").stripSuffix("_").split("_")
				val tok = cont(0)
				val priority = cont(1)
				return "$_"+tok+"_"+priority
			}
			case word if word.startsWith("_0_") => {
				val cont = word.stripPrefix("_0_").stripSuffix("_").split("_")
				val tok = cont(0)
				val priority = cont(1)
				return "0_"+tok+"_"+priority
			}
			case word if word.startsWith("_1_") => {
				val cont = word.stripPrefix("_1_").stripSuffix("_").split("_")
				val tok = cont(0)
				val priority = cont(1)
				return "1_"+tok+"_"+priority
			}
			case word if word.startsWith("_2_") => {
				val cont = word.stripPrefix("_2_").stripSuffix("_").split("_")
				val tok = cont(0)
				val priority = cont(1)
				return "2_"+tok+"_"+priority
			}
			case word if word.startsWith("_3_") => {
				val cont = word.stripPrefix("_3_").stripSuffix("_").split("_")
				val tok = cont(0)
				val priority = cont(1)
				return "3_"+tok+"_"+priority
			}
			case _ =>{
				return "F_"+word+"_0"
			}
		}
	}
}
