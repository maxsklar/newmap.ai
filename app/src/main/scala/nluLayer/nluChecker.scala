package ai.newmap.nluLayer

import ai.newmap.nluLayer.convTerminator.stopConv

object nluChecker {

	val CheckPrefix = """{
    "response_type": "ephemeral",
    """
    val CheckSuffix = """
	  "attachments": [
	      {
	          "text": "Did I understand it correct?",
	          "callback_id": "interpret_checker",
	          "color": "#3AA3E3",
	          "attachment_type": "default",
	          "actions": [
	              {
	                  "name": "Yes",
	                  "text": "Yes",
	                  "type": "button",
	                  "value": "Yes"
	              },
	              {
	                "name": "No",
	                "text": "No",
	                "type": "button",
	                "value": "No"
	              }
	          ]
	      }
	  ]
	}
	"""

	var Got = "_"
	var GoingToGet = "_"

	def generateButtonJsonString(got: String, goingToGet: String): String = {
		this.Got = got
		this.GoingToGet = goingToGet
		CheckPrefix+""" "text": """"+got+"""","""+CheckSuffix
	}
}

