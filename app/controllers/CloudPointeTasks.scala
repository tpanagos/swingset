package controllers

import org.activiti.engine.delegate._
import controllers.CloudPointeAPI._
import play.Logger
import scala.collection.JavaConversions._

/**
 *  REST client leveraging Play Web Service API https://github.com/playframework/Play20/wiki/ScalaWS
 *  CloudPointe Authorization Call: http://cloudxy.com/documentation/page.cfm?d=CloudPointe_Connect_R3&s=7.1.3
 */

class cp_RegisterUser extends org.activiti.engine.delegate.JavaDelegate {
		
	def execute (execution: org.activiti.engine.delegate.DelegateExecution) {
		var status:String = "GOOD" //need to reset just in case we had a failure in previous invocation
		dump_Variables(execution)
		//cp_authorize //this call for using the developer API
		status = cp_preauthorize //this call for using the partner API
		if (status == "GOOD")
			status = cp_createuser(execution)//this call for using the partner API
		if (status == "GOOD")
			status = cp_changepassword(execution)//immediately set the password to the desired text
		//if (status == "GOOD")
		//	cp_userinfo(executionScope)//immediately set the password to the desired text
		execution.setVariable("status", status)		
		dump_Variables(execution)
	}

}

class cp_UserSignIn extends org.activiti.engine.delegate.JavaDelegate {
		
	def execute (execution: org.activiti.engine.delegate.DelegateExecution) {
		var status:String = "GOOD" //need to reset just in case we had a failure in previous invocation
		dump_Variables(execution)
		status = cp_authorize //this call for using the developer API
		execution.setVariable("status", status)		
		dump_Variables(execution)
	}

}

class cp_ListStorageProviders extends org.activiti.engine.delegate.JavaDelegate {
		
	def execute (execution: org.activiti.engine.delegate.DelegateExecution) {
		Logger.info("In ListStorageProviders call.")
		var status:String = "GOOD" //need to reset just in case we had a failure in previous invocation
		//dump_Variables(execution)
		status = cp_authorize //this call for using the dev API
		if (status == "GOOD")
			status = cp_getstoragetypes(execution) //this call for using the partner API
		execution.setVariable("status", status)		
		//dump_Variables(execution)
	}

}

class cp_getStorageCredentials extends org.activiti.engine.delegate.JavaDelegate {
		
	def execute (execution: org.activiti.engine.delegate.DelegateExecution) {
		Logger.info("In cp_getStorageCredentials call.")
		var status:String = "GOOD" //need to reset just in case we had a failure in previous invocation
		//dump_Variables(execution)
		status = cp_authorize //this call for using the dev API
		if (status == "GOOD")
			status = cp_getstoragecreds(execution, execution.getVariable("provider").asInstanceOf[String])
		execution.setVariable("status", status)		
		//dump_Variables(execution)
	}

}

class cp_createStorageSite extends org.activiti.engine.delegate.JavaDelegate {
	
	def execute (execution: org.activiti.engine.delegate.DelegateExecution) {
		var status:String = "GOOD" //need to reset just in case we had a failure in previous invocation
		//dump_Variables(execution)
		//Logger.info("here is the cred: " + execution.getVariable("cp_str_provider_cred").toString)
		status = cp_authorize //this call for using the dev API
		if (status == "GOOD") {
			var r:Seq[play.api.libs.json.JsValue] = null
		    var credsParams: Map[String, String] = Map()
		    /* Loop through the known credential fields and build a parameter key/value structure */
		    for (i <- execution.getVariable("cp_str_provider_cred").asInstanceOf[scala.collection.immutable.$colon$colon[Any]].toList) {
				r = i.asInstanceOf[play.api.libs.json.JsArray].as[Seq[play.api.libs.json.JsValue]]
				credsParams += (r(4).as[String] -> execution.getVariable(r(4).as[String]).asInstanceOf[String])
				Logger.info(r(4).as[String] + " -> " + execution.getVariable(r(4).as[String]).asInstanceOf[String])
			}
			status = cp_createstoragesite(execution, credsParams, execution.getVariable("provider").asInstanceOf[String])//this call for using the partner API
		}
		execution.setVariable("status", status)		
		//dump_Variables(execution)
	}

}

class cp_getWorkspaceParams extends org.activiti.engine.delegate.JavaDelegate {
		
	def execute (execution: org.activiti.engine.delegate.DelegateExecution) {
		Logger.info("In cp_getStorageCredentials call.")
		var status:String = "GOOD" //need to reset just in case we had a failure in previous invocation
		//dump_Variables(execution)
		status = cp_authorize //this call for using the dev API
		if (status == "GOOD")
			status = cp_getstoragecreds(execution, execution.getVariable("provider").asInstanceOf[String])
		execution.setVariable("status", status)		
		//dump_Variables(execution)
	}

}