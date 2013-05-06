package controllers

import play.Logger
import java.lang.Exception 
import play.api.libs.ws.WS
import play.api.libs.concurrent.Promise
import play.api.libs.ws.Response
import play.api.mvc.{ SimpleResult, AsyncResult, AnyContentAsJson}
import play.api.libs.json._
import scala.collection.JavaConversions.mapAsScalaMap
import org.activiti.engine.delegate._

object CloudPointeAPI {
	
	var new_email:String = ""
	var new_password:String = ""

	var status:String = "GOOD"		
		
	val cp_ConnectURL = "https://connect.cloudxy.com";

	val cp_APIVersionDev = "/api/v1" //developer API
	val cp_APIVersionPartner = "/api/partner"; //partner API
	var cp_APIVersion = cp_APIVersionDev //default
	lazy val cp_URL = cp_ConnectURL + cp_APIVersion;
	val cp_RespType = ".json";

	val timeoutInterval = 1000;

	val partner_email = "meltmetal@gmail.com";
	val partner_password = "Loyl8oyl";
	val partner_apikey = "9f7ab436-6bc9-11e2-9638-12313d011937"; //app id
	//val partner_apikey = "11FC4672-0E0B-4143-9D92F1188AD9046E"; //personal developer key
	var SESSIONKEY = ""; //also holds the PARTNERKEY in the case of the Partner API being used
	
	def dump_Variables(execution: org.activiti.engine.delegate.DelegateExecution) {
		Logger.info("Dumping the Execution Available Variables")
		try {
			mapAsScalaMap(execution.getVariables.asInstanceOf[java.util.Map[String, String]]) map { x:(String, String) => {Logger.info(x._1 + " " + x._2.toString)}}				
		} catch {
			case e1: Exception => {
						Logger.error("Failure: Dumping Process Variables.")
						Logger.error(e1.getMessage() + "<br/>" + e1.getStackTraceString)
			}
		}
			
	}
	
	/*
	 * Lazy functions for composing the QueryStrings necessary to invoke a given API call
	 */
	def cp_authorize_param: Map[String, Seq[String]] = {
			cp_APIVersion = cp_APIVersionDev
			Map("email" -> Seq(partner_email), "password" -> Seq(partner_password), "apikey" -> Seq(partner_apikey))	
		}

	def cp_createuser_param(execution: org.activiti.engine.delegate.DelegateExecution): Map[String, Seq[String]] = {
			cp_APIVersion = cp_APIVersionPartner
			Map("apikey" -> Seq(partner_apikey),"email" -> Seq(execution.getVariable("email").toString), "firstname" -> Seq(execution.getVariable("first").toString), "lastname" -> Seq(execution.getVariable("last").toString))	    	
		}
	
	def cp_changepassword_param(execution: org.activiti.engine.delegate.DelegateExecution): Map[String, Seq[String]] = {
			cp_APIVersion = cp_APIVersionPartner
			Map("apikey" -> Seq(partner_apikey),"email" -> Seq(execution.getVariable("email").toString), "userid" -> Seq(execution.getVariable("cp_resp_userid").toString), "oldpassword" -> Seq(execution.getVariable("cp_resp_autopassword").toString), "newpassword" -> Seq(execution.getVariable("password").toString))	    	
		}	
	
	def cp_userinfo_param: Map[String, Seq[String]] = {
			cp_APIVersion = cp_APIVersionPartner
			Map("email" -> Seq(partner_email), "apikey" -> Seq(partner_apikey))	
		}	 
	
	def cp_no_params: Map[String, Seq[String]] = {
			Map()	
		}	
	
	def cp_createstoragesite_params(credentialMap: Map[String, String], providerId:String): Map[String, Seq[String]] = {
			cp_APIVersion = cp_APIVersionDev
			/*
			 * expects the credentials to be passed as a JSON like string array of the form
			 * {"PORTNUMBER":"21","USERNAME":"myuser","PASSIVE":"1","DEFAULTCHROOT":"\/path\/to\/rootdir","HOSTNAME":"my.host.com","PASSWORD":"cleartextpassword"}
			 * 
			 */
			var myCredArray:String = "{"
			credentialMap map { case(k: String, v:String) => myCredArray += "\"" + k + "\":\"" + v + "\","}
			myCredArray = myCredArray.reverse.tail.reverse + '}'
			//Logger.info(" here it is: " + myCredArray)
			Map("siteTypeId" -> Seq(providerId),"name" -> Seq("FirstStorage"), "enabled" -> Seq("1"), "loggingEnabled" -> Seq("1"), "revisionControl" -> Seq("1"), "maxRevisions" -> Seq("3"), "checkinCheckout" -> Seq("0"), "indexingEnabled" -> Seq("0"), "siteArguments" -> Seq(myCredArray))   	
		}
	/*
	 * API Entry Functions
	 */
			
	/*
	* Authorization and PreAuthorization functions for invoking a given API call
	*/  	
	def cp_authorize: String = {
			status = "GOOD"
			cp_stringmap_common_call("auth", cp_authorize_param, {(resp:Response) => (resp.json \ "RESULT" \ "SESSIONKEY")}, {(x:String) => (SESSIONKEY = x)}, {(resp:Response) => resp})
	}
	
	/*
	* Functions valid with the Partner API Only
	*/
	def cp_preauthorize: String = {
			status = "GOOD"
			cp_stringmap_common_call("preauth", cp_authorize_param, {(resp:Response) => (resp.json \ "PARTNERKEY")}, {(x:String) => (SESSIONKEY = x)}, {(resp:Response) => resp}) 
	}
	
	def cp_createuser(execution: org.activiti.engine.delegate.DelegateExecution): String = {
		status = "GOOD"
		var newUserId: String = ""
		val myMatchandStore = (resp: Response) => {
			val matchMap = Map(("USERID" -> "cp_resp_userid"), ("PASSWORD" -> "cp_resp_autopassword"))
		    for ((aResp, aVar) <- matchMap) {
		    	(resp.json \ aResp) match { //apply the function passed at the argument keyMatch to allow custom matchings
			    	case play.api.libs.json.JsString(x) => {
			    		execution.setVariable(aVar, x)
			    		Logger.info("Found expected Response Field " + aResp + " contains value " + x + " will be add to Variable Name " + aVar + ".")		
			    	}
			    	case _ => {
			    		//status = "FAIL"
			    		execution.setVariable(aVar, "")
			    		Logger.info("Failed to find expected Response Field " + aResp + " adding a blank value to Variable Name " + aVar + ".")		
			    	}			    	
		    	}
		    }
		}
		cp_responsemap_common_call("user/create", cp_createuser_param(execution), myMatchandStore, ("Authorization" -> SESSIONKEY))
		Logger.info("New User ID is " + execution.getVariable("cp_resp_userid") + ".")
		status
	}	

	def cp_changepassword(execution: org.activiti.engine.delegate.DelegateExecution): String  = {
		status = "GOOD"
		var error_Status: String = ""
		val myMatchandStore = (resp: Response) => {
			val matchMap = Map(("MESSAGE" -> "cp_pwd_change_msg"))
		    for ((aResp, aVar) <- matchMap) {
		    	(resp.json \ aResp) match { //apply the function passed at the argument keyMatch to allow custom matchings
			    	case play.api.libs.json.JsString(x) => {
			    		execution.setVariable(aVar, x)
			    		Logger.info("Found expected Response Field " + aResp + " contains value " + x + " will be add to Variable Name " + aVar + ".")		
			    	}
			    	case _ => {
			    		//status = "FAIL"
			    		execution.setVariable(aVar, "")
			    		Logger.info("Failed to find expected Response Field " + aResp + " adding a blank value to Variable Name " + aVar + ".")		
			    	}			    	
		    	}
		    }
		}
		cp_responsemap_common_call("user/chpass", cp_changepassword_param(execution), myMatchandStore, ("Authorization" -> SESSIONKEY))
		Logger.info("Password Change Status is " + error_Status + ".")
		status
	}

	def cp_userinfo(execution: org.activiti.engine.delegate.DelegateExecution): String  = {
		status = "GOOD"
		var newUserId: String = ""
		val myMatchandStore = (resp: Response) => {
			val matchMap = Map(("USERID" -> "cp_resp_userid"), ("GROUPID" -> "cp_resp_groupid"), ("FNAME" -> "cp_resp_firstname"), ("LNAME" -> "cp_resp_lastname"))
		    for ((aResp, aVar) <- matchMap) {
		    	(resp.json \ aResp) match { //apply the function passed at the argument keyMatch to allow custom matchings
			    	case play.api.libs.json.JsString(x) => {
			    		execution.setVariable(aVar, x)
			    		Logger.info("Found expected Response Field " + aResp + " contains value " + x + " will be add to Variable Name " + aVar + ".")		
			    	}
			    	case _ => {
			    		//status = "FAIL"
			    		execution.setVariable(aVar, "")
			    		Logger.info("Failed to find expected Response Field " + aResp + " adding a blank value to Variable Name " + aVar + ".")		
			    	}			    	
		    	}
		    }
		}
		cp_responsemap_common_call("user/info", cp_createuser_param(execution), myMatchandStore, ("Authorization" -> SESSIONKEY))
		status
	}	
	
	def cp_getstoragetypes(execution: org.activiti.engine.delegate.DelegateExecution): String = {
		//Logger.info("In getStorageType and the SESSION KEY IS " + SESSIONKEY)
		status = "GOOD"
		var error_Status: String = ""
		/* define a function that will first match specific JSON nodes and second does something to store the matched responses */
		val myMatchandStore = (resp: Response) => {
			val matchMap = Map(("DATA" -> "cp_str_providers"))
		    for ((aResp, aVar) <- matchMap) {
		    	(resp.json \ "RESULT" \ aResp) match { //apply the function passed at the argument keyMatch to allow custom matchings
			    	case play.api.libs.json.JsArray(x) => {
			    		/* of all the data, we just want the names and ids */
			    		val providers = x map {(A) => (A(0),A(1))}
			    		Logger.info(providers.toString)
			    		execution.setVariable(aVar, providers)
			    		Logger.info("Found expected Response Field " + aResp + " contains value " + x + " will be add to Variable Name " + aVar + ".")		
			    	}
			    	case _ => {
			    		status = "FAIL"
			    		execution.setVariable(aVar, "")
			    		Logger.info("Failed to find expected Response Field " + aResp + " adding a blank value to Variable Name " + aVar + ".")		
			    	}			    	
		    	}
		    }
		}
		Logger.info("trying")
		cp_responsemap_common_call("storagetypes/list", cp_no_params, myMatchandStore, ("Authorization" -> SESSIONKEY))
		Logger.info("Results of StorageTypes Listing Status is " + error_Status + ".")
		status
	}	

	def cp_getstoragecreds(execution: org.activiti.engine.delegate.DelegateExecution, providerId:String): String = {
		//Logger.info("In getStorageType and the SESSION KEY IS " + SESSIONKEY)
		status = "GOOD"
		var error_Status: String = ""
		/* define a function that will first match specific JSON nodes and second does something to store the matched responses */
		val myMatchandStore = (resp: Response) => {
			val matchMap = Map(("DATA" -> "cp_str_provider_cred"))
		    for ((aResp, aVar) <- matchMap) {
		    	(resp.json \ "RESULT" \ aResp) match { //apply the function passed at the argument keyMatch to allow custom matchings
			    	case play.api.libs.json.JsArray(x) => {
			    		/* of all the data, we just want the names and ids */
			    		execution.setVariable(aVar, x)
			    		Logger.info("Found expected Response Field " + aResp + " contains value " + x + " will be add to Variable Name " + aVar + ".")		
			    	}
			    	case _ => {
			    		status = "FAIL"
			    		execution.setVariable(aVar, "")
			    		Logger.info("Failed to find expected Response Field " + aResp + " adding a blank value to Variable Name " + aVar + ".")		
			    	}			    	
		    	}
		    }
		}
		cp_responsemap_common_call("storagetypes/" + providerId + "/params", cp_no_params, myMatchandStore, ("Authorization" -> SESSIONKEY))
		status
	}
	
	def cp_createstoragesite(execution: org.activiti.engine.delegate.DelegateExecution, credentialMap: Map[String, String], providerId:String): String = {
		//Logger.info("In getStorageType and the SESSION KEY IS " + SESSIONKEY)
		status = "GOOD"
		var error_Status: String = ""
		/* define a function that will first match specific JSON nodes and second does something to store the matched responses */
		val myMatchandStore = (resp: Response) => {
			val matchMap = Map(("SiteId" -> "cp_str_site_id"))
		    for ((aResp, aVar) <- matchMap) {
		    	(resp.json \ "RESULT" \ aResp) match { //apply the function passed at the argument keyMatch to allow custom matchings
			    	case play.api.libs.json.JsString(x) => {
			    		execution.setVariable(aVar, x)
			    		Logger.info("Found expected Response Field " + aResp + " contains value " + x + " will be add to Variable Name " + aVar + ".")		
			    	}
			    	case _ => {
			    		execution.setVariable(aVar, "")
			    		Logger.info("Failed to find expected Response Field " + aResp + " adding a blank value to Variable Name " + aVar + ".")		
			    	}			    	
		    	}
		    } 
		}
		Logger.info("trying")
		cp_responsemap_common_call("storagesite/create", cp_createstoragesite_params(credentialMap, providerId), myMatchandStore, ("Authorization" -> SESSIONKEY))
		Logger.info("Results of StorageTypes Listing Status is " + error_Status + ".")
		status
	}

	def cp_getworkspaceparams(execution: org.activiti.engine.delegate.DelegateExecution, providerId:String): String = {
		//Logger.info("In getStorageType and the SESSION KEY IS " + SESSIONKEY)
		status = "GOOD"
		var error_Status: String = ""
		/* define a function that will first match specific JSON nodes and second does something to store the matched responses */
		val myMatchandStore = (resp: Response) => {
			val matchMap = Map(("DATA" -> "cp_str_provider_workspace_params"))
		    for ((aResp, aVar) <- matchMap) {
		    	(resp.json \ "RESULT" \ aResp) match { //apply the function passed at the argument keyMatch to allow custom matchings
			    	case play.api.libs.json.JsArray(x) => {
			    		/* of all the data, we just want the names and ids */
			    		execution.setVariable(aVar, x)
			    		Logger.info("Found expected Response Field " + aResp + " contains value " + x + " will be add to Variable Name " + aVar + ".")		
			    	}
			    	case _ => {
			    		status = "FAIL"
			    		execution.setVariable(aVar, "")
			    		Logger.info("Failed to find expected Response Field " + aResp + " adding a blank value to Variable Name " + aVar + ".")		
			    	}			    	
		    	}
		    }
		}
		cp_responsemap_common_call("workspace/" + providerId + "/params", cp_no_params, myMatchandStore, ("Authorization" -> SESSIONKEY))
		status
	}
	
	/*
	 * Common functions used by individual API calls
	 */
	
	/*
	* Common function body for all API calls that need to match a single response json string and do some mapping for it
	* 	keyMatch is a function block that expresses how to match from the JSON
	*  		eg. {(resp:Response) => (resp.json \ "PARTNERKEY")}
	*    
	* 	foundKeyStore is a function block that does something with the string value if it is found by the keyMatch function
	*  		eg. {(x:String) => (SESSIONKEY = x.toString)}    
	*/	
	def cp_responsemap_common_call(apiName: String, cp_param: => Map[String, Seq[String]], matchAndStoreFunction: (Response) => Unit, hdrs: (String, String)*): String = {	    				
					Logger.info("trying")		
					try {
						val response = cp_APICall(apiName, cp_param, hdrs :_*) 	
								response match {
								case Some(resp) => {
									if (resp.status == 200) {
										(resp.json \ "ERROR") match {
											case play.api.libs.json.JsNumber(x) => {
												if (x == 0) {
													Logger.info("Success: Invoking CloudPointe REST API WS " +  apiName)		
													Logger.info("The Response: " + resp.body)
													Logger.info("The Session Key is : " + SESSIONKEY)														
												} else {
													// this fail condition is a valid internal response from server reported a ERROR=1
													status = "FAIL"
													Logger.error("Failure: Invoking CloudPointe REST API WS " +  apiName + ": " + (resp.json \ "MESSAGE").toString)
													Logger.info("The Response: " + resp.body)													
												}
											}
											case _ => {
												// this fail condition is a valid internal response from server reported a ERROR=? by the type is not an integer which means the API has changed
												status = "FAIL"
												Logger.error("Failure: Invoking CloudPointe REST API WS " +  apiName + ": " + (resp.json \ "MESSAGE").toString)
												Logger.info("The Response: " + resp.body)
											}	
										}
										matchAndStoreFunction(resp) //execute the code block or function passed in to the function
									} else {
										//  this fail condition is a response from the HTTP server reporting an unknown HTTP error
										Logger.error("Failure: Invoking CloudPointe REST API WS " +  apiName + ": " + resp.status + " status received from " + cp_URL + ".")
										status = "FAIL"		
											}
								}
								case None =>
								{
									// this fail condition is an empty response object which would most likely be a bug in the program
									Logger.error("Failure: Invoking CloudPointe REST API WS " + apiName + ": Invalid response received from " + cp_URL + ".")
									status = "FAIL"								
										}
						}
					} catch {
						case e1: Exception => {
							Logger.error("Failure: Invoking CloudPointe REST API WS " + apiName + ".")
							Logger.error(e1.getMessage() + "<br/>" + e1.getStackTraceString)
							status = "FAIL"				
								}
					}
					status
		}
	
	/*
	* Common function body for all API calls that need to match a single response json string and do some mapping for it
	* 	keyMatch is a function block that expresses how to match from the JSON
	*  		eg. {(resp:Response) => (resp.json \ "PARTNERKEY")}
	*    
	* 	foundKeyStore is a function block that does something with the string value if it is found by the keyMatch function
	*  		eg. {(x:String) => (SESSIONKEY = x.toString)}    
	*/	
	def cp_stringmap_common_call(apiName: String, cp_param: => Map[String, Seq[String]], keyMatch: (Response) => JsValue, foundKeyStore: (String) => Unit, matchAndStoreFunction: (Response) => Unit, hdrs: (String, String)*):String = {	    				
					try {
						val response = cp_APICall(apiName, cp_param, hdrs :_*) 	
								response match {
								case Some(resp) => {
									if (resp.status == 200) {
										//(resp.json \ "RESULT" \ "PARTNERKEY") match {
										keyMatch(resp) match { //apply the function passed at the argument keyMatch to allow custom matchings
											case play.api.libs.json.JsString(x) => {
												//SESSIONKEY = x.toString;
												foundKeyStore(x)
												Logger.info("Success: Invoking CloudPointe REST API WS " +  apiName)		
												Logger.info("The Response: " + resp.body)
												Logger.info("The Session Key is : " + SESSIONKEY)	
											}
											case _ => {
												status = "FAIL"
												Logger.error("Failure: Invoking CloudPointe REST API WS " +  apiName + ": " + (resp.json \ "MESSAGE").toString)
												Logger.info("The Response: " + resp.body)
											}	
										}
										matchAndStoreFunction(resp) //execute the code block or function passed in to the function
									} else {
										status = "FAIL"
										Logger.error("Failure: Invoking CloudPointe REST API WS " +  apiName + ": " + resp.status + " status received from " + cp_URL + ".")
									}
								}
								case None =>
								{
									status = "FAIL"
									Logger.error("Failure: Invoking CloudPointe REST API WS " + apiName + ": Invalid response received from " + cp_URL + ".")
								}
						}
					} catch {
						case e1: Exception => {
							status = "FAIL"
							Logger.error("Failure: Invoking CloudPointe REST API WS " + apiName + ".")
							Logger.error(e1.getMessage() + "<br/>" + e1.getStackTraceString)
						}
					}
					status
		}

	/*
	* Generic function call to invoke a given REST resource with a variable query string POST
	*/	    
	def cp_APICall(apiCallURL: String, param: => Map[String, Seq[String]], hdrs: (String, String)*): Option[Response] = {
						Logger.info("Invoking CloudPointe REST API WS " + cp_URL + "/" + apiCallURL + cp_RespType)
						var promiseOfResponse: Promise[Response] = null
						if (param.isEmpty) {
							Logger.info("Invoking CloudPointe REST API WS GET " + cp_URL + "/" + apiCallURL + cp_RespType)
							/* Assume a GET */							
								promiseOfResponse = WS.url(cp_URL + "/" + apiCallURL + cp_RespType).withHeaders(hdrs :_*).get;
							}
						else {
							/* Assume a POST */
							Logger.info("Invoking CloudPointe REST API WS POST " + cp_URL + "/" + apiCallURL + cp_RespType)
							promiseOfResponse = WS.url(cp_URL + "/" + apiCallURL + cp_RespType).withHeaders(hdrs :_*).post(param);							
						}

						
						promiseOfResponse.orTimeout(timeoutInterval.toString, timeoutInterval).value.get match {
							case Left(result) => Some(result)
							case Right(timeout) => {
								status = "FAIL"
								Logger.error("Failure: Invoking CloudPointe REST API WS " + apiCallURL + ": Timeout limit of " + timeout + "reached.")
								None
							}
						}
	}

}