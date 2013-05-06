package controllers

import java.io._
import play.api._
import play.api.mvc._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.libs.json.Json
import play.data.DynamicForm
import org.activiti.engine._
import models._
import routes._


object Application extends Controller {

		private var processEngine: ProcessEngine = null
		private var repositoryService: RepositoryService = null
		private var runtimeService: RuntimeService = null
		private var taskService: TaskService = null
		private var formService: FormService = null
		private var deploymentObject: org.activiti.engine.repository.Deployment = null
		private var formData: org.activiti.engine.form.TaskFormData = null
		private	var processInstance: org.activiti.engine.runtime.ProcessInstance = null
		private var currentTask: org.activiti.engine.task.Task = null
		private var startFormData: org.activiti.engine.form.StartFormData = null
		private var message: String = ""
		private var lastTrace: String = ""
		private var newTrace: String = ""
		private var doModal: Short = 0 //doModal States: 0 Unset, 1 Modal, 2 Full Page	

		private val processMap = Map( //action from the route start/action -> processName, processFile
				"onboard" -> ("onboardingWizard","pointOnboardingMaster.bpmn"),
				//"register" -> ("registrationRequest","registration2.bpmn"),
				"signin" -> ("signinRequest","signinRequest.bpmn"),
				//"addstorage" -> ("addStorageRequest","addStorageRequest2.bpmn"),
				//"share" -> ("shareLinkRequest","shareLinkRequest.bpmn"),
				"reloadBPM" -> ("reloadBPMRequest","reloadBPMRequest.bpmn")
		)
		private var processName: String = "registrationRequest2" /* if this name is wrong, you will get a generic exception from the index fn: play.core.ActionInvoker$$anonfun$receive$1$$anon$1: Execution exception [[NullPointerException: null]]*/
		private var processFile: String = "simpleregistration.bpmn"
		//lazy private val processPath: String = "/Users/tim/Documents/dev/workspace/signupWizard/lib/" + processFile
		private val processPath: String = "/Users/tim/Documents/dev/workspace/signupWizard/lib/"		
		
		lazy private val processLabel: String = repositoryService.createProcessDefinitionQuery.latestVersion.processDefinitionKey(processName).singleResult.getName 
		lazy private val processDescription: String = repositoryService.createProcessDefinitionQuery.latestVersion.processDefinitionKey(processName).singleResult.getDescription
		
	  var dynTaskForm = new DynamicForm
	  initializeProcessEngine

	  
	  /**
	   * Action to respond to a GET from the / URL
	  */	  	  	  
      def index = Action {
		  /** This version starts an empty page with a Launch Button that triggers a Modal dialog to process the forms **/
		  doModal = 1
    	  Ok(views.html.index("Welcome to CloudPointe", "start", "nextHandle"))
    	  /** This version launches directly into the process **/
    	  //Ok(views.html.start(processLabel, routes.Application.startHandle, formData.getFormProperties, dynTaskForm))
      }   
	
	  /**
	   * Action to respond to a GET from the /start URL
	   * 
	   */
	  def start(action: String = "") = Action { 
	  	  Logger.info("in start with doModal marked as " + doModal)
	  	  Logger.info("Start action is " + action + " the process to run is " + processMap(action)._1 + " from file " + processMap(action)._2 + ".")
	  	  if (action != "") {
		  	  processName = processMap(action)._1
		  	  processFile = processMap(action)._2  	
	  	  }

	  	  if (doModal == 0) doModal = 2
		  deployProcess
		  try {
		  	  Logger.info(processName + " There are this many processdefs: " + repositoryService.createProcessDefinitionQuery.active.count)
		  	  Logger.info(processName + " There are this many processdefs: " + repositoryService.createProcessDefinitionQuery.latestVersion.processDefinitionKey(processName).count)
			  val processDef = repositoryService.createProcessDefinitionQuery.latestVersion.processDefinitionKey(processName).singleResult;
			  if (processDef != null) {
				  startFormData = formService.getStartFormData(processDef.getId)
			  } 
			  else {throw new IllegalArgumentException(processName + " is not a recognized Process Key.")}

			  if (startFormData.getFormProperties.size > 0) {
			  	  Logger.info("in start no props calling start with startHandle")
				  Logger.info("The Process " + processName + " has " + startFormData.getFormProperties.size + " StartFormProperties. Displaying Start Form.")
				  Ok(views.html.start(processLabel, routes.Application.startHandle, startFormData.getFormProperties, dynTaskForm)) 	
			  } else {
				  Logger.info("The Process " + processName + " has " + startFormData.getFormProperties.size + " StartFormProperties. Skipping to Next Task.")
				  var variables = new java.util.HashMap[String, Object]
				  // start the process
				  startProcess(variables)
				  // go to the next function to continue
				  controllers.Application.nextCore
			  }			
			} catch {
				case e: ActivitiException => {
					newTrace = e.getStackTraceString					
					Logger.error("Activiti Failure: Initializing Activiti Process " + processName + ".")
					Logger.error(e.getMessage)
					Logger.error(newTrace)
				    BadRequest(views.html.error(traceContent)).withSession("trace" -> newTrace) 
				}
				case e: Exception => {
					newTrace = e.getStackTraceString
					Logger.error("Failure: Initializing Activiti Process " + processName + ".")	
					Logger.error(e.getMessage)
					Logger.error(newTrace)
				    BadRequest(views.html.error(traceContent)).withSession("trace" -> newTrace) 
				}
			} finally {
			
			}
	  }
	  
	  /**
	   * Handle the POST 'starting process workflow form' submission 
	   * 
	   */
	  def startHandle = Action { implicit request =>
		  var variables: java.util.HashMap[String, Object] = getVariablesFromBody(request.body)
		  
		  startProcess(variables)
		  
		  /* we need to check because there may be a start without any steps */
		  val numTasks = taskService.createTaskQuery.processInstanceId(processInstance.getId()).count
		  if ((numTasks) > 0) {
		  	  /* still more tasks to be done */
		  	  Logger.info("The Process with ID " + processInstance.getId() + " has " + numTasks + " more Task to complete.")
			  Redirect(routes.Application.next)	   		  	
		  } else {
			  Redirect(routes.Application.last)	  		  	
		  } 
	  }
	  
	  /**
	   * Action to respond to a GET from the /next URL
	  */	  	  	  
      def next = Action {
      	 //Logger.info("in next with doModal marked as " + doModal)
      	 controllers.Application.nextCore
      }     

      /*
       * Core of the next definition pulled out so that the start definition can also call into it to avoid duplicate code
       */
      def nextCore: play.api.mvc.SimpleResult[play.api.templates.Html] = {
      		getCurrentTaskFormData match {
			 case Some(formData) => {
				 if (doModal == 1) { 
					 Logger.info("in next w formdata and modal 1 calling wfForm with nextHandle")
					 Ok(views.html.wfForm(message, routes.Application.nextHandle, formData.getFormProperties, dynTaskForm)) /* if we want a modal form replace, then trigger this */
				 } else { 
					 Logger.info("in next w formdata and modal not 1 call next with nextHandle")					 
					 Ok(views.html.next(processLabel, routes.Application.nextHandle, formData.getFormProperties, dynTaskForm)) /* if we want the whole page to refresh, then trigger this */
				 }
			 }
			 case None => {
				 val processDef = repositoryService.createProcessDefinitionQuery.latestVersion.processDefinitionKey(processName).singleResult;
				 Logger.info("My ProcessDefId is " + processDef.getId + " with name " + processName)
				 val myForm = formService.getTaskFormKey(processDef.getId, currentTask.getTaskDefinitionKey);
				 if (myForm != "" ) {
					 Logger.info("Task has a custom form with id " + myForm + ".")	
					 var myVariableMap: scala.collection.mutable.Map[String,java.lang.Object] = scala.collection.JavaConversions.mapAsScalaMap(runtimeService.getVariables(processInstance.getId))
					 Logger.info(myVariableMap.toString)
					 Ok(views.html.custom.flex(myForm, routes.Application.nextHandle, myVariableMap, dynTaskForm)) /* if we want the whole page to refresh, then trigger this */
				 } else {
					 Logger.info("in next no formdata calling last")
					 Ok(views.html.last("Your " + processLabel + " is complete.<br/>This Process has " + processDescription + ".<br/>Thank you!"))  					  			
				 }					 
			 }
		  }
      }
	  /**
	   * Handle the POST 'next process workflow form' submission 
	  */
	  def nextHandle = Action { implicit request =>
		  var variables: java.util.HashMap[String, Object] = getVariablesFromBody(request.body)
		  
		  completeTask(variables)
		  
		  /* we need to check because there may be a start without any steps */
		  val numTasks = taskService.createTaskQuery.processInstanceId(processInstance.getId()).count
		  if ((numTasks) > 0) {
		  	  /* still more tasks to be done */
		  	  Logger.info("The Process with ID " + processInstance.getId() + " has " + numTasks + " more Task to complete.")
			  Redirect(routes.Application.next)	   		  	
		  } else {
			  Redirect(routes.Application.last)	  		  	
		  } 
	  }	  

	  /**
	   * Action to respond to a GET from the /last URL -- this is the end of the process!
	  */	  	  	  
      def last = Action {
      	  Logger.info("in last with doModal marked as " + doModal)
    	  if (doModal == 1) { 
    		  /* if we want a modal form replace, then trigger this */   		  
    		  Ok(views.html.wfLast("Your " + processLabel + " is complete.<br/>This Process has " + processDescription + ".<br/>Thank you!"))
    	  } else {
    		  /* if we want the whole page to refresh, then trigger this */
    		  Ok(views.html.last("Your " + processLabel + " is complete.<br/>This Process has " + processDescription + ".<br/>Thank you!"))
    	  }

      }   
      
      /**
	   * Action to respond to a GET from the /redo URL -- it resets the process engine so that the next call reloads everything!
	  */	  	  	  
      def redo = Action {
      	  processEngine.close
      	 //Logger.info(deploymentObject.getId)
      	  //repositoryService.deleteDeployment(deploymentObject.getId, true)
		  initializeProcessEngine      
		  Redirect(routes.Application.index)	
      }  
      
	  /**
	   * Handle the POST from the final submit in a Process chain
	  */
	  def doneHandle = Action { implicit request =>
	  	  Logger.info("in doneHandle with doModal marked as " + doModal)
	  	  doModal = 0
		  initializeProcessEngine   
		  Redirect(routes.Application.index)	      
	  }	  
	  
	  /**
	   * Handle the POST from the redo button to clear the process context for a rerun
	  */
	  def redoHandle = Action { implicit request =>
		  initializeProcessEngine
		  session.get("trace") match {
			  case Some(x) => lastTrace = x 
			  case None => lastTrace = ""
		  }         
		  Redirect(routes.Application.index)	      
	  }	  	  
	  
	  /**
	   * Action to respond to a GET from the /debug URL
	  */  
	  def debug = Action {
		    Ok(views.html.header(message))  
	  }
	  
	  /**
	   * Request/Response Helper Functions
	  */
	  
	  /**
	   * Given a POST Response.body, decompose into a HashMap structure suitable for submission to an Activiti process
	  */
	  def getVariablesFromBody(body: AnyContent) : java.util.HashMap[String, Object] = {
			  var variables = new java.util.HashMap[String, Object];
			  Logger.info("Response Body " + body.asJson + ".")		  
			  body.asFormUrlEncoded match {
				  case Some(x) => {
					  /** This handles a filled Start Request Form **/
					  //textBody = x
					  /* the line below creates a new map which automatically transforms the Key to remove the data[] and the Value from a List to a single String */
					  val stringFormMap: Map[String, String] = x map { (x:(String, Seq[String])) => (x._1.replace("data[","").replace("]","") , x._2(0))} 	    		    
					  /* pass through the Form results and add each value to the variables HashMap */
					  for(i <- stringFormMap)
						  variables.put(i._1, i._2.asInstanceOf[AnyRef]) 
						  Logger.info("Response Body Received: " + variables.toString)
					  }
				  case None => {
					  /** This handles an empty Start Request Form -- valid because some Starts may not have form fields**/
					  Logger.info("No Response Body Received: Start may not contain any Form Properties.")
				  }
			  }	 
			  variables
	  }
	  	  
	  /**
	   * Activiti Helper Functions
	  */  
	  
	  /**
	   * Init all of the global private variables associated with the basic Activiti processEngine
	   * http://activiti.org/javadocs/org/activiti/engine/package-frame.html
	  */	  
	  def initializeProcessEngine { 
		  try {
			  Logger.info("Initializing Activiti ProcessEngine and Services.")
			  processEngine = ProcessEngines.getDefaultProcessEngine()
			  //ProcessEngines.init();
			  repositoryService = processEngine.getRepositoryService()
			  runtimeService = processEngine.getRuntimeService()   
			  taskService = processEngine.getTaskService() 
			  formService = processEngine.getFormService()       
		  } catch {
		  case e: Exception => {
			  Logger.error("Failure: Initializing Activiti ProcessEngine and Services.")			    
			  Logger.error(e.getMessage())
			  return
		  } 
		  } finally {

		  }
		  Logger.info("Success: Initializing Activiti ProcessEngine and Services.")
	  }
	  
	  /**
	   * Deploy the process from the file/path set in the global private variables
	   * http://www.activiti.org/userguide/index.html#api.services.deployment
	  */		  
	  def deployProcess { 
		var inputStream: java.io.FileInputStream = null  
	  	try {
	  		//processMap = Map("onboard" -> ("PointOnboard","PointOnboardingWizard.bpmn"))
	  		val myDeployment = repositoryService.createDeployment()
	  			.enableDuplicateFiltering
	  			.name("Activiti1")
			for (p <- processMap) {
			  Logger.info("Deploying process " + p._2._1 + " from " + p._2._2 + ".")
			  /* try to open the Process File and deploy it*/
			  inputStream = new FileInputStream(processPath + p._2._2)
			  myDeployment.addInputStream(p._2._2, inputStream)  //the name of the resource matters: must be the filename?
			  //myDeployment.addClasspathResource(p._2._2)
			}	
	  		myDeployment.deploy()	
			/*
			 * 
			  Logger.info("Deploying process from " + processFile + ".")
			  /* try to open the Process File and deploy it*/
			  inputStream = new FileInputStream(processPath)
			  repositoryService.createDeployment()
				  .addInputStream(processFile, inputStream)  //the name of the resource matters: must be the filename?
				  .deploy()		
			* 
			*/		
		  } catch {
		  case e1: IOException => {
				  Logger.error("File I/O Failure: Deploying process from " + processPath + ".")
				  Logger.error(e1.getMessage() + "<br/>" + e1.getStackTraceString)
				  return
			  }
		  case e2: ActivitiException => {
				  Logger.error("Activiti Failure: Deploying process from " + processFile + ".")
				  Logger.error(e2.getMessage() + "<br/>" + e2.getStackTraceString)
				  return
			  }
		  case e: Exception => {
				  Logger.error("Failure: Deploying process from " + processFile + ".")
				  Logger.error(e.getMessage() + "<br/>" + e.getStackTraceString)
				  return
			  }
		  } finally {
			  	if (inputStream !=null) inputStream.close
		  }
		  Logger.info("Success: Deploying process from " + processFile + ".")
		}
	  
	  /**
	   * Start a processInstance from the ProcessId set in the global private variable processName
	   * http://www.activiti.org/userguide/index.html#api.services.start.processinstance
	  */	  
	  def startProcess (variables:java.util.HashMap[String, Object]) : org.activiti.engine.runtime.ProcessInstance = {
			  Logger.info("Initializing Activiti Process " + processName + ".")
			  /* start a process instance and pass the values supplied as the starting parameters */
			  processInstance = runtimeService.startProcessInstanceByKey(processName, variables)
			  Logger.info("Success: Initializing Activiti Process " + processName + ".")
			  //message += "<br/>Task Data Begin: " + variables.toString
			  processInstance
	  }

	  /**
	   * Start a processInstance from the ProcessId set in the global private variable processName
	   * http://www.activiti.org/userguide/index.html#api.services.start.processinstance
	  */	  
	  def completeTask (variables:java.util.HashMap[String, Object]) : org.activiti.engine.task.Task = {
			  Logger.info("Completing Activiti Task " + currentTask.getId + ":" + currentTask.getName + " from Process with Name: " + processName + ".")
			  /* complete a task by supplying parameters */
			  taskService.complete(currentTask.getId, variables);  
			  Logger.info("Success: Completing Activiti Task " + currentTask.getId + ":" + currentTask.getName + " from Process with Name: " + processName + ".")
			  currentTask
	  }
	  
	  /**
	   * Get the current Task's FormData if there is an outstanding Task to perform
	   * http://www.activiti.org/userguide/index.html#formProperties
	  */	  
	  def getCurrentTaskFormData : Option[org.activiti.engine.form.TaskFormData] = {
			  try {
				  if ((taskService.createTaskQuery.processInstanceId(processInstance.getId).count) > 0) {
					  /* still more tasks to be done */
					  Logger.info("The Process with ID " + processInstance.getId + " has more Tasks outstanding.")
					  currentTask = processEngine.getTaskService.createTaskQuery()
						  .processInstanceId(processInstance.getId)
						  .singleResult()
					  Logger.info("Getting formdata from Task Name: " + currentTask.getName + " with Task Id: " + currentTask.getId + ".")
					  //Some(formService.getTaskFormData(currentTask.getId))
					  if (formService.getTaskFormData(currentTask.getId).getFormProperties.size > 0) {
						  	Some(formService.getTaskFormData(currentTask.getId))					  	
					  } else {
					  		Logger.info("The Current Task " + currentTask.getName + " with Task Id: " + currentTask.getId + " has no form properties.")
					  		None					  	
					  }				
				  } else {
					  Logger.info("The Process with ID " + processInstance.getId + " has no Tasks outstanding.")
					  None
				  }			  	
			  } catch {
			  		case e: Exception => {
				  		Logger.info("The TaskService find no outstanding Tasks for Process with ID " + processInstance.getId + ".")
				  		None
			  		}
			  }
	  }
	  
	 /**
	 * Helper for index Action -- assists in debugging an exception generating bpmn definition
	 */
	  def traceContent = {
		  if (lastTrace != "") {
			  if (lastTrace == newTrace) {
				  //Logger.info("here1")
				  "Trace Repeated<br/>" + newTrace
			  } else {
				  //Logger.info("here2")
				  "Trace Changed<br/>New Trace:<br/>" + newTrace + "<br/>Old Trace:<br/>" + lastTrace			        	  
			  }        	
		  } else {
			  //Logger.info("here3")
			  "Trace New<br/>" + newTrace
		  }
	  }
}