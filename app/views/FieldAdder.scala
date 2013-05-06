/**
 *
 */
package views
import views.html.helper._

import org.activiti.engine.form.FormProperty

import play.Logger
import play.api.templates._
import play.api.templates.PlayMagicForJava

import java.util.List
import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.JavaConversions._
/**
 * @author tim
 *
 */
object FieldAdder {

	def textInput(p: org.activiti.engine.form.FormProperty, dynForm: play.data.DynamicForm) : play.data.DynamicForm = {
		Logger.info("in textInput")
		views.html.helper.inputText(play.api.templates.PlayMagicForJava.javaFieldtoScalaField(dynForm(p.getId)), '_label -> p.getName, 'id -> p.getId, 'value -> p.getValue(), if(!p.isWritable()) 'readonly -> "true" else 'optional -> "true", if(p.isRequired()) 'required -> "true" else 'optional -> "true", '_error -> "something wrong here", '_showerrors -> true, '_showConstraints -> false);
		dynForm
	}

	def smartInput(p: org.activiti.engine.form.FormProperty, dynForm: play.data.DynamicForm) : Html = {
			Logger.info("Found Field " + p.getName + " of Type " + p.getType.getName)
			if(p.isWritable()||p.isReadable()) { 
				p.getType.getName match {
					case "string" => { 
						//'_help -> "Custom help", 
						if (p.getId contains "password") {
							views.html.helper.inputPassword(play.api.templates.PlayMagicForJava.javaFieldtoScalaField(dynForm(p.getId)), '_label -> p.getName, 'id -> p.getId, 'value -> p.getValue(), if(!p.isWritable()) 'readonly -> "true" else 'optional -> "true", if(p.isRequired()) 'required -> "true" else 'optional -> "true", '_error -> "something wrong here", '_showerrors -> true, '_showConstraints -> false);							
						} else {
							views.html.helper.inputText(play.api.templates.PlayMagicForJava.javaFieldtoScalaField(dynForm(p.getId)), '_label -> p.getName, 'id -> p.getId, 'value -> p.getValue(), if(!p.isWritable()) 'readonly -> "true" else 'optional -> "true", if(p.isRequired()) 'required -> "true" else 'optional -> "true", '_error -> "something wrong here", '_showerrors -> true, '_showConstraints -> false);							
						}
					}				
					case "enum" => { 
						var myOptions: scala.collection.mutable.Map[String,String] = scala.collection.JavaConversions.mapAsScalaMap(p.getType.getInformation("values").asInstanceOf[java.util.Map[String, String]])
						var myOptionsSeq: Seq[(String, String)] = myOptions.toSeq
						Logger.info(p.getType.getInformation("values").getClass.getName)
						views.html.helper.select(play.api.templates.PlayMagicForJava.javaFieldtoScalaField(dynForm(p.getId)), myOptionsSeq,  '_label -> p.getName, 'id -> p.getId, 'value -> p.getValue(), if(!p.isWritable()) 'readonly -> "true" else 'optional -> "true", if(p.isRequired()) 'required -> "true" else 'optional -> "true", '_error -> "something wrong here", '_showerrors -> true, '_showConstraints -> false);
				    }			 
				}
			} else {
				Html("")
			}
	}
	
	def smartInput(Id: String, Name: String, Value: Any, Type: String, isWriteable: Boolean, isRequired: Boolean, dynForm: play.data.DynamicForm) : Html = {
			Logger.info("Found Field " + Name + " of Type " + Type)
				Type match {
					case "string" => { 
						//'_help -> "Custom help", 
						if (Id.toUpperCase contains "PASSWORD") {
							views.html.helper.inputPassword(play.api.templates.PlayMagicForJava.javaFieldtoScalaField(dynForm(Id)), '_label -> Name, 'id -> Id, 'value -> Value, if(!isWriteable) 'readonly -> "true" else 'optional -> "true", if(isRequired) 'required -> "true" else 'optional -> "true", '_error -> "something wrong here", '_showerrors -> true, '_showConstraints -> false);							
						} else {
							views.html.helper.inputText(play.api.templates.PlayMagicForJava.javaFieldtoScalaField(dynForm(Id)), '_label -> Name, 'id -> Id, 'value -> Value, if(!isWriteable) 'readonly -> "true" else 'optional -> "true", if(isRequired) 'required -> "true" else 'optional -> "true", '_error -> "something wrong here", '_showerrors -> true, '_showConstraints -> false);							
						}
					}				
					case "enum" => { 
						views.html.helper.select(play.api.templates.PlayMagicForJava.javaFieldtoScalaField(dynForm(Id)), Value.asInstanceOf[Seq[(String, String)]],  '_label -> Name, 'id -> Id, 'value -> Value, if(!isWriteable) 'readonly -> "true" else 'optional -> "true", if(isRequired) 'required -> "true" else 'optional -> "true", '_error -> "something wrong here", '_showerrors -> true, '_showConstraints -> false);
				    }		
					case "checkbox" => { 
						views.html.helper.checkbox(play.api.templates.PlayMagicForJava.javaFieldtoScalaField(dynForm(Id)), '_label -> Name, 'id -> Id,  if(!isWriteable) 'readonly -> "true" else 'optional -> "true", if(isRequired) 'required -> "true" else 'optional -> "true", '_error -> "something wrong here", '_showerrors -> true, '_showConstraints -> false);
				    }
					case "radiogrp" => { 
						views.html.helper.inputRadioGroup(play.api.templates.PlayMagicForJava.javaFieldtoScalaField(dynForm(Id)), Value.asInstanceOf[Seq[(String, String)]], '_label -> Name, 'id -> Id, 'value -> Value, if(!isWriteable) 'readonly -> "true" else 'optional -> "true", if(isRequired) 'required -> "true" else 'optional -> "true", '_error -> "something wrong here", '_showerrors -> true, '_showConstraints -> false);
				    }
				}
		
	}
	
	/*
	 * 
	 * Smart Input entrypoint for CloudPointe dynamic forms constructed from JSON array of field descriptors
	 * (0 SITEARGID: Int, 1 SITETYPEID: Int, 2 LABEL: String, 3 INPUTTYPE: String, 4 FIELDNAME: String, 
	 * 5 TOOLTIP: String, 6 REQUIRED: Boolean, 7 ENABLED: Boolean, 8 ORDERINDEX: Int, 9 PASSWORDFIELD: Boolean)		    
	 * 
	 */
	def smartInputCPJsonArray(processVariables: scala.collection.mutable.Map[String,java.lang.Object], userForm: play.data.DynamicForm) : Html = {
		    var r:Seq[play.api.libs.json.JsValue] = null
		    var s:play.api.libs.json.JsValue = null
		    var HtmlStream: Html = Html("")
		    var fieldType: String = ""
			for (i <- processVariables("cp_str_provider_cred").asInstanceOf[scala.collection.immutable.$colon$colon[Any]].toList) {
				r = i.asInstanceOf[play.api.libs.json.JsArray].as[Seq[play.api.libs.json.JsValue]]
				r(3).as[String] match {
					case "TextInput" => fieldType = "string"
					case "CheckBox" => fieldType = "checkbox"
					case _ => fieldType = "string"
				}
				HtmlStream += smartInput(r(4).as[String], r(2).as[String], "", fieldType, (if (r(7).as[Int] == 1) true else false), (if (r(6).as[Int] == 1) true else false), userForm)
			}
		    HtmlStream
	}
	
}