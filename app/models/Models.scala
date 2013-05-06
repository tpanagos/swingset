package models

import java.util.{Date}

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class User(id: Int, name: String, email: String, password: String, password2: String)
case class Task(id: String, vacationApproved: Option[String])

case class Company(id: Pk[Long] = NotAssigned, name: String)
case class Computer(id: Pk[Long] = NotAssigned, name: String, introduced: Option[Date], discontinued: Option[Date], companyId: Option[Long])

case class SiteCredentials(SITEARGID: Int, SITETYPEID: Int, LABEL: String, INPUTTYPE: String, FIELDNAME: String, TOOLTIP: String, REQUIRED: Boolean, ENABLED: Boolean, ORDERINDEX: Int, PASSWORDFIELD: Boolean)


