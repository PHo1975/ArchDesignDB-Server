package server.webserver


import javax.servlet.DispatcherType
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import management.databrowser.ConsolePanel
import org.eclipse.jetty.security.authentication.DigestAuthenticator
import org.eclipse.jetty.security.{ConstraintMapping, ConstraintSecurityHandler}
import org.eclipse.jetty.server.handler.ErrorHandler
import org.eclipse.jetty.server.session.DefaultSessionIdManager
import org.eclipse.jetty.server.{Request, Server}
import org.eclipse.jetty.servlet._
import org.eclipse.jetty.util.log.{Log, Logger}
import org.eclipse.jetty.util.security.Constraint
import server.config.FSPaths
import util.{Log => MyLog}

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

/**
 * Created by Kathi on 09.03.2015.
 */
object WebServer extends Server() {
  val securityHandler = new ConstraintSecurityHandler()
  lazy val dummySecurityHandler = new ConstraintSecurityHandler()
  val digestAuthenticator = new DigestAuthenticator()
  val loginService: DBLoginService.type = DBLoginService
  loginService.setName("db.holzer-architektur.de")
  val servletContextHandler = new ServletContextHandler(ServletContextHandler.SESSIONS)
  val safeServlet = new ServletHolder("safe", classOf[DefaultServlet])
  var longCaching=true

  def setup(): Unit = {


    val idManager = new DefaultSessionIdManager(this)

    addBean(loginService)
    val mapping = new FilterMapping()
    mapping.setFilterName("Security Filter")
    mapping.setPathSpecs(Array("/*", "/files/*", "/events"))
    mapping.setServletNames(Array("mein"))
    mapping.setDispatcherTypes(java.util.EnumSet.of(DispatcherType.INCLUDE, DispatcherType.REQUEST))
    val fholder = new FilterHolder(classOf[ContentSecurityFilter])
    fholder.setName("Security Filter")
    if (management.databrowser.MainWindow.webSocketSSL)
      servletContextHandler.getServletHandler.addFilter(fholder, mapping)
    //servletContextHandler.addFilter(classOf[ContentSecurityFilter],"/*",null   )
    setHandler(servletContextHandler)

    val userConstraint = new Constraint()
    userConstraint.setName("user_auth")
    userConstraint.setAuthenticate(true)
    userConstraint.setRoles(Array("guest","user", "admin"))
    val userConstraintMapping = new ConstraintMapping()
    userConstraintMapping.setPathSpec("/*")
    userConstraintMapping.setConstraint(userConstraint)
    userConstraint.setDataConstraint(Constraint.DC_CONFIDENTIAL)
    userConstraintMapping.setMethodOmissions(Array[String]("TRACE","TRACK","CUSTOM","DELETE","HEAD","CONNECT"))

    val traceConstraint = new Constraint()
    traceConstraint.setName("Disable TRACE")
    traceConstraint.setAuthenticate(true)
    val traceMapping = new ConstraintMapping()
    traceMapping.setConstraint(traceConstraint)
    traceMapping.setMethod("TRACE")
    traceMapping.setPathSpec("/")

    if (management.databrowser.MainWindow.webSocketSSL) {
      securityHandler.setConstraintMappings(Array(traceMapping,userConstraintMapping ))
      securityHandler.setAuthenticator(digestAuthenticator)
      securityHandler.setLoginService(loginService)
    }
    setSessionIdManager(idManager)
    //sessionManager.setSessionCookie("JSESSIONID_PetersServer")
    val servletHolder=new ServletHolder("mein",classOf[DBServlet])
    servletContextHandler.addServlet(servletHolder,"/events")
    servletContextHandler.setContextPath("/")

    val fileServlet=new ServletHolder("files",classOf[DefaultServlet])
    fileServlet.setInitParameter("dirAllowed","true")
    fileServlet.setInitParameter("gzip","true")
    fileServlet.setInitParameter("resourceBase",FSPaths.deployDir+"files\\")
    fileServlet.setInitParameter("useFileMappedBuffer","false")
    fileServlet.setInitParameter("pathInfoOnly","true")
    if(longCaching)
    fileServlet.setInitParameter("cacheControl","max-age=360000,public")
    servletContextHandler.addServlet(fileServlet,"/files/*")

    safeServlet.setInitParameter("dirAllowed", "true")
    safeServlet.setInitParameter("gzip", "true")
    safeServlet.setInitParameter("resourceBase", FSPaths.deployDir + "safe\\")
    safeServlet.setInitParameter("useFileMappedBuffer", "false")
    safeServlet.setInitParameter("pathInfoOnly", "true")

    val errorHandler = new ErrorHandler() {
      override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
        MyLog.i("Webserver request Error Status:" +response.getStatus+" uri:"+request.getRequestURI+" port:"+baseRequest.getLocalPort+" remote:"+baseRequest.getRemoteAddr+
          " \nFields:"+baseRequest.getHttpFields.iterator().asScala.mkString("| ")+"   method:"+request.getMethod+" content-Type:"+baseRequest.getContentType)
        response.getWriter.append("{\"status\":\"ERROR\",\"message\":\"HTTP ").append(
          response.getStatus.toString).append("\"}")
        ContentSecurityFilter.changeHeaders(response)
      }
    }
    addBean(errorHandler)
    servletContextHandler.setErrorHandler(errorHandler)
    servletContextHandler.setSecurityHandler(securityHandler)
  }


  def switchToCertMode(): Unit = try {
    stop()
    removeBean(loginService)
    servletContextHandler.setSecurityHandler(dummySecurityHandler)
    servletContextHandler.getServletHandler.setFilters(Array[FilterHolder]())

    safeServlet.setEnabled(true)
    servletContextHandler.addServlet(safeServlet, "/*")
    start()
  } catch {
    case NonFatal(e)=> util.Log.e("switch to cert",e)
  }

  def switchToProductionMode(): Unit = try {
    stop()
    addBean(loginService)
    servletContextHandler.removeBean(safeServlet)
    servletContextHandler.setSecurityHandler(securityHandler)
    start()
  } catch {
    case NonFatal(e)=> util.Log.e("switch to production",e)
  }

  def setLogConsole(console:ConsolePanel): Unit = {
    Log.setLog(new MyLogger)
  }
}

class MyLogger () extends Logger{
  override def getName: String = "BaseLogger"

  override def debug(s: String, objects: AnyRef*): Unit = {}

  override def debug(s: String, l: Long): Unit ={}

  override def debug(throwable: Throwable): Unit = {}

  override def debug(s: String, throwable: Throwable): Unit = {}

  override def getLogger(s: String): Logger = this

  override def setDebugEnabled(b: Boolean): Unit = {}

  override def warn(s: String, objects: AnyRef*): Unit = util.Log.w("Webserver:"+s+" "+objects.mkString(", "))

  override def warn(throwable: Throwable): Unit = util.Log.w("Webserver:"+throwable)

  override def warn(s: String, throwable: Throwable): Unit = util.Log.e("Webserver:"+s,throwable)

  override def ignore(throwable: Throwable): Unit = println(throwable)

  override def isDebugEnabled: Boolean = false

  override def info(s: String, objects: AnyRef*): Unit = util.Log.i("Webserver:"+s+" "+objects.mkString(", "))

  override def info(throwable: Throwable): Unit = util.Log.i("Webserver:"+throwable)

  override def info(s: String, throwable: Throwable): Unit = util.Log.i("Webserver:"+s)
}

