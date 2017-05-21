package server.webserver

import java.util
import javax.servlet.DispatcherType
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import management.databrowser.ConsolePanel
import org.eclipse.jetty.security.authentication.DigestAuthenticator
import org.eclipse.jetty.security.{ConstraintMapping, ConstraintSecurityHandler}
import org.eclipse.jetty.server.session.HashSessionIdManager
import org.eclipse.jetty.server.{Request, Server}
import org.eclipse.jetty.servlet._
import org.eclipse.jetty.util.log.{Log, StdErrLog}
import org.eclipse.jetty.util.security.Constraint
import server.config.FSPaths

/**
 * Created by Kathi on 09.03.2015.
 */
object WebServer extends Server() {
  val securityHandler = new ConstraintSecurityHandler()
  lazy val dummySecurityHandler = new ConstraintSecurityHandler()
  val digestAuthenticator = new DigestAuthenticator()
  val loginService = DBLoginService
  loginService.setName("db.holzer-architektur.de")
  val servletContextHandler = new ServletContextHandler(ServletContextHandler.SESSIONS)
  val safeServlet = new ServletHolder("safe", classOf[DefaultServlet])
  //val sessionManager = new HashSessionManager()
  //val sessionHandler = new SessionHandler(sessionManager)

  def setup(): Unit = {
    System.setProperty("org.eclipse.jetty.servlet.LEVEL","DEBUG")

    val idManager = new HashSessionIdManager()

    addBean(loginService)
    val mapping = new FilterMapping()
    mapping.setFilterName("Security Filter")
    mapping.setPathSpecs(Array("/*", "/files/*", "/events"))
    mapping.setServletNames(Array("mein"))
    mapping.setDispatcherTypes(util.EnumSet.of(DispatcherType.INCLUDE, DispatcherType.REQUEST))
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

    val traceConstraint = new Constraint()
    traceConstraint.setName("Disable TRACE")
    traceConstraint.setAuthenticate(true)
    val traceMapping = new ConstraintMapping()
    traceMapping.setConstraint(traceConstraint)
    traceMapping.setMethod("TRACE")
    traceMapping.setPathSpec("/")


    securityHandler.setConstraintMappings(Array(userConstraintMapping, traceMapping))
    securityHandler.setAuthenticator(digestAuthenticator)
    securityHandler.setLoginService(loginService)
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
    servletContextHandler.addServlet(fileServlet,"/files/*")

    safeServlet.setInitParameter("dirAllowed", "false")
    safeServlet.setInitParameter("gzip", "true")
    safeServlet.setInitParameter("resourceBase", FSPaths.deployDir + "safe\\")
    safeServlet.setInitParameter("useFileMappedBuffer", "false")
    safeServlet.setInitParameter("pathInfoOnly", "true")

    val errorHandler = new ErrorPageErrorHandler() {
      override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse): Unit = {
        response.getWriter().append("{\"status\":\"ERROR\",\"message\":\"HTTP ").append(
          response.getStatus().toString).append("\"}")
        ContentSecurityFilter.changeHeaders(response)
      }
    }
    addBean(errorHandler)

    servletContextHandler.setErrorHandler(errorHandler)

    //sessionHandler.setHandler(servletContextHandler)
    servletContextHandler.setSecurityHandler(securityHandler)
    //securityHandler.setHandler(sessionHandler)
  }

  def switchToCertMode(): Unit = {
    stop()
    removeBean(loginService)
    servletContextHandler.setSecurityHandler(dummySecurityHandler)

    safeServlet.setEnabled(true)
    servletContextHandler.addServlet(safeServlet, "/*")
    start()
  }

  def switchToProductionMode(): Unit = {
    stop()
    addBean(loginService)
    servletContextHandler.removeBean(safeServlet)
    servletContextHandler.setSecurityHandler(securityHandler)
    start()
  }

  def setLogConsole(console:ConsolePanel): Unit = {
    Log.getLog match {
      case l:StdErrLog=>l.setStdErrStream(console.out)
      import scala.collection.JavaConverters._
      for(alog<-Log.getLoggers.asScala.valuesIterator) alog match {
         case slog:StdErrLog=> slog.setStdErrStream(console.out)
         case o => println("logger:" + o)
       }
      case _=>
    }
  }
}

