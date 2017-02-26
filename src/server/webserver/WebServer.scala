package server.webserver

import management.databrowser.ConsolePanel
import org.eclipse.jetty.security.authentication.DigestAuthenticator
import org.eclipse.jetty.security.{ConstraintMapping, ConstraintSecurityHandler}
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.session.{HashSessionIdManager, HashSessionManager, SessionHandler}
import org.eclipse.jetty.servlet.{DefaultServlet, ServletContextHandler, ServletHolder}
import org.eclipse.jetty.util.log.{Log, StdErrLog}
import org.eclipse.jetty.util.security.Constraint
import server.config.FSPaths

/**
 * Created by Kathi on 09.03.2015.
 */
object WebServer extends Server(8080) {
  /*val ste=new StdErrLog()
  Log.setLog(ste)
  ste.setDebugEnabled(true)*/


  def setup(): Unit = {
    System.setProperty("org.eclipse.jetty.servlet.LEVEL","DEBUG")
    val idManager = new HashSessionIdManager()
    val loginService =  DBLoginService
    addBean(loginService)

    val securityHandler = new ConstraintSecurityHandler()
    setHandler(securityHandler)

    val userConstraint = new Constraint()
    userConstraint.setName("user_auth")
    userConstraint.setAuthenticate(true)
    userConstraint.setRoles(Array("guest","user", "admin"))
    val userConstraintMapping = new ConstraintMapping()
    userConstraintMapping.setPathSpec("/*")
    userConstraintMapping.setConstraint(userConstraint)
    val adminConstraint = new Constraint()
    adminConstraint.setName("admin_auth")
    adminConstraint.setAuthenticate(true)
    adminConstraint.setRoles(Array("admin"))
    val adminConstraintMapping = new ConstraintMapping()
    adminConstraintMapping.setPathSpec("/admin/*")
    adminConstraintMapping.setConstraint(adminConstraint)
    securityHandler.setConstraintMappings(Array(userConstraintMapping/*, adminConstraintMapping*/))

    securityHandler.setAuthenticator(new DigestAuthenticator())

    securityHandler.setLoginService(loginService)

    setSessionIdManager(idManager)

    val sessionManager = new HashSessionManager()
    sessionManager.setSessionCookie("JSESSIONID_PetersServer")

    val sessionHandler = new SessionHandler(sessionManager)

    val servletContextHandler = new ServletContextHandler(ServletContextHandler.SESSIONS)
    servletContextHandler.setContextPath("/")

    val servletHolder=new ServletHolder("mein",classOf[DBServlet])
    servletContextHandler.addServlet(servletHolder,"/events")
    val fileServlet=new ServletHolder("files",classOf[DefaultServlet])
    fileServlet.setInitParameter("dirAllowed","true")
    fileServlet.setInitParameter("gzip","true")
    fileServlet.setInitParameter("resourceBase",FSPaths.deployDir+"files\\")
    fileServlet.setInitParameter("useFileMappedBuffer","false")
    fileServlet.setInitParameter("pathInfoOnly","true")
    servletContextHandler.addServlet(fileServlet,"/files/*")


    val mainServlet=new ServletHolder("welco",classOf[DefaultServlet])
    mainServlet.setInitParameter("resourceBase",FSPaths.deployDir+"welcome\\")
    mainServlet.setInitParameter("dirAllowed","true")
    mainServlet.setInitParameter("useFileMappedBuffer","false")
    mainServlet.setInitParameter("gzip","true")
    servletContextHandler.addServlet(mainServlet,"/")

    //setHandler(servletContextHandler)
    sessionHandler.setHandler(servletContextHandler)
    securityHandler.setHandler(sessionHandler)
  }

  def setLogConsole(console:ConsolePanel): Unit = {
    println("set log console "+console)
    Log.getLog match {
      case l:StdErrLog=>l.setStdErrStream(console.out)
      import scala.collection.JavaConverters._
      for(alog<-Log.getLoggers.asScala.valuesIterator) alog match {
         case slog:StdErrLog=> slog.setStdErrStream(console.out)
         case _=>
       }
      case _=>
    }
  }
}

