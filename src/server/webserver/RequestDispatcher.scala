package server.webserver

import java.io.PrintWriter
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.handler.AbstractHandler
import server.comm.UserInfo
import scala.collection.mutable.ArrayBuffer


/**
 * Created by Kathi on 09.03.2015.
 */


/*object RequestDispatcher extends AbstractHandler {
  type RequestCheck=(String, UserInfo, PrintWriter) => Option[String]
  val queryString="Q"
  val TEXTPLAIN="text/plain; charset=utf-8"
  val HTML="text/html; charset=utf-8"
  val JSON="application/JSON; charset=utf-8"

  val checkerList=ArrayBuffer[RequestCheck]()
  val topHTML=  """<!DOCTYPE html><html><head><meta charset="utf-8"/><script src="startup.js" charset="utf-8"></script ><link rel="stylesheet" href="main.css"/><link href='http://fonts.googleapis.com/css?family=Source+Sans+Pro:400italic,400' rel='stylesheet' type='text/css'/></head><body onload="onLoad()"> """
  val firstEndHTML=   """<div id="cont"><div id="header"></div><div id="prDivsGroup"></div></div></body></html>"""
  val loadmiddleHTML= """<div id="cont"><div id="header"></div><div id="prDivsGroup"></div></div><script>var loadRef="""" ;
  val loadEndHTML="""";</script><script src="startup.js" charset="utf-8"/></body></html>"""


  def handle(target: String, baseRequest: Request, request: HttpServletRequest,
             response: HttpServletResponse) = {
    val out = response.getWriter()
    //response.setCharacterEncoding("utf-8")
    //request.setCharacterEncoding("utf-8")
    baseRequest.setHandled(true)
    println("handle target:" + target + " sessionID:" + request.getSession.getId() + " user:" + request.getUserPrincipal)
    DBLoginService.getUser(request.getUserPrincipal().getName()) match {
      case Some(user) => checkerList.iterator.map(func => func(target, user, out)).collectFirst { case Some(x) => x} match {
        case Some(ctntType) => response.setContentType(ctntType)
        case None => baseRequest.setHandled(false) //response.setContentType(TEXTPLAIN); out.println("unknown target:" + target)
      }
      case None => out.println("unknown user " + request.getUserPrincipal())
    }

  }

  checkerList+= ((target,user,out)=>
    if(target.startsWith("/admin")) {
      out.println("<h1>Admin console for " + target.drop(6) + "</h1>")
      Some(RequestDispatcher.HTML)
    }
    else None )


  checkerList+= ((target,user,out)=> if(target=="/A"){
    out.println(topHTML+firstEndHTML)
    Some(RequestDispatcher.HTML)
  } else None)



}*/

