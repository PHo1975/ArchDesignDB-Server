package server.webserver

import javax.servlet._
import javax.servlet.http.HttpServletResponse

/**
 * Created by Peter Holzer on 21.03.2017 .
 */

object ContentSecurityFilter {
  var allowedDomains = List("fonts.googleapis.com", "fonts.gstatic.com")
  var allowedStyleDomains = List("fonts.googleapis.com")

  def changeHeaders(hr: HttpServletResponse): Unit = {
    val contentSecurityPolicy = "default-src 'none' " +
      " ; connect-src 'self' wss://db.holzer-architektur.de; script-src 'self' " + allowedDomains.mkString(" ") +
      "; style-src 'self' " +
      allowedStyleDomains.mkString(" ") + " ; img-src 'self'; font-src " + allowedDomains.mkString(" ") + " ;"
    addHeader(hr, "Content-Security-Policy", contentSecurityPolicy)
    addHeader(hr, "Referrer-Policy", "no-referrer")
    addHeader(hr, "X-Frame-Options", "SAMEORIGIN")
    addHeader(hr, "X-Xss-Protection", "1; mode=block")
    addHeader(hr, "X-Content-Type-Options", "nosniff")
    addHeader(hr, "Public-Key-Pins", "pin-sha256=\"M++BlLndycWWali+q2Hz/3apXEdG5djQ72ZfVj8MKqo=\";" +
      "pin-sha256=\"2T3GN5ve+IZ32Xx0n788Kr2gFnpDQjIPD67uxdo5GoE=\"; max-age=2592000; includeSubdomains;")
  }

  def addHeader(hr: HttpServletResponse, header: String, value: String): Unit =
    if (!hr.containsHeader(header)) hr.addHeader(header, value)
}

class ContentSecurityFilter extends Filter {

  import server.webserver.ContentSecurityFilter._

  override def init(filterConfig: FilterConfig): Unit = {
  }

  override def doFilter(servletRequest: ServletRequest, servletResponse: ServletResponse, filterChain: FilterChain): Unit = {
    servletResponse match {
      case hr: HttpServletResponse =>
        changeHeaders(hr)
        filterChain.doFilter(servletRequest, hr)
      case _ =>
    }
  }

  override def destroy(): Unit = {}
}
