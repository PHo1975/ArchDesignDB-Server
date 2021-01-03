package management.databrowser

import client.ui.ViewConstants
import org.shredzone.acme4j
import org.shredzone.acme4j.challenge.{Challenge, Http01Challenge}
import org.shredzone.acme4j.exception.{AcmeException, AcmeRetryAfterException}
import server.config.FSPaths
import server.webserver.WebServer
import util.{CollUtils, Log}
import org.shredzone.acme4j._
import org.shredzone.acme4j.util.{CSRBuilder, KeyPairUtils}

import java.awt.Dimension
import java.io._
import java.net.URL
import java.security.cert.{Certificate, CertificateFactory, X509Certificate}
import java.security.{KeyPair, KeyStore}
import javax.swing.JOptionPane
import scala.collection.mutable
import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal


/**
 * Created by Peter Holzer   on 02.03.2017 .
 */
class CertPanel extends BoxPanel(Orientation.Vertical) {
  //val CA_STAGING_URL = "https://acme-staging.api.letsencrypt.org/acme"
  xLayoutAlignment = 0
  //val AGREEMENT_URL = "https://letsencrypt.org/documents/LE-SA-v1.1.1-August-1-2016.pdf"
  contents += ViewConstants.label("Zertifikats-Management") += Swing.VStrut(20) += new FormatLine(220, "Domain",()=> FSPaths.certDomain, FSPaths.setCertDomain) +=
    new FormatLine(220, "Root Folder for Challenge",()=> FSPaths.certRootFolder, FSPaths.setCertRootFolder) +=
    new FormatLine(220, "ACME request certificate url", ()=>FSPaths.certAcmeURL , FSPaths.setCertAcmeURL) +=
    new FormatLine(220, "Email", ()=>FSPaths.certEmail , FSPaths.setCertEmail)


  val sendButton = new Button("Autorisieren und Zertifikat anfordern mit Challenge")
  //val updateButton = new Button("Update Registrationsdaten")
  val readyButton = new Button("Server nach Challenge zurücksetzen")
  val infoButton = new Button("Zertifikat Information")
  //val renewButton = new Button("Zertifikat erneuern")
  //val keyStoreButton=new Button("Keystore")
  val infoLabel = new TextArea()
  val scroller:ScrollPane = new ScrollPane() {
    viewportView = infoLabel
    preferredSize = new Dimension(600, 500)
  }

  val userKeyFile = new File(FSPaths.configDir + "cryptuser.key")
  val domainKeyFile = new File(FSPaths.configDir + "cryptdomain.key")
  val domainCSRFile = new File(FSPaths.configDir + "cryptuser.csr")
  val domainChainFile = new File(FSPaths.configDir + "cryptchain.crt")

  var tokenFile: Option[File] = None


  contents += Swing.VStrut(20)
  contents += sendButton += readyButton +=  infoButton += Swing.VGlue += scroller

  listenTo(sendButton, readyButton, infoButton )
  reactions += {

    case ButtonClicked(`sendButton`) => doCert()
    case ButtonClicked(`readyButton`) =>
      for (t <- tokenFile) t.delete()
      WebServer.switchToProductionMode()

    case ButtonClicked(`infoButton`) => loadInfo()

  }

  def createOrLoadKeys(file: File): KeyPair = {
    if (file.exists()) {
      CollUtils.tryWith(new FileReader(file)) { r => KeyPairUtils.readKeyPair(r) } match {
        case Some(keyPair) => keyPair
        case _ => throw new IllegalArgumentException("Cant load Cryptkey " + file.getName)
      }

    } else {
      Log.e("Zertifikatdatei "+file+" existiert nicht, wird neu angelegt")
      val keyPair = KeyPairUtils.createKeyPair(2048)
      CollUtils.tryWith(new FileWriter(file)) {
        f => KeyPairUtils.writeKeyPair(keyPair, f)
      }
      keyPair
    }
  }

  def findOrRegisterAccount(sess: Session): Account = {
      Log.w("Session:"+sess)
      val userKeyPair=createOrLoadKeys(userKeyFile)
      val accountURLFile=new File(FSPaths.configDir+"acmeAccountURL.txt")
      if (accountURLFile.exists()){
        val af=Source.fromFile(accountURLFile)
        val aURL=af.mkString
        Log.w("Account File exists:"+aURL)
        af.close()
        val login=sess.login(new URL(aURL),userKeyPair)
        login.getAccount
      } else {
        val account = new AccountBuilder().addContact("mailto:" + FSPaths.certEmail).agreeToTermsOfService().
          useKeyPair(userKeyPair).create(sess)
        val aURL: URL = account.getLocation
        Log.w("Create Account " + aURL)
        CollUtils.tryWith(new PrintWriter(accountURLFile)) {
          f => f.write(aURL.toString)
        }
        account
      }
  }


  def doCert(): Unit = try {
    WebServer.switchToCertMode()
    infoLabel.text = "do Cert "
    val session = new Session(FSPaths.certAcmeURL)
    val account: Account = findOrRegisterAccount(session)
    val order=account.newOrder().domains(FSPaths.certDomain).create()
    val authorizations: mutable.Seq[Authorization] =order.getAuthorizations.asScala
    println("Reg: status:"+account.getStatus+"\nauthorisations:"+authorizations.mkString(" | "))
    for(auth<-authorizations;if auth.getStatus!=Status.VALID){
      authorize(auth)
    }
    val cert=getCertificate(order)
    val certInst=cert.getCertificate
    Log.w("Certificate valid until:" + certInst.getNotAfter)
    infoLabel.text = "new Certificate valid until " + certInst.getNotAfter
    CollUtils.tryWith(new FileWriter(domainChainFile)) { d =>
      cert.writeCertificate(d)
    }
    saveInKeyStore()
    Log.w("Certificate written")
  } catch {
    case a:AcmeException=> Log.e("error when doing cert:"+a.getCause+" "+a.getMessage+"\n",a)
    case NonFatal(e)=> Log.e("do cert",e)
  }

  def prepareChallenge(auth: Authorization): Challenge = {
    val challenge: Http01Challenge = auth.findChallenge(Http01Challenge.TYPE)
    if (challenge == null) throw new AcmeException("Found no " + Http01Challenge.TYPE + " challenge, don't know what to do...")
    var folder=FSPaths.certRootFolder
    if (folder.last=='/') folder=folder.dropRight(1)
    val tFile = new File(folder + "/.well-known/acme-challenge/" + challenge.getToken)
    Log.w("Token:"+tFile)
    CollUtils.tryWith(new FileWriter(tFile)) { f => f.write(challenge.getAuthorization) }
    tokenFile = Some(tFile)
    challenge
  }

  def authorize(auth: Authorization): Unit = if(FSPaths.certRootFolder.length>0){
    val challenge= prepareChallenge(auth)
    val domain=auth.getIdentifier.getDomain
    Log.w("authorize "+domain)

    if (challenge.getStatus != Status.VALID) {
      Log.w("challenge triggered")
      infoLabel.text="challenge triggered"
      challenge.trigger()
      try {
        var attempts = 10
        while (challenge.getStatus != Status.VALID && attempts > 0) {
          if (challenge.getStatus == Status.INVALID) throw new AcmeException("Challenge got invalid")
          else Log.w("Challenge status:" + challenge.getStatus)
          Swing.onEDT(infoLabel.text="challenge attempts "+attempts)
          Thread.sleep(3000L)
          try {
            challenge.update()
          } catch {
            case re: AcmeRetryAfterException =>
              val when = re.getRetryAfter
              val now = new java.util.Date()
              println("retry at:" + when)
              Thread.sleep(2000L)
          }
          attempts -= 1
        }
      } catch {
        case ex: InterruptedException => Log.e("Challenge interrupted", ex)
        //case NonFatal(o)=>Log.e("Error when Challenging:"+o)
      }
    } else println("Challenge already valid")
    if (challenge.getStatus == Status.INVALID) throw new AcmeException("Challenge failed final")
  } else Log.e("Try to authorize but no folder")


  def getCertificate(order:Order): acme4j.Certificate ={
    val domainKeyPair = createOrLoadKeys(domainKeyFile)
    val csrBuilder = new CSRBuilder()
    csrBuilder.addDomain(FSPaths.certDomain)
    csrBuilder.sign(domainKeyPair)
    CollUtils.tryWith(new FileWriter(domainCSRFile)) { c =>
      csrBuilder.write(c)
    }
    val csr=csrBuilder.getEncoded
    order.execute(csr)
    var attempts=10
    try {
      while(order.getStatus!=Status.VALID && attempts>0){
        if(order.getStatus==Status.INVALID) throw new AcmeException("Order got invalid")
        else Log.w("Order status "+order.getStatus)
        Swing.onEDT(infoLabel.text="order attempts "+attempts)
        Thread.sleep(3000L)
        try{
          order.update()
        }
        catch {
          case re:AcmeRetryAfterException=>
            println("retry at "+re.getRetryAfter)
            Thread.sleep(3000L)
        }
        attempts  -= 1
      }
    } catch {case ex: InterruptedException => Log.e("order interrupted", ex)}
    if(order.getStatus==Status.INVALID) throw new AcmeException("Order failed final")
    order.getCertificate
  }


  def loadInfo(): Unit = {
    if (domainChainFile.exists) {
      val certs = loadCertChain()
      val text = certs.map {
        case x509: X509Certificate => "Certifikat " +
          x509.getIssuerDN.getName + "\nSubject:" + x509.getSubjectDN.getName + "\n notAfter:" + x509.getNotAfter +
          "\nusage:" + x509.getExtendedKeyUsage + " SigAlgName " + x509.getSigAlgName + " bc:" + x509.getBasicConstraints +
          "\n base64:" + x509.getPublicKey.getEncoded + " format:" + x509.getPublicKey.getFormat + " algo:" + x509.getPublicKey.getAlgorithm +
          "\n enc:" + x509.getPublicKey.getEncoded.map(_.toString).mkString("|")
        case o => "Certificate type:" + o.getType + "\n " + o.toString
      }.mkString("\n\n--------------------------------\n")
      println(text)
      infoLabel.text = text

    } else infoLabel.text = "no certfile found"
  }


  private def loadCertChain(): Array[Certificate] = if (domainChainFile.exists)
    CollUtils.tryWith(new FileInputStream(domainChainFile)) { df =>
      val certificateFactory = CertificateFactory.getInstance("X.509")
      certificateFactory.generateCertificates(df).asScala.toArray[java.security.cert.Certificate]
    } match {
      case Some(ar) => ar;
      case None => Array.empty[Certificate]
    } else Array.empty[Certificate]


  def saveInKeyStore(): Unit = {
    val keyStore = KeyStore.getInstance(KeyStore.getDefaultType)
    JOptionPane.showInputDialog(peer, "Keystore Passwort:", "In Keystore schreiben", JOptionPane.QUESTION_MESSAGE) match {
      case null =>
      case password: String =>
        val pwCA = password.toCharArray
        val ksf = FSPaths.keyStoreFile
        val fis = if (ksf.exists) new FileInputStream(ksf) else null
        try {
          keyStore.load(fis, pwCA)
        } finally {
          if (fis != null) fis.close()
        }
        val domainKeyPair = createOrLoadKeys(domainKeyFile)
        val certs = loadCertChain()
        if (certs.length > 1) {
          keyStore.setKeyEntry("1", domainKeyPair.getPrivate, pwCA, certs.asInstanceOf[Array[Certificate]])
          CollUtils.tryWith(new FileOutputStream(ksf)) { fos =>
            keyStore.store(fos, pwCA)
            fos.close()
          }
        }
    }
  }

}

