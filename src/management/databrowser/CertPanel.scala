package management.databrowser

import java.awt.Dimension
import java.io._
import java.security.cert.{Certificate, CertificateFactory, X509Certificate}
import java.security.{KeyPair, KeyStore}

import client.dataviewer.ViewConstants
import javax.swing.JOptionPane
import org.bouncycastle.pkcs.PKCS10CertificationRequest
import org.shredzone.acme4j.challenge.{Challenge, Http01Challenge}
import org.shredzone.acme4j.exception.{AcmeConflictException, AcmeException, AcmeRetryAfterException}
import server.config.FSPaths
import server.webserver.WebServer
import util.{CollUtils, Log}
import org.shredzone.acme4j._
import org.shredzone.acme4j.util.{CSRBuilder, CertificateUtils, KeyPairUtils}

import scala.collection.JavaConverters._
import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Button, Orientation, ScrollPane, Swing, TextArea}
import scala.util.control.NonFatal


/**
 * Created by Peter Holzer   on 02.03.2017 .
 */
class CertPanel extends BoxPanel(Orientation.Vertical) {
  //val CA_STAGING_URL = "https://acme-staging.api.letsencrypt.org/acme"
  xLayoutAlignment = 0
  val AGREEMENT_URL = "https://letsencrypt.org/documents/LE-SA-v1.1.1-August-1-2016.pdf"
  contents += ViewConstants.label("Zertifikats-Management") += Swing.VStrut(20) += new FormatLine(220, "Domain", FSPaths.certDomain _, FSPaths.setCertDomain) +=
    new FormatLine(220, "Root Folder for Challenge", FSPaths.certRootFolder _, FSPaths.setCertRootFolder) +=
    new FormatLine(220, "ACME request certificate url", FSPaths.certAcmeURL _, FSPaths.setCertAcmeURL) +=
    new FormatLine(220, "Email", FSPaths.certEmail _, FSPaths.setCertEmail)


  val sendButton = new Button("Autorisieren und Zertifikat anfordern mit Challenge")
  val updateButton = new Button("Update Registrationsdaten")
  val readyButton = new Button("Server nach Challenge zurücksetzen")
  val infoButton = new Button("Zertifikat Information")
  val renewButton = new Button("Zertifikat erneuern")
  //val keyStoreButton=new Button("Keystore")
  val infoLabel = new TextArea()
  val scroller = new ScrollPane() {
    viewportView = infoLabel
    preferredSize = new Dimension(600, 500)
  }

  val userKeyFile = new File(FSPaths.configDir + "cryptuser.key")
  val domainKeyFile = new File(FSPaths.configDir + "cryptdomain.key")
  val domainCSRFile = new File(FSPaths.configDir + "cryptuser.csr")
  val domainChainFile = new File(FSPaths.configDir + "cryptchain.crt")

  var tokenFile: Option[File] = None


  contents += Swing.VStrut(20)
  contents += sendButton += readyButton += updateButton += infoButton += renewButton += Swing.VGlue += scroller

  listenTo(sendButton, readyButton, updateButton, infoButton, renewButton)
  reactions += {

    case ButtonClicked(`sendButton`) => doCert()
    case ButtonClicked(`readyButton`) =>
      for (t <- tokenFile) t.delete()
      WebServer.switchToProductionMode()
    case ButtonClicked(`updateButton`) => updateRegistration()
    case ButtonClicked(`infoButton`) => loadInfo()
    case ButtonClicked(`renewButton`) => renewCertificate()
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

  def findOrRegisterAccount(sess: Session): Registration = {
    try {
      Log.w("Session:"+sess)
      val regBuilder = new RegistrationBuilder()
      regBuilder.addContact("mailto:" + FSPaths.certEmail)
      val reg = regBuilder.create(sess)
      Log.w("Register a new User:" + reg.getLocation)
      val agreement = reg.getAgreement
      reg.modify().setAgreement(agreement).commit()
      Log.w("TOS:\n" + agreement)
      reg
    }
    catch {
      case ex: AcmeConflictException =>
        Log.e("Account already exists:" + ex.getLocation)
        Registration.bind(sess, ex.getLocation)
    }
  }


  def authorize(reg: Registration, domain: String): Unit = {
    val auth = reg.authorizeDomain(domain)
    Log.w("Authorization for " + domain)
    val challenge = httpChallenge(auth, domain)
    if (challenge.getStatus != Status.VALID) {
      Log.w("challenge triggered")
      infoLabel.text="challenge triggered"
      challenge.trigger()
      try {
        var attempts = 10
        while (challenge.getStatus != Status.VALID && attempts > 0) {
          if (challenge.getStatus == Status.INVALID) throw new AcmeException("Challenge failed")
          else Log.w("Challenge status:" + challenge.getStatus())
          Swing.onEDT(infoLabel.text="challenge attempts "+attempts)
          Thread.sleep(2000L)
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
      } catch {case ex: InterruptedException => Log.e("Challenge interrupted", ex)}
    } else println("Challenge already valid")
    if (challenge.getStatus == Status.INVALID) throw new AcmeException("Challenge failed final")
  }


  def httpChallenge(auth: Authorization, domain: String): Challenge = {
    val challenge: Http01Challenge = auth.findChallenge(Http01Challenge.TYPE)
    if (challenge == null) throw new AcmeException("Found no " + Http01Challenge.TYPE + " challenge, don't know what to do...")
    val tFile = new File(FSPaths.certRootFolder + "/.well-known/acme-challenge/" + challenge.getToken())
    CollUtils.tryWith(new FileWriter(tFile)) { f => f.write(challenge.getAuthorization) }
    tokenFile = Some(tFile)
    challenge
  }


  protected def requestCertificate(reg:Registration,csr: PKCS10CertificationRequest):Unit = try {
    val certificate = reg.requestCertificate(csr.getEncoded())
    Log.w("Certificate generated:" + certificate.getLocation)
    val cert = certificate.download()
    if(cert!=null) {
      Log.w("Certificate valid until:" + cert.getNotAfter)
      infoLabel.text = "new Certificate valid until " + cert.getNotAfter
      val chain = certificate.downloadChain()
      CollUtils.tryWith(new FileWriter(domainChainFile)) { d =>
        CertificateUtils.writeX509CertificateChain(d, cert, chain: _*)
      }
      Log.w("Certificate chain written " + chain.mkString("\n"))
      saveInKeyStore()
    } else Log.e("Cert == null")
  } catch {case NonFatal(e)=> Log.e("requestCertificatge",e)}


  def doCert(): Unit = try {

    WebServer.switchToCertMode()
    val userKeyPair = createOrLoadKeys(userKeyFile)
    val domainKeyPair = createOrLoadKeys(domainKeyFile)
    infoLabel.text = "do Cert "+userKeyPair.getPublic.toString
    val session = new Session(FSPaths.certAcmeURL, userKeyPair)

    val reg: Registration = findOrRegisterAccount(session)
    println("Reg: status:"+reg.getStatus+"\nauthorisations:"+reg.getAuthorizations.asScala.mkString(" | ")+
      "\ncertificates:"+reg.getCertificates.asScala.mkString(" | "))
    authorize(reg, FSPaths.certDomain)
    val csrBuilder = new CSRBuilder()
    csrBuilder.addDomain(FSPaths.certDomain)
    csrBuilder.sign(domainKeyPair)
    CollUtils.tryWith(new FileWriter(domainCSRFile)) { c =>
      csrBuilder.write(c)
    }
    val csr=csrBuilder.getCSR
    Log.w("CSR:"+csr)
    requestCertificate(reg,csr)
  } catch {
    case NonFatal(e)=> Log.e("do cert",e)
  }

  def updateRegistration(): Unit = try{
    val userKeyPair = createOrLoadKeys(userKeyFile)
    val reg = findOrRegisterAccount(new Session(FSPaths.certAcmeURL, userKeyPair))
    val agreement = reg.getAgreement
    Log.w("Agreement:" + agreement)
    reg.modify().addContact("mailto:" + FSPaths.certEmail).setAgreement(agreement).commit()
  } catch {
    case NonFatal(e)=> Log.e("updateRegistration",e)
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

  def renewCertificate(): Unit = {
    WebServer.switchToCertMode()
    val userKeyPair = createOrLoadKeys(userKeyFile)
    Log.w("Keypair:"+userKeyPair)
    val reg = findOrRegisterAccount(new Session(FSPaths.certAcmeURL, userKeyPair))
    Log.w("Registration:"+reg.getContacts.asScala.mkString(","))
    if (domainCSRFile.exists) {
      CollUtils.tryWith(new FileInputStream(domainCSRFile)) { df =>
        val csr: PKCS10CertificationRequest = CertificateUtils.readCSR(df)
        Log.w("CSR:"+csr)
        requestCertificate(reg,csr)
      }
    } else{
      Log.e("Zertifikatdatei "+domainCSRFile+" nicht gefunden")
      infoLabel.text = "no certfile found "+domainCSRFile
    }
    saveInKeyStore()
    WebServer.switchToProductionMode()
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
          keyStore.setKeyEntry("1", domainKeyPair.getPrivate(), pwCA, certs.asInstanceOf[Array[Certificate]])
          CollUtils.tryWith(new FileOutputStream(ksf)) { fos =>
            keyStore.store(fos, pwCA)
            fos.close()
          }
        }
    }
  }

}

