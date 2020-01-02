package client.dataviewer.export

import java.io.File
import java.util.Date

import client.comm.ClientQueryManager
import client.dataviewer.InstanceSelection
import definition.data.InstanceData
import definition.expression.DateConstant
import definition.typ.DataType
import util.{JavaUtils, Log}

import scala.xml.{Elem, NodeBuffer, XML}

/**
 * Created by Kathi on 07.02.2017.
 */
trait ExportModule {
  def export(sel:InstanceSelection):java.util.ArrayList[File]
}


object GAEB83Module extends ExportModule {

  def unitReplace=Map(("Stück","Stck"),("Meter","m"),("pausch","psch"))

  def stripLength(st:String, length: Int): String = if(st.length<=length  ) st else st.substring(0,length)

  def splitText(text:String): Array[NodeBuffer] =
    for(part<-text.split('\n')) yield <span>{part}</span><br/>


  def export(sel:InstanceSelection):java.util.ArrayList[File]={
    sel.selection.headOption match {
      case Some(rootRef) =>
        val rootInst=ClientQueryManager.queryInstance(rootRef,-1).head
        val bidDate=rootInst.fieldData(4).getValue
        ClientQueryManager.getNextParentOfType(rootRef,202) match {
          case Some(parent)=>
            var currID=0
            val addressParent=ClientQueryManager.queryInstance(parent.ref,0).head
            val projectInfo=ClientQueryManager.queryInstance(addressParent.ref,0).head
            val ownerInfo=ClientQueryManager.queryInstance(addressParent.ref,1).head
            val vorbemerkungen=ClientQueryManager.queryInstance(rootRef,3)
            val tempDir=new File(System.getProperty("java.io.tmpdir"))
            val today=new Date()
            val rootName=rootInst.fieldValue(2).toString
            val fileName=tempDir.getAbsolutePath()+File.separatorChar+ rootInst.toString.replace(" ","_")+"_"+
              util.JavaUtils.shortDateFormat.format(today)+".x83"
            val gaebFile=new File(fileName)

            def getNextID={currID+=1;"A"+currID}

            def createPosition(data:InstanceData)= {
              val id=getNextID
              val pos=f"${
                data.fieldData(1).getValue.toInt match {
                  case 0 => currID
                  case o => o
                }
              }%03d"
              val text=data.fieldValue(5).toString
              val menge=data.fieldValue(3).toDouble
              val einheit=data.fieldData(3).getValue.toUnitNumber.unitFraction.toString
              val posTyp=data.fieldValue(2).toInt
              <Item ID={id.toString} RNoPart={pos}>
                {if(posTyp==2) <Provis>WithoutTotal</Provis>}
                <Qty>{f"$menge%1.3f".replace(',','.')}</Qty>
                <QU>{unitReplace.getOrElse(einheit,stripLength(einheit,4))}</QU>
                <Description><CompleteText>
                  <DetailTxt>
                    <Text>{splitText(text)}</Text>
                  </DetailTxt>
                  <OutlineText><OutlTxt><TextOutlTxt><span>{stripLength(text,70)}</span></TextOutlTxt></OutlTxt></OutlineText>
                </CompleteText></Description>
              </Item>
            }

            def loopLV()={
              val children=ClientQueryManager.queryInstance(rootRef,2).groupBy(_.ref.typ)
              if(children.contains(133)) {
                <BoQCtgy ID={getNextID} RNoPart="01">
                  <LblTx><span>{rootName}</span></LblTx>
                  <BoQBody>
                    <BoQCtgy ID={getNextID} RNoPart="01">
                      <LblTx><span></span></LblTx>
                      <BoQBody><Itemlist>
                        {for(pos<-children(133)) yield createPosition(pos)}
                      </Itemlist></BoQBody>
                    </BoQCtgy>
                  </BoQBody>
                </BoQCtgy>
              }
              else if(children.contains(135)) for (t<-children(135)) yield loopTitel(1,t)
            }

            def loopTitel(level:Int,titelData:InstanceData):Elem={
              val oz=f"${
                titelData.fieldData(1).getValue.toInt match {
                  case 0 => currID
                  case o => o
                }
              }%02d"

              <BoQCtgy ID={getNextID} RNoPart={oz}>
                <LblTx><span>{titelData.fieldValue(2).toString}</span></LblTx>
                <BoQBody>
                {
                  val children=ClientQueryManager.queryInstance(titelData.ref,1).groupBy(_.ref.typ)
                  if (children.contains(133)){
                    if(level==1)
                    <BoQCtgy ID={getNextID} RNoPart="01">
                      <LblTx><span></span></LblTx>
                      <BoQBody><Itemlist>
                        {for(pos<-children(133)) yield createPosition(pos)}
                      </Itemlist></BoQBody>
                    </BoQCtgy>
                    else <Itemlist>
                      {for(pos<-children(133)) yield createPosition(pos)}
                    </Itemlist>
                  } else if(children.contains(135)) for(t<-children(135)) yield loopTitel(level+1,t)

                }
                </BoQBody>
              </BoQCtgy>
            }

            def createVorbemerkungen: Elem ={
              <AddText>
                <OutlineAddText>
                  <span>{stripLength(vorbemerkungen.head.fieldValue(0).toString,60)}</span>
                </OutlineAddText>
              <DetailAddText>
                { for (ordner<-vorbemerkungen) yield {
                    <p style="font-weight:bold;"><span>{ordner.fieldValue(0).toString}</span><br/></p>
                      <p>{splitText(ordner.fieldValue(1).toString)}</p>
                      <p></p>
                  }
                }</DetailAddText>
              </AddText>
            }

            if(gaebFile.exists)gaebFile.delete()
            //val outBuffer=new PrintWriter(new BufferedWriter(new FileWriter(gaebFile)))

            val prInfoXML= <PrjInfo>
            <NamePrj>{stripLength(ownerInfo.fieldValue(1).toString,20)}</NamePrj>
              <LblPrj>{stripLength(projectInfo.fieldValue(0).toString+" "+projectInfo.fieldValue(1).toString+"- "+
                projectInfo.fieldValue(2).toString+", "+projectInfo.fieldValue(3).toString+" "+projectInfo.fieldValue(4).toString ,60)}</LblPrj>
              <Cur>EUR</Cur>
              <CurLbl>Euro</CurLbl>
              <BidCommPerm>Yes</BidCommPerm>
              <AlterBidPerm>Yes</AlterBidPerm>
            </PrjInfo>
            val gaeb= <GAEB xmlns="http://www.gaeb.de/GAEB_DA_XML/DA83/3.2">
              <GAEBInfo>
                <Version>3.2</Version>
                <VersDate>2013-10</VersDate>
                <Date>{util.JavaUtils.GAEBDateFormat.format(today)}</Date>
                <Time>{util.JavaUtils.shortTimeFormat.format(today)}</Time>
                <ProgSystem>Holzer Scalabase</ProgSystem>
                <ProgName>Scalabase</ProgName>
              </GAEBInfo>
              {prInfoXML}
              <Award>
                <DP>83</DP>
                <AwardInfo>
                  <Cat>OpenCall</Cat>
                  <Cur>EUR</Cur>
                  <CurLbl>Euro</CurLbl>
                  <OpenDate>{util.JavaUtils.GAEBDateFormat.format(today)}</OpenDate>
                  <OpenTime>{util.JavaUtils.shortTimeFormat.format(today)}</OpenTime>
                  <EvalEnd>{util.JavaUtils.GAEBDateFormat.format(JavaUtils.toJavaDate(
                    if(bidDate.getType!=DataType.DateTyp) DateConstant().addDays(14) else
                    rootInst.fieldData(4).getValue.toDate)) }</EvalEnd>
                  <AcceptType>Förmliche Abnahme</AcceptType>
                  <WarrDur>2</WarrDur>
                  <WarrUnit>Years</WarrUnit>
                </AwardInfo>
                <OWN>
                  <Address>
                    <Name1>{ownerInfo.fieldValue(0).toString+" "+ownerInfo.fieldValue(1).toString}</Name1>
                    <Street>{ownerInfo.fieldValue(2).toString}</Street>
                    <PCode>{ownerInfo.fieldValue(3).toString}</PCode>
                    <City>{ownerInfo.fieldValue(4).toString}</City>
                  </Address>
                  <DPNo>{"D-"+rootRef.instance.toString}</DPNo>
                  <AwardNo>{"V-"+rootRef.instance.toString}</AwardNo>
                </OWN>
                {if(vorbemerkungen.nonEmpty)createVorbemerkungen}
                <BoQ ID="AO">
                  <BoQInfo>
                    <Name>{stripLength(rootInst.fieldValue(2).toString,20)}</Name>
                    <LblBoQ>{ownerInfo.fieldValue(1).toString+"-"+rootInst.fieldValue(2).toString}</LblBoQ>
                    <Date>{util.JavaUtils.GAEBDateFormat.format(today)}</Date>
                    <OutlCompl>AllTxt</OutlCompl>
                    <BoQBkdn>
                      <Type>BoQLevel</Type>
                      <LblBoQBkdn>Titel</LblBoQBkdn>
                      <Length>2</Length>
                      <Num>Yes</Num>
                      <Alignment>right</Alignment>
                    </BoQBkdn>
                    <BoQBkdn>
                      <Type>BoQLevel</Type>
                      <LblBoQBkdn>Titel</LblBoQBkdn>
                      <Length>2</Length>
                      <Num>Yes</Num>
                      <Alignment>right</Alignment>
                    </BoQBkdn>
                    <BoQBkdn>
                      <Type>Item</Type>
                      <LblBoQBkdn>Position</LblBoQBkdn>
                      <Length>3</Length>
                      <Num>Yes</Num>
                      <Alignment>right</Alignment>
                    </BoQBkdn>
                  </BoQInfo>
                  <BoQBody>
                    {loopLV()}
                  </BoQBody>
                </BoQ>
              </Award>
            </GAEB>
            XML.save(gaebFile.getPath,gaeb,"UTF-8",true)
            Log.w("Export "+sel.selection.mkString(","))

            val result =new java.util.ArrayList[File]()
            result.add(gaebFile)
            result
          case _ => Log.e("project not found for "+rootRef);null
        }

      case other => Log.e("no rootref "+other+" sel:"+sel.selection.mkString(","));null
    }

  }
}
