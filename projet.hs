import Parser
-----------------------------------------------------------XML
type XmlAttribut = (String,String)
type XmlNom = String 
type XmlText= String
type XmlElement= (XmlNom, [XmlAttribut]) 

data XmlTree = 
   Feuille XmlText
 | Noeud XmlElement [XmlTree] deriving Show
------------------------------------------------------------path
type Element=String
type Condition= XmlAttribut
data Conditions= 
      Con Condition
  |   Cons Condition [Conditions] deriving Show
data ElementConditions= 
     El Element
  |  EL2 Element [Conditions] deriving Show
data Path= 
     ElCond ElementConditions
  |  ElcondP ElementConditions Path deriving Show
data XPathQuery = 
     XPQ Path   deriving Show
----------------------------------------------------------------------------------parseur
openBalise = parserChars "<"
closedBalise = parserChars ">"
separator = parserChars "/"
separator2 = parserChars "="
blanc = parserChars " \t\n"
skipBlancs = zeroOuPlus blanc    
------------------------------------------------------------------- affichege de xmltree
showXmlNom  :: XmlNom-> String
showXmlNom nom = nom

showXmlText :: XmlText-> String
showXmlText text = text

showXmlAttribut ::XmlAttribut-> String 
showXmlAttribut (attributeName, attributeValue) = 
   showXmlNom attributeName ++ "=\"" ++  attributeValue ++ "\"" 

showXmlListeAttribut:: [XmlAttribut] ->String
showXmlListeAttribut [] = ""
showXmlListeAttribut (attr:listAttrib) = 
  showXmlAttribut attr ++ " " ++ (showXmlListeAttribut listAttrib)
           
showXmlElement :: XmlElement -> String
showXmlElement (nom,listAttributs) = 
  "<" ++ showXmlNom nom ++ "  " ++ showXmlListeAttribut listAttributs ++ "> " 
  
showXmlEndElement (nom,listAttributs) = "</" ++ showXmlNom nom ++ ">" ++ "\n"


showXmlTreeList:: [XmlTree]->String
showXmlTreeList [] = ""
showXmlTreeList (tree:listeTree) = showXmlTree tree++"  "++(showXmlTreeList listeTree)


 
showXmlTree ::XmlTree ->String
showXmlTree (Feuille a)= showXmlText a
showXmlTree (Noeud element sons)  = showXmlElement element ++showXmlTreeList sons++ showXmlEndElement element




----------------------------------------------------------------------------------------parseur


readXml1= do
    text<-caractere
    if (text/= '<')&&(text /='>')&&(text /='/') then return text else fail""

readXml2= do
    text<-caractere
    if text/= '>' &&(text /='/')&&(text/=' ')then return text else fail""

readXml3= do
    text<-caractere
    if (text/='=') then return text else fail""

readXmlbalise= do
    text<-caractere
    if (text/='<')&&(text/='/') then return text else fail""

--readXmlAttribut=do
--if text/=' ' then return text else fail""




readXmlAtt= do
    text<-caractere
    if text/= ' ' then return text else fail""
              
lireTousText1:: Parser String
lireTousText1= unOuPlus readXml1 

lireTousText2:: Parser String
lireTousText2= zeroOuPlus readXml2

lireTousText3:: Parser String
lireTousText3= zeroOuPlus readXml3

lireTousreadXmlbalise:: Parser String
lireTousreadXmlbalise= unOuPlus readXmlbalise

readXmlText1= do
           skipBlancs
           txt<-lireTousText1
           skipBlancs
           return txt

readXmlText2= do
           skipBlancs
           txt<- lireTousText2
           skipBlancs
           return txt

readXmlText3= do
           skipBlancs
           txt<- lireTousText3
           separator2
           skipBlancs
           return txt

{--readXmlATT= do
           skipBlancs
           txt<-lireTousATT
           skipBlancs
           return txt--}
readClose=do
          skipBlancs
          txt<-lireTousreadXmlbalise
          skipBlancs
          return txt    

parseXmlTree::Parser XmlTree 
parseXmlTree=do
               skipBlancs
               txt<-readXmlText1
               skipBlancs
               return (Feuille txt)
              |||
              do
                skipBlancs
                openBalise
                skipBlancs
                txt<-readXmlText2
                skipBlancs
                nomattributs<-readXmlText3
                skipBlancs
                valattributs<-readXmlText2
                skipBlancs
                closedBalise
                skipBlancs
                txt2<- parseXmlTree
                skipBlancs
                openBalise
                skipBlancs
                separator
                txt3<-readClose
				    --if txt3 = txt then 
                return(Noeud (txt,[(nomattributs,valattributs)]) [(txt2)]) 
                ---else fail""

              |||
              do
                skipBlancs
                openBalise
                skipBlancs
                txt<-readXmlText2
                skipBlancs
                closedBalise
                skipBlancs
                txt2<- parseXmlTree
                skipBlancs
                openBalise
                skipBlancs
                separator
                txt3<-readClose
				    --if txt3 = txt then 
                return(Noeud (txt,[("","")]) [(txt2)]) 
                ---else fail""

------------------------------------------------------path
readCondition=do
               txt<-caractere
               if (txt/='@')&& (txt/= ']') && (txt/='[') then return txt else fail ""
   

lireConditions:: Parser String
lireConditions= zeroOuPlus readCondition


parseXpathQuery :: Parser XPathQuery
parseXpathQuery=do
                --skipBlancs
                txt<-lireConditions
                --skipBlancs
                return (XPQ )
                 




      
          

        
        

--parseXmlTree str= if elem 'f' str then Just(str) else Nothing


     
       

        



-----------------affichier juste feuille (Feuille "hgh")
--test =(Noeud ("book",[("aaa","bbb")]) [(Feuille "abc")])
--test2=(Noeud ("book",[("aaa","bbb")]) [Noeud ("book2",[("aaa","bbb")]) []])

---concat[showXmlNom x++"  " | x<-text]
		



