#######################################################################################
################ Preprocesing dat a explora�n� anal�za ################################
############### Ad�la Vrtkov�, Martina Litschmannov� ##################################
#######################################################################################

##  M�me data, a co d�l?
#   1. Spust�me pot�ebn� bal��ky, kter� obsahuj� dal�� statistick� funkce
#   2. Nastav�me pracovn� adres��, odkud importujeme data, pop�. kam chceme ukl�dat v�stupy
#   3. Importujeme data (z pracovn�ho adres��e, z internetu)
#   4. Pre-processing -> a) Pod�v�me se na data
#                        b) ulo��me si data ve v�ce form�tech (ka�d� funkce m� "rad�ji" jin� form�t)
#   5. Anal�za kvalitativn�ch prom�nn�ch
#   6. Anal�za kvantitativn�ch prom�nn�ch
#   7. Identifikace a rozhodnut� o vylou�en�/ponech�n� odlehl�ch pozorov�n�

#######################################################################################
## 1. Jak nainstalovat a spustit roz�i�uj�c� bal��ek funkc�? ##########################

# Instalov�n� bal��ku
install.packages("openxlsx") 

# Na�ten� bal��ku (nutno opakovat p�i ka�d�m nov�m spu�t�n� Rka, vhodn� m�t na za��tku skriptu)
library(openxlsx)

#######################################################################################
## 2. Kde se ukl�daj� generovan� v�stupy, nastaven� pracovn�ho adres��e ###############

# V�pis pracovn�ho adres��e
getwd()

# Nastaven� pracovn�ho adres��e -> do uvozovek, celou cestu
setwd("C:/Users/lit40/.rkward")


#######################################################################################
## 3. Na�ten� datov�ho souboru ########################################################

# z�kladn� funkce - read.table, read.csv, read.csv2, ...
# z�le�� hlavn� na form�tu souboru (.txt, .csv), na tzv. odd�lova�i jednotliv�ch hodnot, desetinn� ��rce/te�ce

# Na�ten� a ulo�en� datov�ho souboru ve form�tu csv2 z lok�ln�ho disku do datov�ho r�mce data
data=read.csv2(file="C:/Martina/STA1/DATA/aku.csv")

# Na�ten� a ulo�en� datov�ho souboru ve form�tu csv2 z internetu do datov�ho r�mce data
data=read.csv2(file="http://am-nas.vsb.cz/lit40/DATA/aku.csv")

# Na�ten� a ulo�en� datov�ho souboru ve form�tu xlsx z lok�ln�ho disku do datov�ho r�mce data - pou��v�me funkci z bal��ku XLConnect, kter� jsme v �vodu rozbalili
install.packages("openxlsx")
library(openxlsx)
data=readWorkbook("C:/Users/lit40/Desktop/aku.xlsx",
             sheet=1,                        # ��slo listu (defaultn� hodnota 1)
             colNames=TRUE,                  # informace, zda v prvn�m ��dku je hlavi�ka s n�zvy sloupc� (defaultn� hodnota TRUE)
             startRow = 4,		     # na kter�m ��dku m� na��t�n� za��t (nen�-li tento parametr zad�n, za��n� na��t�n� na ��dku 1)
             cols = 2:9)   		     # kter� sloupce se maj� na��st (nen�-li tento parametr zad�n, na��taj� se v�echny nepr�zdn� sloupce)
colnames(data)=c("A5","B5","C5","D5","A100","B100","C100","D100")

# nebo

install.packages("XLConnect")
library(XLConnect)
wb=loadWorkbook("C:/Martina/STA1/DATA/aku.xlsx")
data=readWorksheet(wb, sheet="Data", header=TRUE, startRow = 4, startCol=2)
colnames(data)=c("A5","B5","C5","D5","A100","B100","C100","D100")

# Na�ten� a ulo�en� datov�ho souboru ve form�tu xlsx z internetu (pomoc� bal��ku XLConnect) 
# do datov�ho r�mce data (komplikovan�j��, doporu�ujeme rad�ji si st�hnout xlsx soubor na lok�ln� disk)
tmp = tempfile(fileext = ".xlsx")
download.file(url = "http://am-nas.vsb.cz/lit40/DATA/aku.xlsx", destfile = tmp, mode="wb")
wb=loadWorkbook(tmp)
data=readWorksheet(wb,sheet="Data",header=TRUE,startRow = 4,startCol=2)
colnames(data)=c("A5","B5","C5","D5","A100","B100","C100","D100")


#######################################################################################
## 4. Pre-processing dat ##############################################################

# V�pis datov�ho souboru
data
# Zobrazen� prvn�ch �esti ��dk�
head(data)

# Zobrazen� posledn�ch �esti ��dk�
tail(data)

# Zobrazen� 10. ��dku
data[10,]

# Zobrazen� 3. sloupce 
data[,3]
# nebo (v�me-li, jak se jmenuje prom�nn� zaps�na ve 3. sloupci)
data[["C5"]]
# nebo
data$C5  

# Ulo�en� prvn�ho a p�t�ho sloupce dat. r�mce data do dat. r�mce pokus
pokus=data[,c(1,5)]

# Ulo�en� prvn�ch 4 sloupc� dat. r�mce data do dat. r�mce data5
data5=data[,c(1:4)]
#nebo
data5=data[,-c(5:8)]

## Pozn. p�i ukl�d�n� dat mysleme na p�ehlednost v n�zvech, data5 obsahuj� kapacity akumul�tor� v�ech v�robc� po 5ti cyklech

## P�evod dat do standardn�ho datov�ho form�tu
data5S=reshape(data[,1:4],			         # ��st datov�ho r�mce, kter� bude p�ev�d�na do std. datov�ho form�tu
               direction="long",                # parametr ur�uj�c� tzv. "long" nebo "wide" form�t
               varying=c("A5","B5","C5","D5"),	 # n�zvy prom�nn�ch, kter� maj� b�t za�azeny do sloupce hodnot
               v.names="kap5",			             # pojmenov�n� sloupce hodnot
               times=c("A","B","C","D"),		     # varianty prom�nn�, kter� p�i�azuje identifik�tory jednotliv�m hodnot�m prom�nn� kap5
               timevar="vyrobce")		           # pojmenov�n� prom�nn� obsahuj�c� identifik�tory prom�nn� kap5

# p�eveden� prom�nn� data5S$vyrobce na typ factor
data5S$vyrobce=as.factor(data5S$vyrobce)

# odstran�n� nadbyte�n� prom�nn� id z datov�ho r�mce data5S
data5S=data5S[,-3]

# odstran�n� NA z datov�ho r�mce data5S
data5S=na.omit(data5S)

## P�evod p�rov�ch dat do standardn�ho datov�ho form�tu
dataS=reshape(data,
              direction="long",
              varying=list(c("A5","B5","C5","D5"),
                           c("A100","B100","C100","D100")),
              v.names=c("kap5","kap100"),
              times=c("A","B","C","D"),
              timevar="vyrobce")

# p�eveden� prom�nn� dataS$vyrobce na typ factor
dataS$vyrobce=as.factor(dataS$vyrobce)

# odstran�n� nadbyte�n� prom�nn� id z datov�ho r�mce dataS
dataS=dataS[,-4]

# odstran�n� NA z datov�ho r�mce dataS
dataS=na.omit(dataS)

# Definov�n� nov� prom�nn� v st�vaj�c�m datov�m r�mci
dataS$pokles=dataS$kap5-dataS$kap100

## Vytvo�en� samostatn�ch prom�nn�ch
a5=dataS$kap5[dataS$vyrobce=="A"]
b5=dataS$kap5[dataS$vyrobce=="B"]
c5=dataS$kap5[dataS$vyrobce=="C"]
d5=dataS$kap5[dataS$vyrobce=="D"]

a100=dataS$kap100[dataS$vyrobce=="A"]
b100=dataS$kap100[dataS$vyrobce=="B"]
c100=dataS$kap100[dataS$vyrobce=="C"]
d100=dataS$kap100[dataS$vyrobce=="D"]

pokles.a=dataS$pokles[dataS$vyrobce=="A"]
pokles.b=dataS$pokles[dataS$vyrobce=="B"]
pokles.c=dataS$pokles[dataS$vyrobce=="C"]
pokles.d=dataS$pokles[dataS$vyrobce=="D"]

### Pozn�mky pro zopakov�n� principu grafiky v R ######################################
# z�kladem jsou tzv. high-level funkce, kter� vytvo�� graf (tj. otev�ou grafick� oknou a vykresl� dle zadan�ch parametr�)
# na n� navazuj� tzv. low-level funkce, kter� n�co do aktvin�ho grafick�ho okna p�idaj�, samy o sob� neotev�ou nov�
# p�. low-level funkc� - nap�. abline, points, lines, legend, title, axis ... kter� p�idaj� p��mku, body, legendu...
# tzn. p�ed pou�it�m "low-level" funkce je pot�eba, volat "high-level" funkci (nap�. plot, boxplot, hist, barplot, pie,...)

# dal�� grafick� parametry naleznete v n�pov�d�
# nebo nap�. zde http://www.statmethods.net/advgraphs/parameters.html
# nebo zde https://flowingdata.com/2015/03/17/r-cheat-sheet-for-graphical-parameters/
# nebo http://bcb.dfci.harvard.edu/~aedin/courses/BiocDec2011/2.Plotting.pdf

## Barvy v R
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf


#######################################################################################
## 5. Explora�n� anal�za a vizualizace kategori�ln� prom�nn� ##########################

## V�po�et �etost� ####################################################################
cetnosti=table(dataS$vyrobce) 
cetnosti # v�pis

# V�po�et relativn�ch �etnost� - 2 ekvivalentn� zp�soby
rel.cetnosti=100*cetnosti/sum(cetnosti)  
rel.cetnosti # v�pis

rel.cetnosti2=prop.table(cetnosti)*100

# Zaokrouhlen� relativn�ch �etnost� (%) na 1 desetinn� m�sto
rel.cetnosti=round(rel.cetnosti,digits=1)
rel.cetnosti  # v�pis

# Pozor na zaokrouhlovac� chybu!!
rel.cetnosti[4]=100-sum(rel.cetnosti[1:3])
rel.cetnosti   # v�pis

# Slou�en� �etnost� a relativn�ch �etnost� do tabulky �etnost�
tabulka=cbind(cetnosti,rel.cetnosti)    # cbind() ... slou�en� sloupc�
tabulka   # v�pis

# P�ejmenov�n� n�zv� sloupc� v tabulce �etnost�
colnames(tabulka)=c("�etnost","rel.�etnost (%)")
tabulka

# Ulo�en� tabulky do csv souboru pro export do MS Excel
write.csv2(tabulka,file="tabulka.csv")

# Kde je tabulka ulo�ena?
getwd()


## V�se�ov� (kol��ov�) graf - angl. piechart ##########################################
cetnosti=table(dataS$vyrobce)  # v tuto chv�li nen� nutno pou��vat, �etnosti jsme ji� spo�etli a ulo�ili do prom�nn� cetnosti v��e
pie(cetnosti)

# Zabarven� grafu
pie(cetnosti,
    col=c("red","green","yellow","blue"))
pie(cetnosti,
    col=heat.colors(4))

# P�id�n� n�zvu grafu a popisk�
pie(cetnosti,
    col=heat.colors(4),
    main="Zastoupen� v�robc� ve v�b�ru",
    labels=c("V�robce A","V�robce B","V�robce C","V�robce D"))

pie(cetnosti,
    col=heat.colors(4),
    main="Zastoupen� v�robc� ve v�b�ru",
    labels=paste("V�robce",names(cetnosti),"\n",cetnosti)) # funkce paste() umo��uje slou�it textov� �et�zce a hodnoty prom�nn�ch, symbol "\n" tvo�� nov� ��dek v textu

# rel. �etnosti byly spo�teny a ulo�eny do prom�nn� rel.cetnosti v��e
pie(cetnosti,
    col=heat.colors(4),
    main="Zastoupen� v�robc� ve v�b�ru",
    labels=paste("V�robce",names(cetnosti),"\n",cetnosti,";",rel.cetnosti,"%"))

# Pro z�jemce - bal��ek plotrix a funkce pie3D vytvo�� 3D kol��ov� graf


## Sloupcov� graf - angl. barplot #####################################################
cetnosti=table(data5S$vyrobce)  # v tuto chv�li nen� nutno pou��vat, �etnosti jsme ji� spo�etli a ulo�ili do prom�nn� cetnosti v��e
barplot(cetnosti)

# Zm�na barev, p�id�n� n�zvu
barplot(cetnosti,
        col=heat.colors(4),
        main="Zastoupen� v�robc� ve v�b�ru",
        space=0.6)                             # parametr space vytvo�� mezeru mezi sloupci

# P�id�n� dal��ch popisk� a legendy
barplot(cetnosti,
        col=heat.colors(4),
        horiz=TRUE,                            # horizont�ln� orientace grafu
        border=FALSE,				                   # nevykresluje ��ru kolem sloupe�k�
        main="Zastoupen� v�robc� ve v�b�ru",
        names.arg=paste("V�robce",names(cetnosti)))

legend("topright",
       paste("V�robce",names(cetnosti)),
       col=heat.colors(4),
       fill=heat.colors(4),
       border=FALSE,
       bty="n")

# P�id�n� absolutn�ch a relativn�ch �etnost� k odpov�daj�c�m sloupc�m
bp = barplot(cetnosti,
             col=heat.colors(4),
             main="Zastoupen� v�robc� ve v�b�ru",
             names.arg=paste("V�robce",names(cetnosti)))
text(bp,cetnosti,cetnosti)

bp = barplot(cetnosti,
             col=heat.colors(4),
             main="Zastoupen� v�robc� ve v�b�ru",
             names.arg=paste("V�robce",names(cetnosti)))
text(bp,
     cetnosti,paste(cetnosti,";",rel.cetnosti,"%"),
     pos=1)						# parametr pos ud�v�, kde bude text uveden vzhledem k dan� pozici (1 = pod, 2 = vlevo, 3 = nad, 4 = vpravo) 	

## Jak graf ulo�it? ##################################################################

# Zji�t�n� aktivn�ho okna, nastaven� aktivn�ho okna - tj. kter� graf chceme ulo�it?
dev.cur()
dev.set(2)

# Ulo�en� obr�zku ve form�tu pdf (v��ka a ���ka jsou uvedeny v palc�ch (inch), 1inch=2,54cm)
dev.print(device=pdf,file="barplot.pdf",width=6.5,height=5)

# Zav�en� grafick�ho okna
dev.off()

# Kam se obr�zek ulo�il?
getwd()

#######################################################################################
#####Pie charts are a very bad way of displaying information.##########################
##The eye is good at judging linear measures and bad at judging relative areas.########
##A bar chart or dot chart is a preferable way of displaying this type of data. #######
#######################################################################################

#######################################################################################
## 6. Explora�n� anal�za a vizualizace kvantitativn� prom�nn� #########################

## Popisn� statistika #################################################################
summary(dataS$kap5)

# V�po�et pr�m�ru jedn� prom�nn�
mean(dataS$kap5)
mean(a5)

# Pozor na chyb�j�c� hodnoty
mean(data$C5)
mean(data$C5,na.rm=TRUE)
mean(na.omit(data$C5))

# V�po�et medi�nu jedn� prom�nn�
quantile(dataS$kap5,probs=0.5)
quantile(a5,probs=0.5)

# Ur�en� rozsahu
length(na.omit(dataS$kap5))

#######################################################################################
## funkce, kter� umo�n� aplikaci vybran� funkce na sloupce datov�ho r�mce, na hodnoty vektoru apod.
x=1:4
#     - lapply - na vstup aplikuje zvolenou funkci, vr�t� list (seznam) hodnot
lapply(x,sqrt)

#     - sapply - na vstup aplikuje volenou funkci, vr�t� vektor hodnot
sapply(x,sqrt)

#     - vapply - stejn� jako sapply, akor�t s parametrem nav�c, kter� specifikuje v�stup
vapply(x,sqrt,numeric(1))

#     - tapply - lze aplikovat na vektor, jemu� je p�i�azen faktor rozli�uj�c� hodnoty do skupin
#              - pou�ijeme d�le 
#######################################################################################

# V�po�et pr�m�r� kapacit po 5 cyklech dle v�robc�
tapply(data5S$kap5, data5S$vyrobce, mean)

# V�po�et medi�nu kapacit po 5 cyklech dle v�robc�
tapply(data5S$kap5, data5S$vyrobce, quantile, probs=0.5)

# Obdobn� lze ur�it dal�� charakteristiky.

# Pozor! Funkce pro v�po�et �ikmosti (skewness) a �pi�atosti (kurtosis) nejsou sou��st� z�kladn�ho R, najdete je v bal��ku moments
install.packages("moments")
library(moments)

# dal�� charakteristiky -> var(), sd(), min(), max(), skewness(), kurtosis() 


## Krabicov� graf - angl. boxplot #####################################################
boxplot(b5)

# �prava grafu
boxplot(b5,
        main="Kapacita po 5 cyklech (mAh)", 
        xlab="V�robce B",
        ylab="kapacita (mAh)")

# Zabarven� boxplotu, zobrazen� pr�m�ru
boxplot(b5,
        main="Kapacita po 5 cyklech (mAh)", 
        xlab="V�robce B",
        ylab="kapacita (mAh)",
        col="grey")
points(1, mean(b5,na.rm=TRUE), pch=3)

# Horizont�ln� boxplot
boxplot(b5,
        main="Kapacita po 5 cyklech (mAh), v�robce B",
        horizontal=TRUE,
        xlab="kapacita (mAh)")

# Dal�� parametry
boxplot(b5,
        main="Kapacita po 5 cyklech (mAh)", 
        xlab="V�robce B",
        ylab="kapacita (mAh)",
        col="grey",
        outline=FALSE,			    # nezobraz� odlehl� pozorov�n�
        boxwex=0.5)                         # zm�n� ���ku krabice na 1/2

# P�id�n� p��mky y=mean(b5)
abline(h=mean(b5,na.rm=TRUE), col="red",lty=2)

# Funkci boxplot lze vyu��t nejen k vykreslen� grafu
boxplot(b5,plot=FALSE)

## V�cen�sobn� krabicov� graf - angl. multiple boxplot ################################
boxplot(data5)

boxplot(data5, 
        boxwex=0.5,
        col="grey")

boxplot(data5, 
        boxwex=c(0.5,0.5,1.5,1.5),
        col=c("red","yellow","grey","blue"))

boxplot(data5, 
        boxwex=c(0.5,0.5,1.5,1.5),
        col=terrain.colors(4))
abline(h=1900,col="red",lty=2)

# Mezera v boxplotu - my�lenka - t�et� pozice bude vynech�na
boxplot(data5,
        at=c(1,2,4,5))

# Boxplot konstruov�n z dat ve standartn�m datov�m form�tu - s vyu�it�m funkce split
pom=split(data5S$kap5, data5S$vyrobce) #do prom�nn� pom jsou ulo�ena data ve form�tu list, tj. seznam prom�nn�ch
boxplot(pom)

# Po�ad� krabic dle p��n� s vyu�it�m funkce list
pom=list(a5, b5, c5, d5)
boxplot(pom)

pom=list(d5, c5, b5, a5)
boxplot(pom)

## Histogram ##########################################################################
hist(a5)
hist(a5,breaks=10) # Co d�laj� r�zn� hodnoty parametru breaks s grafem?

hist(a5, 
     main="Histogram pro kapacitu akumul�tor� po 5 cyklech, v�robce A", 
     xlab="kapacita (mAh)",
     ylab="f(x)",
     col="blue", 
     border="grey",
     labels=TRUE)         # p�id� absolutn� �etnosti dan�ch kategori� ve form� popisk�  

hist(a5, 
     main="Histogram pro kapacitu akumul�tor� po 5 cyklech, v�robce A", 
     xlab="kapacita (mAh)",
     ylab="f(x)",
     col="blue", 
     border="grey",
     labels=TRUE,         # p�id� absolutn� �etnosti dan�ch kategori� ve form� popisk�  
     freq=FALSE)	       # zm�na m���tka na ose y --> f(x)
lines(density(a5))        # p�ipoj� graf odhadu hustoty pravd�podobnosti

# Generov�n� hustoty norm�ln�ho rozd�len�
xfit=seq(min(a5), max(a5), length=40) 
yfit=dnorm(xfit, mean=mean(a5), sd=sd(a5)) 
lines(xfit, yfit, col="black", lwd=2)

## QQ-graf - aneb grafick� n�stroj pro posouzen� normality ############################
qqnorm(a5)
qqline(a5)

#######################################################################################
## V�ce graf� do jednoho obr�zku -> funkce layout nebo par ############################
## Pod�vejte se na mo�nosti kombinov�n� graf� - http://www.statmethods.net/advgraphs/layout.html

## Kombinace histogramu a boxplotu ####################################################
pom=layout(mat = matrix(1:8,2,4, byrow=FALSE), height = c(2.5,1))
layout.show(pom)
par(oma=c(2,2,3,2),mar=c(2,2,3,2))

hist(a5, 
     main="V�robce A",
     xlab="kapacita (mAh) po 5 cyklech", 
     ylab="�etnost", 
     ylim=c(0,32), 
     xlim=c(1730,2040))
boxplot(a5, 
        horizontal=TRUE, 
        ylim=c(1700,2040), 
        boxwex=1.5)

hist(b5, 
     main="V�robce B", 
     xlab="kapacita (mAh) po 5 cyklech", 
     ylab="�etnost", 
     ylim=c(0,32), 
     xlim=c(1730,2040))
boxplot(b5, 
        horizontal=TRUE, 
        ylim=c(1700,2040), 
        boxwex=1.5)

hist(c5, 
     main="V�robce C", 
     xlab="kapacita (mAh) po 5 cyklech", 
     ylab="�etnost", 
     ylim=c(0,32), 
     xlim=c(1730,2040))
boxplot(c5, 
        horizontal=TRUE, 
        ylim=c(1700,2040), 
        boxwex=1.5)

hist(d5, 
     main="V�robce D", 
     xlab="kapacita (mAh) po 5 cyklech", 
     ylab="�etnost", 
     ylim=c(0,32), 
     xlim=c(1730,2040))
boxplot(d5, 
        horizontal=TRUE, 
        ylim=c(1700,2040), 
        boxwex=1.5)
mtext("Histogramy a boxploty pro jednotliv� v�robce po 5 cyklech", cex = 1.5, outer=TRUE, side=3)

# Pro pokro�il� - pomoc� for-cyklu
pom=layout(mat = matrix(1:8,2,4, byrow=FALSE), height = c(2.5,1))
layout.show(pom)
par(oma=c(2,2,3,2), mar=c(2,2,3,2))

for (i in 1:4){
  hist(data5[,i], 
       main=paste("V�robce",colnames(data5)[i]), 
       xlab="kapacita (mAh) po 5 cyklech", 
       ylab="�etnost", 
       xlim=c(min(data5,na.rm=TRUE), max(data5,na.rm=TRUE)), 
       ylim=c(0,32))
  boxplot(data5[,i], 
          horizontal=TRUE, 
          ylim=c(min(data5,na.rm=TRUE), max(data5,na.rm=TRUE)), 
          boxwex=1.5)
}
mtext("Histogramy a boxploty pro jednotliv� v�robce po 5 cyklech", cex = 1.5, outer=TRUE, side=3)

## Kombinace histogramu a QQ-plotu ####################################################
pom=layout(mat = matrix(1:8,2,4, byrow=FALSE), height = c(2,1.5))
layout.show(pom)
par(oma=c(2,2,3,2), mar=c(2,2,3,2))

for (i in 1:4){
  hist(data5[,i], 
       main=paste("V�robce",colnames(data5)[i]), 
       xlab="kapacita (mAh) po 5 cyklech", 
       ylab="�etnost", 
       xlim=c(min(data5,na.rm=TRUE), max(data5,na.rm=TRUE)), 
       ylim=c(0,0.037), 
       freq=FALSE)
  lines(density(data5[,i], na.rm=TRUE))
  xfit=seq(min(data5[,i], na.rm=TRUE), max(data5[,i], na.rm=TRUE), length=40) 
  yfit=dnorm(xfit, mean=mean(data5[,i], na.rm=TRUE), sd=sd(data5[,i], na.rm=TRUE)) 
  lines(xfit, yfit, col="blue", lty=2)
  qqnorm(data5[,i])
  qqline(data5[,i])
}
mtext("Histogramy a QQ-ploty pro jednotliv� v�robce po 5 cyklech", cex = 1.5, outer=TRUE, side=3)


#######################################################################################
## 7. Identifikace odlehl�ch pozorov�n� (a jejich odstran�n� z dat. r�mce)#############

## S individu�ln�m posouzen�m, jak nalo�it s odlehl�mi hodnotami ######################
# Se��zen� datov�ho r�mce aku.st podle prom�nn�ch aku.st$vyrobce a data.st$kap5
data5S=data5S[with(data5S, order(data5S$vyrobce,data5S$kap5)),]

# V RkWardu lze pro se�azen� dat pou��t z�lo�ku Data / Sort data
# Z�pis prom�nn� do nov�ho sloupce, v n�m� budeme odstra�ovat vybran� odlehl� pozorov�n�
data5S$kap5.bez=data5S$kap5

# N�sledn� lze pohodln� "ru�n�" odstranit (pop�. opravit) vybran� odlehl� pozorov�n�

# Sofistikovan�j�� metody -> nap�. vnit�n� hradby, mnohorozm�rn� detekce odlehl�ch hodnot...