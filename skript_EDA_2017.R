#######################################################################################
################ Preprocesing dat a exploraèní analýza ################################
############### Adéla Vrtková, Martina Litschmannová ##################################
#######################################################################################

##  Máme data, a co dál?
#   1. Spustíme potøebné balíèky, které obsahují další statistické funkce
#   2. Nastavíme pracovní adresáø, odkud importujeme data, popø. kam chceme ukládat výstupy
#   3. Importujeme data (z pracovního adresáøe, z internetu)
#   4. Pre-processing -> a) Podíváme se na data
#                        b) uložíme si data ve více formátech (každá funkce má "radìji" jiný formát)
#   5. Analýza kvalitativních promìnných
#   6. Analýza kvantitativních promìnných
#   7. Identifikace a rozhodnutí o vylouèení/ponechání odlehlých pozorování

#######################################################################################
## 1. Jak nainstalovat a spustit rozšiøující balíèek funkcí? ##########################

# Instalování balíèku
install.packages("openxlsx") 

# Naètení balíèku (nutno opakovat pøi každém novém spuštìní Rka, vhodné mít na zaèátku skriptu)
library(openxlsx)

#######################################################################################
## 2. Kde se ukládají generované výstupy, nastavení pracovního adresáøe ###############

# Výpis pracovního adresáøe
getwd()

# Nastavení pracovního adresáøe -> do uvozovek, celou cestu
setwd("C:/Users/lit40/.rkward")


#######################################################################################
## 3. Naètení datového souboru ########################################################

# základní funkce - read.table, read.csv, read.csv2, ...
# záleží hlavnì na formátu souboru (.txt, .csv), na tzv. oddìlovaèi jednotlivých hodnot, desetinné èárce/teèce

# Naètení a uložení datového souboru ve formátu csv2 z lokálního disku do datového rámce data
data=read.csv2(file="C:/Martina/STA1/DATA/aku.csv")

# Naètení a uložení datového souboru ve formátu csv2 z internetu do datového rámce data
data=read.csv2(file="http://am-nas.vsb.cz/lit40/DATA/aku.csv")

# Naètení a uložení datového souboru ve formátu xlsx z lokálního disku do datového rámce data - používáme funkci z balíèku XLConnect, který jsme v úvodu rozbalili
install.packages("openxlsx")
library(openxlsx)
data=readWorkbook("C:/Users/lit40/Desktop/aku.xlsx",
             sheet=1,                        # èíslo listu (defaultní hodnota 1)
             colNames=TRUE,                  # informace, zda v prvním øádku je hlavièka s názvy sloupcù (defaultní hodnota TRUE)
             startRow = 4,		     # na kterém øádku má naèítání zaèít (není-li tento parametr zadán, zaèíná naèítání na øádku 1)
             cols = 2:9)   		     # které sloupce se mají naèíst (není-li tento parametr zadán, naèítají se všechny neprázdné sloupce)
colnames(data)=c("A5","B5","C5","D5","A100","B100","C100","D100")

# nebo

install.packages("XLConnect")
library(XLConnect)
wb=loadWorkbook("C:/Martina/STA1/DATA/aku.xlsx")
data=readWorksheet(wb, sheet="Data", header=TRUE, startRow = 4, startCol=2)
colnames(data)=c("A5","B5","C5","D5","A100","B100","C100","D100")

# Naètení a uložení datového souboru ve formátu xlsx z internetu (pomocí balíèku XLConnect) 
# do datového rámce data (komplikovanìjší, doporuèujeme radìji si stáhnout xlsx soubor na lokální disk)
tmp = tempfile(fileext = ".xlsx")
download.file(url = "http://am-nas.vsb.cz/lit40/DATA/aku.xlsx", destfile = tmp, mode="wb")
wb=loadWorkbook(tmp)
data=readWorksheet(wb,sheet="Data",header=TRUE,startRow = 4,startCol=2)
colnames(data)=c("A5","B5","C5","D5","A100","B100","C100","D100")


#######################################################################################
## 4. Pre-processing dat ##############################################################

# Výpis datového souboru
data
# Zobrazení prvních šesti øádkù
head(data)

# Zobrazení posledních šesti øádkù
tail(data)

# Zobrazení 10. øádku
data[10,]

# Zobrazení 3. sloupce 
data[,3]
# nebo (víme-li, jak se jmenuje promìnná zapsána ve 3. sloupci)
data[["C5"]]
# nebo
data$C5  

# Uložení prvního a pátého sloupce dat. rámce data do dat. rámce pokus
pokus=data[,c(1,5)]

# Uložení prvních 4 sloupcù dat. rámce data do dat. rámce data5
data5=data[,c(1:4)]
#nebo
data5=data[,-c(5:8)]

## Pozn. pøi ukládání dat mysleme na pøehlednost v názvech, data5 obsahují kapacity akumulátorù všech výrobcù po 5ti cyklech

## Pøevod dat do standardního datového formátu
data5S=reshape(data[,1:4],			         # èást datového rámce, která bude pøevádìna do std. datového formátu
               direction="long",                # parametr urèující tzv. "long" nebo "wide" formát
               varying=c("A5","B5","C5","D5"),	 # názvy promìnných, které mají být zaøazeny do sloupce hodnot
               v.names="kap5",			             # pojmenování sloupce hodnot
               times=c("A","B","C","D"),		     # varianty promìnné, která pøiøazuje identifikátory jednotlivým hodnotám promìnné kap5
               timevar="vyrobce")		           # pojmenování promìnné obsahující identifikátory promìnné kap5

# pøevedení promìnné data5S$vyrobce na typ factor
data5S$vyrobce=as.factor(data5S$vyrobce)

# odstranìní nadbyteèné promìnné id z datového rámce data5S
data5S=data5S[,-3]

# odstranìní NA z datového rámce data5S
data5S=na.omit(data5S)

## Pøevod párových dat do standardního datového formátu
dataS=reshape(data,
              direction="long",
              varying=list(c("A5","B5","C5","D5"),
                           c("A100","B100","C100","D100")),
              v.names=c("kap5","kap100"),
              times=c("A","B","C","D"),
              timevar="vyrobce")

# pøevedení promìnné dataS$vyrobce na typ factor
dataS$vyrobce=as.factor(dataS$vyrobce)

# odstranìní nadbyteèné promìnné id z datového rámce dataS
dataS=dataS[,-4]

# odstranìní NA z datového rámce dataS
dataS=na.omit(dataS)

# Definování nové promìnné v stávajícím datovém rámci
dataS$pokles=dataS$kap5-dataS$kap100

## Vytvoøení samostatných promìnných
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

### Poznámky pro zopakování principu grafiky v R ######################################
# základem jsou tzv. high-level funkce, které vytvoøí graf (tj. otevøou grafické oknou a vykreslí dle zadaných parametrù)
# na nì navazují tzv. low-level funkce, které nìco do aktviního grafického okna pøidají, samy o sobì neotevøou nové
# pø. low-level funkcí - napø. abline, points, lines, legend, title, axis ... které pøidají pøímku, body, legendu...
# tzn. pøed použitím "low-level" funkce je potøeba, volat "high-level" funkci (napø. plot, boxplot, hist, barplot, pie,...)

# další grafické parametry naleznete v nápovìdì
# nebo napø. zde http://www.statmethods.net/advgraphs/parameters.html
# nebo zde https://flowingdata.com/2015/03/17/r-cheat-sheet-for-graphical-parameters/
# nebo http://bcb.dfci.harvard.edu/~aedin/courses/BiocDec2011/2.Plotting.pdf

## Barvy v R
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf


#######################################################################################
## 5. Exploraèní analýza a vizualizace kategoriální promìnné ##########################

## Výpoèet èetostí ####################################################################
cetnosti=table(dataS$vyrobce) 
cetnosti # výpis

# Výpoèet relativních èetností - 2 ekvivalentní zpùsoby
rel.cetnosti=100*cetnosti/sum(cetnosti)  
rel.cetnosti # výpis

rel.cetnosti2=prop.table(cetnosti)*100

# Zaokrouhlení relativních èetností (%) na 1 desetinné místo
rel.cetnosti=round(rel.cetnosti,digits=1)
rel.cetnosti  # výpis

# Pozor na zaokrouhlovací chybu!!
rel.cetnosti[4]=100-sum(rel.cetnosti[1:3])
rel.cetnosti   # výpis

# Slouèení èetností a relativních èetností do tabulky èetností
tabulka=cbind(cetnosti,rel.cetnosti)    # cbind() ... slouèení sloupcù
tabulka   # výpis

# Pøejmenování názvù sloupcù v tabulce èetností
colnames(tabulka)=c("èetnost","rel.èetnost (%)")
tabulka

# Uložení tabulky do csv souboru pro export do MS Excel
write.csv2(tabulka,file="tabulka.csv")

# Kde je tabulka uložena?
getwd()


## Výseèový (koláèový) graf - angl. piechart ##########################################
cetnosti=table(dataS$vyrobce)  # v tuto chvíli není nutno používat, èetnosti jsme již spoèetli a uložili do promìnné cetnosti výše
pie(cetnosti)

# Zabarvení grafu
pie(cetnosti,
    col=c("red","green","yellow","blue"))
pie(cetnosti,
    col=heat.colors(4))

# Pøidání názvu grafu a popiskù
pie(cetnosti,
    col=heat.colors(4),
    main="Zastoupení výrobcù ve výbìru",
    labels=c("Výrobce A","Výrobce B","Výrobce C","Výrobce D"))

pie(cetnosti,
    col=heat.colors(4),
    main="Zastoupení výrobcù ve výbìru",
    labels=paste("Výrobce",names(cetnosti),"\n",cetnosti)) # funkce paste() umožòuje slouèit textové øetìzce a hodnoty promìnných, symbol "\n" tvoøí nový øádek v textu

# rel. èetnosti byly spoèteny a uloženy do promìnné rel.cetnosti výše
pie(cetnosti,
    col=heat.colors(4),
    main="Zastoupení výrobcù ve výbìru",
    labels=paste("Výrobce",names(cetnosti),"\n",cetnosti,";",rel.cetnosti,"%"))

# Pro zájemce - balíèek plotrix a funkce pie3D vytvoøí 3D koláèový graf


## Sloupcový graf - angl. barplot #####################################################
cetnosti=table(data5S$vyrobce)  # v tuto chvíli není nutno používat, èetnosti jsme již spoèetli a uložili do promìnné cetnosti výše
barplot(cetnosti)

# Zmìna barev, pøidání názvu
barplot(cetnosti,
        col=heat.colors(4),
        main="Zastoupení výrobcù ve výbìru",
        space=0.6)                             # parametr space vytvoøí mezeru mezi sloupci

# Pøidání dalších popiskù a legendy
barplot(cetnosti,
        col=heat.colors(4),
        horiz=TRUE,                            # horizontální orientace grafu
        border=FALSE,				                   # nevykresluje èáru kolem sloupeèkù
        main="Zastoupení výrobcù ve výbìru",
        names.arg=paste("Výrobce",names(cetnosti)))

legend("topright",
       paste("Výrobce",names(cetnosti)),
       col=heat.colors(4),
       fill=heat.colors(4),
       border=FALSE,
       bty="n")

# Pøidání absolutních a relativních èetností k odpovídajícím sloupcùm
bp = barplot(cetnosti,
             col=heat.colors(4),
             main="Zastoupení výrobcù ve výbìru",
             names.arg=paste("Výrobce",names(cetnosti)))
text(bp,cetnosti,cetnosti)

bp = barplot(cetnosti,
             col=heat.colors(4),
             main="Zastoupení výrobcù ve výbìru",
             names.arg=paste("Výrobce",names(cetnosti)))
text(bp,
     cetnosti,paste(cetnosti,";",rel.cetnosti,"%"),
     pos=1)						# parametr pos udává, kde bude text uveden vzhledem k dané pozici (1 = pod, 2 = vlevo, 3 = nad, 4 = vpravo) 	

## Jak graf uložit? ##################################################################

# Zjištìní aktivního okna, nastavení aktivního okna - tj. který graf chceme uložit?
dev.cur()
dev.set(2)

# Uložení obrázku ve formátu pdf (výška a šíøka jsou uvedeny v palcích (inch), 1inch=2,54cm)
dev.print(device=pdf,file="barplot.pdf",width=6.5,height=5)

# Zavøení grafického okna
dev.off()

# Kam se obrázek uložil?
getwd()

#######################################################################################
#####Pie charts are a very bad way of displaying information.##########################
##The eye is good at judging linear measures and bad at judging relative areas.########
##A bar chart or dot chart is a preferable way of displaying this type of data. #######
#######################################################################################

#######################################################################################
## 6. Exploraèní analýza a vizualizace kvantitativní promìnné #########################

## Popisná statistika #################################################################
summary(dataS$kap5)

# Výpoèet prùmìru jedné promìnné
mean(dataS$kap5)
mean(a5)

# Pozor na chybìjící hodnoty
mean(data$C5)
mean(data$C5,na.rm=TRUE)
mean(na.omit(data$C5))

# Výpoèet mediánu jedné promìnné
quantile(dataS$kap5,probs=0.5)
quantile(a5,probs=0.5)

# Urèení rozsahu
length(na.omit(dataS$kap5))

#######################################################################################
## funkce, které umožní aplikaci vybrané funkce na sloupce datového rámce, na hodnoty vektoru apod.
x=1:4
#     - lapply - na vstup aplikuje zvolenou funkci, vrátí list (seznam) hodnot
lapply(x,sqrt)

#     - sapply - na vstup aplikuje volenou funkci, vrátí vektor hodnot
sapply(x,sqrt)

#     - vapply - stejné jako sapply, akorát s parametrem navíc, který specifikuje výstup
vapply(x,sqrt,numeric(1))

#     - tapply - lze aplikovat na vektor, jemuž je pøiøazen faktor rozlišující hodnoty do skupin
#              - použijeme dále 
#######################################################################################

# Výpoèet prùmìrù kapacit po 5 cyklech dle výrobcù
tapply(data5S$kap5, data5S$vyrobce, mean)

# Výpoèet mediánu kapacit po 5 cyklech dle výrobcù
tapply(data5S$kap5, data5S$vyrobce, quantile, probs=0.5)

# Obdobnì lze urèit další charakteristiky.

# Pozor! Funkce pro výpoèet šikmosti (skewness) a špièatosti (kurtosis) nejsou souèástí základního R, najdete je v balíèku moments
install.packages("moments")
library(moments)

# další charakteristiky -> var(), sd(), min(), max(), skewness(), kurtosis() 


## Krabicový graf - angl. boxplot #####################################################
boxplot(b5)

# Úprava grafu
boxplot(b5,
        main="Kapacita po 5 cyklech (mAh)", 
        xlab="Výrobce B",
        ylab="kapacita (mAh)")

# Zabarvení boxplotu, zobrazení prùmìru
boxplot(b5,
        main="Kapacita po 5 cyklech (mAh)", 
        xlab="Výrobce B",
        ylab="kapacita (mAh)",
        col="grey")
points(1, mean(b5,na.rm=TRUE), pch=3)

# Horizontální boxplot
boxplot(b5,
        main="Kapacita po 5 cyklech (mAh), výrobce B",
        horizontal=TRUE,
        xlab="kapacita (mAh)")

# Další parametry
boxplot(b5,
        main="Kapacita po 5 cyklech (mAh)", 
        xlab="Výrobce B",
        ylab="kapacita (mAh)",
        col="grey",
        outline=FALSE,			    # nezobrazí odlehlá pozorování
        boxwex=0.5)                         # zmìní šíøku krabice na 1/2

# Pøidání pøímky y=mean(b5)
abline(h=mean(b5,na.rm=TRUE), col="red",lty=2)

# Funkci boxplot lze využít nejen k vykreslení grafu
boxplot(b5,plot=FALSE)

## Vícenásobný krabicový graf - angl. multiple boxplot ################################
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

# Mezera v boxplotu - myšlenka - tøetí pozice bude vynechána
boxplot(data5,
        at=c(1,2,4,5))

# Boxplot konstruován z dat ve standartním datovém formátu - s využitím funkce split
pom=split(data5S$kap5, data5S$vyrobce) #do promìnné pom jsou uložena data ve formátu list, tj. seznam promìnných
boxplot(pom)

# Poøadí krabic dle pøání s využitím funkce list
pom=list(a5, b5, c5, d5)
boxplot(pom)

pom=list(d5, c5, b5, a5)
boxplot(pom)

## Histogram ##########################################################################
hist(a5)
hist(a5,breaks=10) # Co dìlají rùzné hodnoty parametru breaks s grafem?

hist(a5, 
     main="Histogram pro kapacitu akumulátorù po 5 cyklech, výrobce A", 
     xlab="kapacita (mAh)",
     ylab="f(x)",
     col="blue", 
     border="grey",
     labels=TRUE)         # pøidá absolutní èetnosti daných kategorií ve formì popiskù  

hist(a5, 
     main="Histogram pro kapacitu akumulátorù po 5 cyklech, výrobce A", 
     xlab="kapacita (mAh)",
     ylab="f(x)",
     col="blue", 
     border="grey",
     labels=TRUE,         # pøidá absolutní èetnosti daných kategorií ve formì popiskù  
     freq=FALSE)	       # zmìna mìøítka na ose y --> f(x)
lines(density(a5))        # pøipojí graf odhadu hustoty pravdìpodobnosti

# Generování hustoty normálního rozdìlení
xfit=seq(min(a5), max(a5), length=40) 
yfit=dnorm(xfit, mean=mean(a5), sd=sd(a5)) 
lines(xfit, yfit, col="black", lwd=2)

## QQ-graf - aneb grafický nástroj pro posouzení normality ############################
qqnorm(a5)
qqline(a5)

#######################################################################################
## Více grafù do jednoho obrázku -> funkce layout nebo par ############################
## Podívejte se na možnosti kombinování grafù - http://www.statmethods.net/advgraphs/layout.html

## Kombinace histogramu a boxplotu ####################################################
pom=layout(mat = matrix(1:8,2,4, byrow=FALSE), height = c(2.5,1))
layout.show(pom)
par(oma=c(2,2,3,2),mar=c(2,2,3,2))

hist(a5, 
     main="Výrobce A",
     xlab="kapacita (mAh) po 5 cyklech", 
     ylab="èetnost", 
     ylim=c(0,32), 
     xlim=c(1730,2040))
boxplot(a5, 
        horizontal=TRUE, 
        ylim=c(1700,2040), 
        boxwex=1.5)

hist(b5, 
     main="Výrobce B", 
     xlab="kapacita (mAh) po 5 cyklech", 
     ylab="èetnost", 
     ylim=c(0,32), 
     xlim=c(1730,2040))
boxplot(b5, 
        horizontal=TRUE, 
        ylim=c(1700,2040), 
        boxwex=1.5)

hist(c5, 
     main="Výrobce C", 
     xlab="kapacita (mAh) po 5 cyklech", 
     ylab="èetnost", 
     ylim=c(0,32), 
     xlim=c(1730,2040))
boxplot(c5, 
        horizontal=TRUE, 
        ylim=c(1700,2040), 
        boxwex=1.5)

hist(d5, 
     main="Výrobce D", 
     xlab="kapacita (mAh) po 5 cyklech", 
     ylab="èetnost", 
     ylim=c(0,32), 
     xlim=c(1730,2040))
boxplot(d5, 
        horizontal=TRUE, 
        ylim=c(1700,2040), 
        boxwex=1.5)
mtext("Histogramy a boxploty pro jednotlivé výrobce po 5 cyklech", cex = 1.5, outer=TRUE, side=3)

# Pro pokroèilé - pomocí for-cyklu
pom=layout(mat = matrix(1:8,2,4, byrow=FALSE), height = c(2.5,1))
layout.show(pom)
par(oma=c(2,2,3,2), mar=c(2,2,3,2))

for (i in 1:4){
  hist(data5[,i], 
       main=paste("Výrobce",colnames(data5)[i]), 
       xlab="kapacita (mAh) po 5 cyklech", 
       ylab="èetnost", 
       xlim=c(min(data5,na.rm=TRUE), max(data5,na.rm=TRUE)), 
       ylim=c(0,32))
  boxplot(data5[,i], 
          horizontal=TRUE, 
          ylim=c(min(data5,na.rm=TRUE), max(data5,na.rm=TRUE)), 
          boxwex=1.5)
}
mtext("Histogramy a boxploty pro jednotlivé výrobce po 5 cyklech", cex = 1.5, outer=TRUE, side=3)

## Kombinace histogramu a QQ-plotu ####################################################
pom=layout(mat = matrix(1:8,2,4, byrow=FALSE), height = c(2,1.5))
layout.show(pom)
par(oma=c(2,2,3,2), mar=c(2,2,3,2))

for (i in 1:4){
  hist(data5[,i], 
       main=paste("Výrobce",colnames(data5)[i]), 
       xlab="kapacita (mAh) po 5 cyklech", 
       ylab="èetnost", 
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
mtext("Histogramy a QQ-ploty pro jednotlivé výrobce po 5 cyklech", cex = 1.5, outer=TRUE, side=3)


#######################################################################################
## 7. Identifikace odlehlých pozorování (a jejich odstranìní z dat. rámce)#############

## S individuálním posouzením, jak naložit s odlehlými hodnotami ######################
# Seøázení datového rámce aku.st podle promìnných aku.st$vyrobce a data.st$kap5
data5S=data5S[with(data5S, order(data5S$vyrobce,data5S$kap5)),]

# V RkWardu lze pro seøazení dat použít záložku Data / Sort data
# Zápis promìnné do nového sloupce, v nìmž budeme odstraòovat vybraná odlehlá pozorování
data5S$kap5.bez=data5S$kap5

# Následnì lze pohodlnì "ruènì" odstranit (popø. opravit) vybraná odlehlá pozorování

# Sofistikovanìjší metody -> napø. vnitøní hradby, mnohorozmìrná detekce odlehlých hodnot...