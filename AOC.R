library(maptools)
library(rgdal)
library(sp)
library(RColorBrewer)
library(classInt)
france<-readOGR("../../Documents/vieux-Documents/cartographie/geofla/COMMUNES/","COMMUNE") # fichier geofla communes (open data)
dataigp<-read.csv2("../../Downloads/2015-03-24-COMAGRI_-_Communes_Aires_IG_.csv",stringsAsFactors=F) # fichier IGP sur data.gouv.fr
dataaoc<-read.csv2("../../Downloads/2015-03-24-COMAGRI_-_Communes_Aires_AO.csv",stringsAsFactors=F) # fichier AOC sur data.gouv.fr

europe<-readOGR("NUTS_2010_03M_SH/Data","NUTS_RG_03M_2010") # données EUROSTAT : chercher NUTS Eurostat
frontieres<-subset(europe,europe$NUTS_ID=="FR") # on garde juste la France 
frontieres<-spTransform (frontieres, CRS ("+init=epsg:2154") ) # change la projection en LAMBERT pour correspondre avec geofla
# le code suivant n'est pas super optimisé
dataigp$CID<-sprintf("%05s",dataigp$CI)
dataigp$NOMBRE<-1
dataigp<-aggregate(NOMBRE~CID,data=dataigp,sum)
dataaoc$CID<-sprintf("%05s",dataaoc$CI)
dataaoc$NOMBRE<-1
dataaoc<-aggregate(NOMBRE~CID,data=dataaoc,sum)
data<-rbind(dataigp,dataaoc) # on combine les deux fichiers et il n'est pas très malin d'appeler ça "data"
data<-aggregate(NOMBRE~CID,data=data,sum)

m <- match(france$INSEE_COM, data$CID) # pour faire le lien entre france et les données
france$VAR <- data$NOMBRE[m]
nclr <- 11
plotclr <- brewer.pal(nclr,"RdYlGn")[nclr:1] 
class <- classIntervals(france$VAR, nclr, style="quantile",dataPrecision=0)
colcode <- findColours(class, plotclr)
colcode[is.na(colcode)]<-"#999999"
pdf("../../Desktop/france-test2.pdf",width=8,height=8)
par(mar=c(1,2,2,1))
plot(france, col=colcode, border=colcode, lwd=.1) #border="#ffffff00"
#plot(subset(france,france$INSEE_COM=="10387"),add=T,col="black")
plot(frontieres,add=T,col="#ffffff00",xlim=c(99226,1242375),ylim=c(6049647,7110524))
title(main="Nombre d'AOC, AOP et IGP par commune",line=-1)
title(sub="Source : INAO sur data.gouv.fr | Réalisation Baptiste Coulmont \nhttp://coulmont.com/blog/2015/03/29/aoc-igp",line=-5 ,cex.sub=.7)
legend(090224,6711753,legend=c(names(attr(colcode,"table"))[nclr:1],"N.A."), fill=c(attr(colcode, "palette")[nclr:1],"#999999"), cex=1, bty="n")
dev.off()

# même chose mais on change la variable cartographiée
m<-match(france$INSEE_COM,data$CID)
france$VAR<-data$NOMBRE[m]
france$VAR<-100*france$VAR/france$SUPERFICIE # on s'intéresse au nombre d'IGP par km carré par commune
nclr <- 7
plotclr <- brewer.pal(nclr,"RdYlGn")[nclr:1] 
class <- classIntervals(france$VAR, nclr, style="quantile",dataPrecision=1)
colcode <- findColours(class, plotclr)
colcode[is.na(colcode)]<-"#999999"
pdf("../../Desktop/france-test-superficie.pdf",width=8,height=8)
par(mar=c(1,2,2,1))
plot(france,col=colcode,border="#ffffff00")
plot(frontieres,add=T,col="#ffffff00",xlim=c(99226,1242375),ylim=c(6049647,7110524))
title(main="Nombre d' AOC/AOP/IGP par km2 par commune",line=-1)
title(sub="Source : INAO sur data.gouv.fr | Réalisation Baptiste Coulmont \nhttp://coulmont.com/blog/2015/03/29/aoc-igp",line=-5 ,cex.sub=.7)
legend(090224,6711753,legend=c(names(attr(colcode,"table"))[nclr:1],"N.A."), fill=c(attr(colcode, "palette")[nclr:1],"#999999"), cex=1, bty="n")
dev.off()


