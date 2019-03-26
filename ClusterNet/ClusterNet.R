#-----------------------------------------------------------------
# Institution: FLACSO (https://www.flacso.edu.ec/portal)
# Proyect: Exploratory Data Analysis. 
# Data: Resutls from Pérez-Oviedo y Cajas-Guijarro (2017)
#   - Midiendo la centralidad de los países y la integración 
#   - comercial desde una perspectiva de red
# Author: Víctor Morales-Oñate (https://sites.google.com/site/moralesonatevictor/)
# Date: 06-03-2018
# Article: Análisis Cluster en la Red de Comercio 
#                   Internacional entre 1992 y 2015
#-----------------------------------------------------------------

rm(list=ls())
gc()
graphics.off()

#************** St:Libraries
library(xtable) #LaTeX tables
library(ggplot2) # Densities plots
library(fpc) # cluster stats: within, betweeen, others
library(reshape2) # reshaping for groups barplots
#************** End:Libraries

#************** St:Import and Data cooking
setwd("~/Documents/DataBase/Gravity/03 Proyecto 3/Articulo flash")
load("RESULTADOS.RData")

# Keeping target variables only:
elas <- subset(resultados, Indicador == "Elasticidad país-país")
summary(elas)

pib <- subset(resultados, Indicador == "% de PIB global")
summary(pib)

# Removing not needed objects
obj <- ls()
keep <- c("elas","pib")
rm(list = obj[-match(keep,obj)])

#remove "Indicador" variable from data.frame
elas <- elas[,c("i","j","Valor","year")] 
out <- (elas[,"i"]!=elas[,"j"])
elas <- elas[out,]

# Assigning % pib by Origin and Destination countries:
names(pib) <- c("or","dest","Indicador","perpib","year")
elas <- merge(elas,pib,by.x = c("i","year"),
              by.y = c("or","year")) #origin
elas <- merge(elas,pib,by.x = c("j","year"),
              by.y = c("or","year")) #dest

elas <- elas[,c(1,2,3,4,7,10)]
names(elas) <- c("j","year","i","Valor","perpib.i","perpib.j")
#************** End:Import and Data cooking

# ================= St:Control parameters if running all code:
yeardiff <- 0 # 0|1: evaluates pairwise Kruskal Wallis tests by year. 
pbool <- 0 # 0|1: 0 indicates that plots are not generated. 
bpdf <- 0 # plots for PDF are generated
cst <- 0 # pairwise yearly clustering statistics (time consumming)
# ================= End:Control parameters if running all code:

#************** St:Descriptive statistics
# Global data distribution
if(pbool==1)
{
  plot(density(log(elas$Valor)))
}
summary(elas$Valor)
summary(log(elas$Valor))

# Yearly distribution:

yrs <- unique(elas$year)

par(mfrow = c(6,4),mar=c(0,0,1,0))
yealydat <- list()
for(k in 1:length(yrs))
{
  # gen yearly data:
  yealydat[[k]] <- subset(elas,year==yrs[k])
  if(pbool==1)
  {
    plot(density(log(yealydat[[k]]$Valor)),main=yrs[k])
  }
  #Summaries by year:
  print(summary(yealydat[[k]]$Valor))
  print(summary(log(yealydat[[k]]$Valor)))
  print(k)
}
par(mfrow = c(1,1))


if(pbool==1 & bpdf == 1)
{
  pdf("EDA_01")
  ggplot(elas, aes(Valor)) + geom_density(adjust = 5)
  dev.off()
}
if(pbool==1)
{
  ggplot(elas, aes(Valor)) + geom_density(adjust = 5)
}



elas$year <- factor(elas$year)
if(pbool==1 & bpdf == 1)
{
  pdf("EDA_02")
  ggplot(elas, aes(log(Valor), fill = year, colour = year)) +
    geom_density(alpha = 0.1) + xlim(-20, 1)
  graphics.off()
}
if(pbool==1)
{
  ggplot(elas, aes(log(Valor), fill = year, colour = year)) +
    geom_density(alpha = 0.1) + xlim(-20, 1)
}




tapply((elas$Valor),elas$year,shapiro.test)

#************** End:Descriptive statistics




#************** St: Pairwise Kruskal Wallis tests by year.

#Are populations different? Kruskal-Wallis (Ho: identical populations)

gktest <- kruskal.test(log(elas$Valor),elas$year) #global kruskal test
print(gktest)

#Loop for pariwise significance test between years (local kruskal test):
lkTest <- NULL
for(i in 1:(length(yrs)-1))
{
  for(j in (i+1):(length(yrs)))
  {
    aux <- data.frame(
      auxdat= c(log(yealydat[[i]]$Valor),log(yealydat[[j]]$Valor)),
      auxfa =  c(rep(1,nrow(yealydat[[i]])),rep(2,nrow(yealydat[[j]])))
    )
    lkaux <- kruskal.test(aux$auxdat~as.factor(aux$auxfa))
    sol <- c(yrs[i],yrs[j],lkaux$p.value)
    lkTest <- rbind(lkTest,sol)
  }
}

corte <- 0.05 #alpha
soltest <- lkTest[lkTest[,3]<corte,] # "<" means that populations ARE different
rownames(soltest) <- NULL
xtable(soltest,auto = TRUE)
#************** End: Pairwise Kruskal Wallis tests by year.


#************** St: Pairwise yearly clustering statistics (time consumming)
if(cst==1)
{
  statclus <- NULL
  for(i in 1:length(yrs))
  {
    dat.aux <- elas[elas$year==yrs[i],]
    
    # Data for graph:
    ctr.i <- unique(dat.aux$i)
    ctr.j <- unique(dat.aux$j)
    edge <- cbind(dat.aux$i,dat.aux$j)
    length(ctr.i);length(ctr.j)
    edge <- (union(ctr.i,ctr.j))
    
    dat.aux <- dat.aux[c("i","j","year","Valor","perpib.i","perpib.j")]
    
    # Geting pairwise link names:
    # rnom <- paste(dat.aux$j,dat.aux$i,sep="-")
    rnom <- paste(dat.aux$i,dat.aux$j,sep="-")
    # Naming pairwise links:
    rownames(dat.aux) <- rnom
    # Deleting unnecesary variables:
    dat.aux$year <- NULL;dat.aux$j <- NULL;dat.aux$i <- NULL
    
    dat.aux <- scale(dat.aux)
    dcl <- dist((dat.aux))
    
    hcl.single <- hclust(d = dcl,method = "single")
    nclus <- 10
    groups.single <- cutree(hcl.single, k=nclus) 
    
    hcl.complete <- hclust(d = dcl,method = "complete")
    groups.complete <- cutree(hcl.complete, k=nclus) 
    
    hcl.average <- hclust(d = dcl,method = "average")
    # plot(hcl.average,main = "Average Method",labels = FALSE)
    # nclus <- 10
    # rect.hclust(hcl.average, k=nclus, border="red")
    groups.average <- cutree(hcl.average, k=nclus) 
    # table(groups.average)
    
    hcl.ward <- hclust(d = dcl,method = "ward.D")
    groups.ward <- cutree(hcl.ward, k=nclus) 
    # table(groups.ward)
    
    # comparing 2 cluster solutions
    library(fpc)
    
    #Single
    stat.single <- cluster.stats(dcl, groups.single)
    #Complete
    stat.complete <- cluster.stats(dcl,  groups.complete)
    #Average
    stat.average <- cluster.stats(dcl, groups.average)
    #Ward
    stat.ward <- cluster.stats(dcl, groups.ward)
    
    a <- stat.single$average.within
    b <- stat.single$average.between
    
    c <- stat.complete$average.within
    d <- stat.complete$average.between
    
    e <- stat.average$average.within
    f <- stat.average$average.between
    
    g <- stat.ward$average.within
    h <- stat.ward$average.between
    
    cstat <- rbind(cbind(a,b),cbind(c,d),cbind(e,f),cbind(g,h))
    cstat <- data.frame(Algorith = c("Single","Complete","Average","Ward"),cstat)
    cstat <- cbind(rep(yrs[i],4),cstat)
    colnames(cstat) <- c("Year","Algorithm","Within","Between")
    statclus <- rbind(statclus,cstat)
    cat("Año",yrs[i],"\n")
  }
  save(statclus,file="statclus.RData")
}
#************** End: Pairwise yearly clustering statistics (time consumming)




#************** St: Hierarchical clustering Analisys

yrs <- c(1993,2001,2015)

Groups <- list() #Saves clus belongs by year
hc.model <- list() # saves hclust results per year
elas.g <- NULL #median per scaled log-elasticity

nclus <- 5  # Dendogram cut
pbool <- 0 # 0|1: 0 indicates that plots are not generated. 
bpdf <- 0# plots for PDF are generated

for(i in 1:length(yrs))
{
  dat.aux <- elas[elas$year==yrs[i],]

  # Data for graph:
  ctr.i <- unique(dat.aux$i)
  ctr.j <- unique(dat.aux$j)
  edge <- cbind(dat.aux$i,dat.aux$j)
  length(ctr.i);length(ctr.j)
  edge <- (union(ctr.i,ctr.j))
  
  dat.aux <- dat.aux[,c("i","j","year","Valor","perpib.i","perpib.j")]
  
  # Geting pairwise link names:
  # rnom <- paste(dat.aux$j,dat.aux$i,sep="-")
  rnom <- paste(dat.aux$i,dat.aux$j,sep="-")
  # Naming pairwise links:
  rownames(dat.aux) <- rnom
  # Deleting unnecesary variables:
  dat.aux$year <- NULL;dat.aux$j <- NULL;dat.aux$i <- NULL
  
  aux.dat <- dat.aux
  dat.aux <- scale(dat.aux)
  
  dcl <- dist((dat.aux))
  
  hcl.ward <- hclust(d = dcl,method = "ward.D")
  hc.model[[i]] <- hcl.ward
  
  if(pbool ==1 & bpdf ==1)
  {
    pdf(paste("EDA_",yrs[i],sep=""))
    plot(hcl.ward ,main="",labels = FALSE, 
         h = -0.5,ylab = "",xlab ="",sub="") #h cuts a height proportion
    rect.hclust(hcl.ward, k=nclus, border="red")  
    dev.off()
  }
  if(pbool ==1)
  {
    plot(hcl.ward ,main="",labels = FALSE, 
         h = -0.5,ylab = "",xlab =paste(yrs[i]),sub="") #h cuts a height proportion
    rect.hclust(hcl.ward, k=nclus, border="red")  
  }
  
  #median of scaled log elasticity
  elas.g <- rbind(elas.g,tapply(dat.aux[,1],groups.ward,median))
  groups.ward <- cutree(hcl.ward, k=nclus)
  
  dat.aux <- cbind(groups.ward,dat.aux)
  aux.dat <- data.frame(groups.ward,aux.dat)
  
  
  
  aux.dat$groups.ward <- factor(aux.dat$groups.ward)
  colnames(aux.dat)[1] <- "Grupo"
  
  if(pbool==1 & bpdf == 1)
  {
    pdf(paste("EDA_G",yrs[i],sep=""))
    ggplot(aux.dat, aes(log(Valor), fill = Grupo, colour = Grupo)) +
      geom_density(alpha = 0.1) + xlim(-20, 1)
    
    dev.off()
  }
  if(pbool==1)
  {
    ggplot(aux.dat, aes(log(Valor), fill = Grupo, colour = Grupo)) +
      geom_density(alpha = 0.1) + xlim(-20, 1)
  }
  
  
  
  colnames(dat.aux) <- c("Grupo","Elasticidad","Pib_i","Pib_j")
  
  or <- substr(rownames(aux.dat),1,3)#origin
  dt <- substr(rownames(aux.dat),5,7)#destination
  aux.dat <- data.frame(cbind(aux.dat,or,dt))
  # colnames(dat.aux) <- c("Grupo","Elasticidad","Pib_i","Pib_j","Origen","Destino")
  
  aux.dat <- data.frame(aux.dat,dat.aux[,2])
  colnames(aux.dat) <- c("Grupo","Valor","Pib_i","Pib_j","Origen","Destino","Elasticidad")
  write.xlsx(aux.dat, paste("groups",yrs[i],".xlsx",sep=""))
  
  Groups[[i]] <- groups.ward
  print(table(groups.ward))
  cat("Año",yrs[i],"\n")
}

save(Groups,file = "linkGroups.RData")
# rm(list = ls())
# load("linkGroups.RData")

bg <- NULL #
yrs <- c(1993,2001,2015)
for(i in 1:3)
{
  bg <- rbind(bg,as.vector(table(Groups[[i]])))
}
bg

bg <- as.data.frame(bg)
bg <- cbind(yrs,bg)
colnames(bg) <- c("Año","G1","G2","G3","G4","G5")
bg <- melt(bg,id.vars="Año")
colnames(bg) <- c("Año","Grupo","Frecuencia")
bg

if(pbool ==1 & bpdf ==1)
{
  pdf("EDA_bars")
  ggplot(bg,aes(x=Grupo,y=Frecuencia,fill=factor(Año)))+
    geom_bar(stat="identity",position="dodge")+
    scale_fill_discrete(name="Año",
                        breaks=c("1993","2001","2015"))+
    xlab("Grupos")+ylab("Frecuencia")+
    geom_text(
      aes(label = Frecuencia), 
      position = position_dodge(1),
      vjust = -0.3, size = 3.5
    )
  dev.off()
}
if(pbool ==1)
{
  ggplot(bg,aes(x=Grupo,y=Frecuencia,fill=factor(Año)))+
    geom_bar(stat="identity",position="dodge")+
    scale_fill_discrete(name="Año",
                        breaks=c("1993","2001","2015"))+
    xlab("Grupos")+ylab("Frecuencia")
}



plot(hc.model[[1]] ,main="",labels = FALSE, 
     h = -0.5,ylab = "",xlab ="",sub="") #h cuts a height proportion
rect.hclust(hcl.ward, k=nclus, border="red") 


# groups.ward <- cutree(hc.model[[1]], k=1:50)
str(hc.model[[2]])
xtable(elas.g,digits = 3)
sort(elas.g[1,])

kk <- 1 #año
jj <- 3 #grupo jj
# Groups[[kk]][Groups[[kk]]==jj]
names(Groups[[kk]])

gg <- c("G1","G2","G3","G4","G5")


for(kk in 1:3)
{
  for(jj in 1:5)
  {
    print(c(yrs[kk],gg[jj]))
    or <- substr(names(Groups[[kk]]),1,3)#origin
    dt <- substr(names(Groups[[kk]]),5,7)#destination
    
    print(sort(table(or[Groups[[kk]]==jj]),decreasing = TRUE)[1:5])
    print(sort(table(dt[Groups[[kk]]==jj]),decreasing = TRUE)[1:5])
    cat("\n")
  }
}



table(Groups[[1]])
Groups[[1]][Groups[[1]]==5]



# i j esten bien (check)
# distribuciones por grupo en grafica (check)
# 2015: todas las parejas de todos los grupos (check)
#     




rm(list=ls())
gc()
graphics.off()

#************** St:Libraries
library(igraph)
library(rworldmap)
#************** End:Libraries

#************** St:Import and Data cooking
setwd("~/Documents/DataBase/Gravity/03 Proyecto 3/Articulo flash")
load("linkGroups.RData")

i = 3
yrs <- c(1993,2001,2015)

load("RESULTADOS.RData")

# Keeping target variables only:
fil <- resultados$Indicador == "Elasticidad país-país" & resultados$year == yrs[i]
elas <- resultados[fil,]
# str(elas)
elas <- data.frame(elas)


elas <- elas[,c("i","j","Valor","year")] 
out <- (elas[,"i"]!=elas[,"j"])
elas <- elas[out,]

elas <- elas[order(elas$i,elas$j),]


data1 <- Groups[[i]]
str(data1)
names(data1)


data1 <- as.data.frame(data1)
data1$nflow <- rownames(data1)
colnames(data1)[1] <- "weight"
data1$flow <- 1
data1$origin <- substr(data1$nflow,1,3)
data1$dest <- substr(data1$nflow,5,7)
data1 <- data1[order(data1$origin,data1$dest),]


data1 <- data1[c("origin","dest","flow","weight")]
rownames(data1) <- 1:nrow(data1)

data1$flow <- elas$Valor


g1 <- graph_from_data_frame(data1[,c("origin","dest")], directed = TRUE, vertices = NULL)
E(g1)$width <- data1$flow
E(g1)$weight <- data1$weight

df.aux1 <- unique(c(data1$origin,data1$dest))
df.aux <- data.frame(i = df.aux1, val = rep(1,length(df.aux1)))
df.aux <- df.aux[order(df.aux$i),]


base_map_c <-joinCountryData2Map(df.aux,
                                 joinCode = "ISO3",
                                 nameJoinColumn = "i")

gsa <- g1

base_map <- base_map_c[which(base_map_c$ISO3 %in% names(V(g1))   ),]
base_map <- base_map[order(base_map$ISO3),]
coordenadas<-coordinates(base_map)
rownames(coordenadas)<-base_map$ISO3

color_letra=""
color_tierra="white"
color_bordes="grey"
color_oceano="lightblue"

ccol <- factor(E(gsa)$weight*5)
colores <- c("goldenrod1","green","blue","violet","red")
colores <- c("blue","green","snow2","red","violet")
# colores <- c("thistle","thistle1","thistle3","thistle2","thistle4")
# colores <- c("snow2","papayawhip","violet","thistle","navyblue")



colores <- c("blue","green","snow2","red","violet")#total
# colores <- c("blue","gray96","gray97","gray98","gray99")#G1
# colores <- c("gray96","blue","gray97","gray98","gray99")#G2
# colores <- c("gray97","gray96","blue","gray98","gray99")#G3
# colores <- c("gray98","gray96","gray97","blue","gray99")#G4
# colores <- c("gray98","gray96","gray97","gray99","blue")#G5


levels (ccol ) <- colores
ccol <- as.character(ccol)

graphics.off()
png(paste("map",yrs[i],".png",sep=""),  width = 7000, height = 5000)
mapBubbles( dF=base_map
            , nameZSize="Value"
            , nameZColour="white"
            , colourPalette='white2Black'
            , oceanCol=color_oceano
            , landCol=color_tierra
            , addColourLegend = FALSE
            , borderCol = color_bordes
            , addLegend = FALSE
            , symbolSize = 5
)


plot(gsa, 
     edge.arrow.size=0.001,
     # edge.width = (E(gsa)$width)*20,
     edge.width = 1,
     edge.color=ccol,
     vertex.label.cex=0.001,
     vertex.frame.color="grey", 
     edge.curved=0.2,
     layout=coordenadas,
     add=T,
     rescale=F)
legend(x = -110,y = -50,legend = c("G1","G2","G3","G4","G5"), 
       col = colores,bty = "n",
       lty = 1,horiz=TRUE,xpd = FALSE, cex = 10,lwd = 8)
dev.off()

