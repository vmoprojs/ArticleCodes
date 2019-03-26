#-----------------------------------------------------------------
# Institution: FLACSO (https://www.flacso.edu.ec/portal)
# Proyect: CLARABD. 
# Author: Víctor Morales-Oñate (https://sites.google.com/site/moralesonatevictor/)
# Date: 06-03-2018
# Article: Innovación en Ecuador: un enfoque espacial
#-----------------------------------------------------------------

rm(list = ls())
gc()
graphics.off()
cat("\014")


dat.f <- "~/Documents/DataBase/ACTI/Datos"
inn.f <- "/BASE DE DATOS SPSS INN"

setwd(paste(dat.f,inn.f,sep = ""))
library(rio)
#inn years: 2012,2013,2014
inn <- import("bdd_INN_2015.sav")

# **********VARIABLES REGIONALES
cbind(table(inn$cod_provincia))
# creo dummies:
prov.aux <- factor(inn$cod_provincia)
inn$provdumm <- model.matrix(~prov.aux)
rm(prov.aux)

# **********VARIABLES SECTORIALES

inn$sectoremp <- NA
f1 <- with(inn,(ciiu2=="C10" | ciiu2=="C11" | ciiu2=="C12" | ciiu2=="C13" | ciiu2=="C14" | ciiu2=="C15" | ciiu2=="C16" | ciiu2=="C17" | ciiu2=="C18" | ciiu2=="C31" | ciiu2=="C32"))
f2 <- with(inn,(ciiu2=="C19" | ciiu2=="C23" | ciiu2=="C24" | ciiu2=="C25" | ciiu2=="C33" | ciiu2=="S95"))
f3 <- with(inn,(ciiu2=="C20" | ciiu2=="C22" | ciiu2=="C27" | ciiu2=="C28" | ciiu2=="C29" | ciiu2=="C30" ))
f4 <- with(inn,(ciiu2=="C21" | ciiu2=="C26"))
f5 <- with(inn,(ciiu2=="B09" | ciiu2=="G45" | ciiu2=="G46" | ciiu2=="G47" | ciiu2=="H49" | ciiu2=="H50" | ciiu2=="H51" | ciiu2=="H52" | ciiu2=="H53" | ciiu2=="I55" | ciiu2=="I56" | ciiu2=="L68" | ciiu2=="M73" | ciiu2=="N77" | ciiu2=="N78" | ciiu2=="N79" | ciiu2=="N81" | ciiu2=="N82" | ciiu2=="O84" | ciiu2=="S94" | ciiu2=="S96" | ciiu2=="T97" | ciiu2=="T98" | ciiu2=="U99"))
f6 <- with(inn,(ciiu2=="J58" | ciiu2=="J59" | ciiu2=="J60" | ciiu2=="J61" | ciiu2=="J62" | ciiu2=="J63" | ciiu2=="K64" | ciiu2=="K65" | ciiu2=="K66" | ciiu2=="M69" | ciiu2=="M70" | ciiu2=="M71" | ciiu2=="M72" | ciiu2=="M74" | ciiu2=="M75" | ciiu2=="N80" | ciiu2=="P85" | ciiu2=="Q86" | ciiu2=="Q87" | ciiu2=="Q88" | ciiu2=="R90" | ciiu2=="R91" | ciiu2=="R92" | ciiu2=="R93"))
f7 <- with(inn,(ciiu2=="D35" | ciiu2=="E35" | ciiu2=="E36" | ciiu2=="E37" | ciiu2=="E38" | ciiu2=="E39"))
f8 <- with(inn,(ciiu2=="B05" | ciiu2=="B06" | ciiu2=="B07" | ciiu2=="B08"))
f9 <- with(inn,(ciiu2=="F41" | ciiu2=="F42" | ciiu2=="F43"))
f10 <- with(inn,(ciiu2=="A01" | ciiu2=="A02" | ciiu2=="A03"))

inn$sectoremp <- with(inn,ifelse (f1,1,sectoremp))
inn$sectoremp <- with(inn,ifelse (f2,2,sectoremp))
inn$sectoremp <- with(inn,ifelse (f3,3,sectoremp))
inn$sectoremp <- with(inn,ifelse (f4,4,sectoremp))
inn$sectoremp <- with(inn,ifelse (f5,5,sectoremp))
inn$sectoremp <- with(inn,ifelse (f6,6,sectoremp))
inn$sectoremp <- with(inn,ifelse (f7,7,sectoremp))
inn$sectoremp <- with(inn,ifelse (f8,8,sectoremp))
inn$sectoremp <- with(inn,ifelse (f9,9,sectoremp))
inn$sectoremp <- with(inn,ifelse (f10,10,sectoremp))


cbind(table(inn$sectoremp))


inn$sector_1 <- 0
inn$sector_1 <- with(inn,ifelse (sectoremp == 1,1,sector_1))
inn$sector_2 <- 0
inn$sector_2 <- with(inn,ifelse (sectoremp == 2,1,sector_2))
inn$sector_3 <- 0
inn$sector_3 <- with(inn,ifelse (sectoremp == 3,1,sector_3))
inn$sector_4 <- 0
inn$sector_4 <- with(inn,ifelse (sectoremp == 4,1,sector_4))
inn$sector_5 <- 0
inn$sector_5 <- with(inn,ifelse (sectoremp == 5,1,sector_5))
inn$sector_6 <- 0
inn$sector_6 <- with(inn,ifelse (sectoremp == 6,1,sector_6))
inn$sector_7 <- 0
inn$sector_7 <- with(inn,ifelse (sectoremp == 7,1,sector_7))
inn$sector_8 <- 0
inn$sector_8 <- with(inn,ifelse (sectoremp == 8,1,sector_8))
inn$sector_9 <- 0
inn$sector_9 <- with(inn,ifelse (sectoremp == 9,1,sector_9))


table(inn$sector_9)

# *******************CARACTERISTICAS DE LAS EMPRESAS

inn$publica <- inn$ii_3   #/*si la empresa es publica*/
inn$grupo <- inn$ii_1  #/*si pertenece a un grupo*/
  
table(inn$ii_2)
inn$mne <- 0
f <- (inn$ii_2!=1 & !is.na(inn$ii_2))
inn$mne <- with(inn,ifelse (f,1,mne))  #/*si es una multinacional extranjera*/

  
inn$edad <- 2014-inn$ii_6    #/*edad de la empresa*/
inn$exportadora <- 0
f <- with(inn, ii_9_b_2012>0 | ii_9_b_2013>0 | ii_9_b_2014>0)
inn$exportadora <- with(inn,ifelse (f,1,exportadora)) #/*si la empresa es exportadora*/

inn$ltamano <- with(inn,log(ii_9_d_2014))      #/*logaritmo tamano de la empresa*/
inn$lprod_trabajo <- with(inn,log(ii_9_a_2014/ii_9_d_2014))     #/*logaritmo productividad del trabajo*/
  
inn$ltamano <- with(inn,log(ii_9_d_2014))      #/*logaritmo tamano de la empresa*/
inn$lprod_trabajo <- with(inn,log(ii_9_a_2014/ ii_9_d_2014))     #/*logaritmo productividad del trabajo*/


# ********************OUTPUTS INNOVADORES

# /*por tipo y grado de novedad*/
inn$bien_rad <- inn$iii_1_a
inn$bien_inc <- inn$iii_1_c
inn$serv_rad <- inn$iii_1_b
inn$serv_inc <- inn$iii_1_d
inn$proc_rad <- inn$iv_1_a 
inn$proc_inc <- inn$iv_1_b

# /*por tipo y grado de novedad*/*
inn$producto <- 0
f <- with(inn, bien_rad==1 | bien_inc==1 | serv_rad==1 | serv_inc==1)
inn$producto <- with(inn,ifelse (f,1,producto))

inn$proceso <- 0
f <- with(inn, proc_rad==1 | proc_inc==1)
inn$proceso <- with(inn,ifelse (f,1,proceso))

# /*innovador exitoso*/

inn$exitoso <- 0
f <- with(inn, producto==1 | proceso==1)
inn$exitoso <- with(inn,ifelse (f,1,exitoso))

# /*organizacional y marketing*/
inn$org <- inn$xi_1
inn$mkt <- inn$xii_1

# /*variables continuas sobre la innovacion de productos*/
inn$vtas_rad <- inn$iii_3_a_ventas 
inn$vtas_inc <- inn$iii_3_b_ventas
inn$vtas_inn <- inn$vtas_rad+inn$vtas_inc


# ***********************INPUTS INNOVADORES

inn$id_int <- inn$v_1_a
inn$int_int=with(inn,v_1_a_2014/ ii_9_a_2014)

inn$id_ext <- inn$v_1_b
inn$int_ext <- with(inn,v_1_b_2014/ii_9_a_2014)


inn$id <- 0
f <- with(inn, v_1_a==1 | v_1_b==1)
inn$id <- with(inn,ifelse (f,1,id))

inn$int_id <- NA
inn$int_id <- with(inn,ifelse(ii_9_a_2014!=0,(v_1_a_2014+v_1_b_2014)/ii_9_a_2014,int_id))

inn$otros <- 0
f <- with(inn, v_4_a==1 | v_4_b==1 | v_4_c==1 | v_4_d==1 | v_4_e==1 | v_4_f==1 | v_4_g==1 | v_4_h==1 )
inn$otros <- with(inn,ifelse (f,1,otros))

inn$int_otros <- NA
inn$int_otros <- with(inn,ifelse(ii_9_a_2014!=0,(v_4_a_2014+v_4_b_2014+v_4_c_2014+v_4_d_2014+v_4_e_2014+v_4_f_2014+v_4_g_2014+v_4_h_2014)/ii_9_a_2014,int_id))

summary(inn$int_otros)
cbind(tapply(inn$int_otros,inn$cod_provincia,mean,na.rm =TRUE))

# library(survey)
# # stratified sample
# dstrat <- svydesign(id=~inn$id_empresa,strata=as.numeric(inn$cod_provincia), weights=inn$fexp)
# dstrat <- svydesign(id=~inn$id_empresa,weights=~inn$fexp)
# # one-stage cluster sample
# svymean(inn$cod_provincia~inn$int_otros,design = dstrat,na.rm = TRUE)

setwd("~/Documents/DataBase/BCE/Provinciales")
library(readxl)
yrs <- c(2012,2013,2014)
vab.tot <- NULL

#2012
ann <- 1
nom <- paste("Cab",yrs[ann],".xlsx",sep = "")
print(nom)
vab.aux <- read_excel(nom,range = "VAB TOTAL!A6:G227")
vab.aux <- as.data.frame(vab.aux)
vab <- aggregate(vab.aux$`VALOR AGREGADO BRUTO`,list(vab.aux$`CÓDIGO PROVINCIA`),sum)
colnames(vab) <- c("cod_provincia","vab2012")
vab12 <- vab
#2013
ann <- 2
nom <- paste("Cab",yrs[ann],".xlsx",sep = "")
print(nom)
vab.aux <- read_excel(nom,range = "VAB TOTAL!A6:G227")
vab.aux <- as.data.frame(vab.aux)
vab <- aggregate(vab.aux$`VALOR AGREGADO BRUTO`,list(vab.aux$`CÓDIGO PROVINCIA`),sum)
colnames(vab) <- c("cod_provincia","vab2013")
vab13 <- vab
#2014
ann <- 3
nom <- paste("Cab",yrs[ann],".xlsx",sep = "")
print(nom)
vab.aux <- read_excel(nom,range = "VAB TOTAL!A6:G227")
vab.aux <- as.data.frame(vab.aux)
vab <- aggregate(vab.aux$`VALOR AGREGADO BRUTO`,list(vab.aux$`CÓDIGO PROVINCIA`),sum)
colnames(vab) <- c("cod_provincia","vab2014")
vab14 <- vab

cbind(vab12,vab13,vab,14)





#*********************************** 2012 *****************************

# ***********************INPUTS INNOVADORES

# id_int12 <- inn$v_1_a
# int_int12 <- with(inn,v_1_a_2014/ ii_9_a_2012)
# 
# id_ext12 <- inn$v_1_b
# int_ext12 <- with(inn,v_1_b_2014/ii_9_a_2012)
# 
# 
# id12 <- 0
# f <- with(inn, v_1_a==1 | v_1_b==1)
# id12 <- ifelse (f,1,id12)

sum(with(inn,v_1_total_2012+v_1_total_2013+v_1_total_2014))
sum(inn$v_1_total_2012*inn$fexp)
sum(inn$v_1_total_2013*inn$fexp)
sum(inn$v_1_total_2014*inn$fexp)

cbind(tapply(inn$v_1_total_2014*inn$fexp,inn$cod_provincia,sum))

tapply(inn$v_1_total_2012*inn$fexp,inn$cod_provincia,sum)/vab12$vab2012
tapply(inn$v_1_total_2013*inn$fexp,inn$cod_provincia,sum)/vab13$vab2013
tapply(inn$v_1_total_2014*inn$fexp,inn$cod_provincia,sum)/vab14$vab2014


setwd("~/Documents/Consultorias&Cursos/DataLectures/SpatialData")



##**************************** 2012 ****************************

library(spdep)
library(RColorBrewer)
library(classInt)
library(maptools)

setwd("~/Documents/Consultorias&Cursos/DataLectures/SpatialData")

pr <- rgdal::readOGR("nxprovincias.shp")
pr <- pr[pr$DPA_PROVIN!=90,]
pr <- pr[pr$DPA_PROVIN!=20,]
wr <- poly2nb(pr, queen=TRUE)
wm <- nb2mat(wr, style='B', zero.policy = TRUE)
# 
AcrProv <- substr(pr$DPA_DESPRO,1,2)
AcrProv[3] <- "CR"
AcrProv[23] <- "SE"
AcrProv[12] <- "LR"
xy <- coordinates(pr)

# quartz()
# plot(pr, col='gray', border='blue')
# plot(wr, xy, col='red', lwd=2, add=TRUE)
# text(xy,AcrProv,cex=0.7)

# Creo lista de vecinos
wrl <- nb2listw(wr, style="W",zero.policy = TRUE) 
# Número de vecinos por área
i <- rowSums(wm)
vals <- i/sum(i)
moran.plot(vals,wrl)



farm.den07 <- tapply(inn$v_1_total_2012*inn$fexp,inn$cod_provincia,sum)/vab12$vab2012
# farm.den07 <- tapply(inn$v_1_total_2012*inn$fexp,inn$cod_provincia,sum)
farm.den07 <- farm.den07[-20]

# library(ggplot2)
# dd <- data.frame(farm.den07)
# ggplot(dd, aes(farm.den07)) +
#   geom_density()

pdf(file = "density2012.pdf")
par(mfrow = c(1,2))
plot(density((farm.den07)),main = "")
plot(density(log(farm.den07)),main = "")
par(mfrow = c(1,1))
dev.off()

farm.den07 <- log(farm.den07+1)
pdf(file = "moran2012.pdf")
moran.plot(as.vector((farm.den07)),wrl, xlab = "Gasto",ylab = "Autocorrelación Espacial")
dev.off()
# Mapping Farm Density in 2007
pal.red <- brewer.pal(5,"Reds")
q5.den <- classIntervals(farm.den07,5,style="quantile") 
cols.den <- findColours(q5.den, pal.red)

quartz()
pdf(file = "v_1_total2012.pdf")
plot(pr, col=cols.den)
text(xy,AcrProv,cex=0.7)
brks.den <- round(q5.den$brks,2)
leg.txt  <- paste(brks.den[-6], brks.den[-1], sep=" - ")
legend("bottomright", fill=attr(cols.den,"palette"), legend=leg.txt ,bty="n")
dev.off()



# View(pr@data)
pr.listw <- nb2listw(wr, style="B",zero.policy = TRUE)
moran(farm.den07, pr.listw, length(pr.listw$weights),Szero(pr.listw),zero.policy = FALSE)
# Spatial autocorrelation tests
moran.test(farm.den07, pr.listw)
geary.test(as.vector(farm.den07), pr.listw)


# Variogram
library(gstat)
pr.v <- variogram(farm.den07 ~ 1,  pr)
pr.v.fit <- fit.variogram(pr.v, vgm(11,"Sph", "30000", 1))
plot(pr.v, pr.v.fit)

vv <- vab12$vab2012[-20]
lm.farm <- lm(farm.den07 ~ log(vv))
summary(lm.farm)



lm.morantest(lm.farm,pr.listw, alternative = "greater", resfun = weighted.residuals)






##**************************** 2013 ****************************

farm.den07 <- tapply(inn$v_1_total_2012*inn$fexp,inn$cod_provincia,sum)/vab12$vab2012
# farm.den07 <- tapply(inn$v_1_total_2013*inn$fexp,inn$cod_provincia,sum)
farm.den07 <- farm.den07[-20]




pdf(file = "density2013.pdf")
par(mfrow = c(1,2))
plot(density((farm.den07)),main = "")
plot(density(log(farm.den07)),main = "")
par(mfrow = c(1,1))
dev.off()

farm.den07 <- log(farm.den07+1)
pdf(file = "moran2013.pdf")
moran.plot(as.vector((farm.den07)),wrl, xlab = "Gasto",ylab = "Autocorrelación Espacial")
dev.off()
# Mapping Farm Density in 2007
pal.red <- brewer.pal(5,"Reds")
q5.den <- classIntervals(farm.den07,5,style="quantile") 
cols.den <- findColours(q5.den, pal.red)

quartz()
pdf(file = "v_1_total2013.pdf")
plot(pr, col=cols.den)
text(xy,AcrProv,cex=0.7)
brks.den <- round(q5.den$brks,2)
leg.txt  <- paste(brks.den[-6], brks.den[-1], sep=" - ")
legend("bottomright", fill=attr(cols.den,"palette"), legend=leg.txt ,bty="n")
dev.off()



# View(pr@data)
pr.listw <- nb2listw(wr, style="W",zero.policy = TRUE)
moran(farm.den07, pr.listw, length(pr.listw$weights),Szero(pr.listw),zero.policy = FALSE)
# Spatial autocorrelation tests
moran.test(farm.den07, pr.listw)
geary.test(as.vector(farm.den07), pr.listw)


# Variogram
library(gstat)
pr.v <- variogram(farm.den07 ~ 1,  pr)
pr.v.fit <- fit.variogram(pr.v, vgm(11,"Sph", "30000", 1))
plot(pr.v, pr.v.fit)

vv <- vab13$vab2013[-20]
lm.farm <- lm((farm.den07+1) ~ log(vv))
summary(lm.farm)



mspace <- lm.morantest(lm.farm,pr.listw, alternative = "greater", resfun = weighted.residuals)
print(mspace)


moran.plot(as.vector(residuals(lm.farm)),pr.listw, quiet = TRUE)

# pr.listw <- nb2listw(wr, style="W",zero.policy = TRUE)
sp.correlogram(pr.listw, as.vector(residuals(lm.farm)), order = 8, method = "I",style = "B")
sol <- localmoran(as.vector(residuals(lm.farm)),pr.listw)
round(sol[,5]*100,2)





##**************************** 2014 ****************************

# farm.den07 <- tapply(inn$v_1_total_2012*inn$fexp,inn$cod_provincia,sum)/vab12$vab2012
farm.den07 <- tapply(inn$v_1_total_2014*inn$fexp,inn$cod_provincia,sum)/vab14$vab2014
farm.den07 <- farm.den07[-20]




pdf(file = "density2014.pdf")
par(mfrow = c(1,2))
plot(density((farm.den07)),main = "")
plot(density(log(farm.den07)),main = "")
par(mfrow = c(1,1))
dev.off()

farm.den07 <- log(farm.den07+1)
pdf(file = "moran2014.pdf")
moran.plot(as.vector((farm.den07)),wrl, xlab = "Gasto",ylab = "Autocorrelación Espacial")
dev.off()
# Mapping Farm Density in 2007
pal.red <- brewer.pal(5,"Reds")
q5.den <- classIntervals(farm.den07,5,style="quantile") 
cols.den <- findColours(q5.den, pal.red)

quartz()
pdf(file = "v_1_total2014.pdf")
plot(pr, col=cols.den)
text(xy,AcrProv,cex=0.7)
brks.den <- round(q5.den$brks,2)
leg.txt  <- paste(brks.den[-6], brks.den[-1], sep=" - ")
legend("bottomright", fill=attr(cols.den,"palette"), legend=leg.txt ,bty="n")
dev.off()


# View(pr@data)
pr.listw <- nb2listw(wr, style="W",zero.policy = TRUE)
moran(farm.den07, pr.listw, length(pr.listw$weights),Szero(pr.listw),zero.policy = FALSE)
# Spatial autocorrelation tests
moran.test(farm.den07, pr.listw)
geary.test(as.vector(farm.den07), pr.listw)


# Variogram
library(gstat)
pr.v <- variogram(farm.den07 ~ 1,  pr)
pr.v.fit <- fit.variogram(pr.v, vgm(11,"Sph", "30000", 1))
plot(pr.v, pr.v.fit)

vv <- vab13$vab2013[-20]
lm.farm <- lm((farm.den07+1) ~ log(vv))
summary(lm.farm)



mspace <- lm.morantest(lm.farm,pr.listw, alternative = "greater", resfun = weighted.residuals)
print(mspace)


moran.plot(as.vector(residuals(lm.farm)),pr.listw, quiet = TRUE)

# pr.listw <- nb2listw(wr, style="W",zero.policy = TRUE)
sp.correlogram(pr.listw, as.vector(residuals(lm.farm)), order = 8, method = "I",style = "B")
sol <- localmoran(as.vector(residuals(lm.farm)),pr.listw)
round(sol[,5]*100,2)
