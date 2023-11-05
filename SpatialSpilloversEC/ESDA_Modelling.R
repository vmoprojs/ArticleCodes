rm(list = ls())
cat("\014")
d_dir <- ("/Users/victormorales/Documents/Articles/CobbDouglas/Ordered/6. Bases de Datos/")
#Finales
Unificada <- paste(d_dir,"04 Finales/03 Base Unificada",sep = "")
#Resutlados
Resultados <-  paste(d_dir,"05 Resutaldos y Anexos",sep = "")


############################## 1. CREAR BASE GEOREFERENCIADA ##############################

#Se carga base de datos cantonales economicos
setwd(Unificada)

Base <- read.csv("Base.csv")
Base <- Base[,-1]

#Se carga shapefile con Georreferenciación de Cantones por Delimitación Territorial

library(sf)
canton_lim <- read_sf(paste(d_dir,"02 Brutas/01 Georreferenciación/ORGANIZACION TERRITORIAL DEL ESTADO CANTONAL.shp",sep = "")) # Se importa shape file con límites cantonales

canton_lim <- canton_lim[-224,] #Se elimina observación "Isla" que no genera vecinos.

canton_lim_B <- data.frame(canton_lim) #Se transforma a base de datos para modificar identificadores

canton_lim_B<- as.character(canton_lim_B$DPA_CANTON) # Se homogeniza identificador
canton_lim_B<- as.numeric(canton_lim_B)

canton_lim$DPA_CANTON <- canton_lim_B # Se une con shape file el identificador modificado

canton_lim <- canton_lim[-219:-223,] # Se eliminan cantones no delimitados

Base <- Base[-201:-203,] #Se eliminan cantones de galapagos

#Se conbinan las bases georrefenciada y y de datos economicos
proof <- sp::merge(canton_lim, Base, by.y = "Codigo",by.x = "DPA_CANTON",  all.y = T)  #Se unifica en una sola base con datos de geo referenciación

colnames(proof)[1] <- "Codigo"

#Se estiman matrices de pesos espaciales



############################## 2. MAPAS Y DESCRIPTIVOS ##############################

setwd(Resultados)

#2.1. Descriptivos

##### TABLA 2 Y 3 #####
library(spdep)
library(spatialreg)

v1 <- c("VAB2017_NP","Inversion_Priv","Inversion_Pub","PEA2017","PET2017" ,"POB2017")
v2 <- c("VABNPpp","IPbpp","IPrpp","l")

summary(proof[,v1])
summary(proof[,v2])

################################# 2.3. Maps. #################################

library(RColorBrewer)
library(gridExtra)
library(cowplot)

#VAB No Petrolero

c <- seq(0,max(proof$VAB2017_NP),max(proof$VAB2017_NP)/9)
c1 <- seq(0,max(proof$VABNPpp),max(proof$VABNPpp)/9)

pdf("VAB no Petrolero c.pdf")
plot(proof["VAB2017_NP"], 
     main = "Total", 
     breaks = c, nbreaks = 9, pal = brewer.pal(9, "Blues"), key.pos = 1)
plot(proof["VABNPpp"], 
     main = "Per Cápita", 
     breaks = c1, nbreaks = 9, pal = brewer.pal(9, "Blues"), key.pos = 1)
dev.off()

pdf("VAB no Petrolero q.pdf")
plot(proof["VAB2017_NP"], 
     main = "Total", 
     breaks = "quantile", nbreaks = 9, pal = brewer.pal(9, "Blues"), key.pos = 1)
plot(proof["VABNPpp"], 
     main = "Per Cápita", 
     breaks = "quantile", nbreaks = 9, pal = brewer.pal(9, "Blues"), key.pos = 1)
dev.off()

#Inversion Publica

c <- seq(0,max(proof$Inversion_Pub),max(proof$Inversion_Pub)/9)
c1 <- seq(0,max(proof$IPbpp),max(proof$IPbpp)/9)

pdf("Inversión Publica q.pdf")
plot(proof["Inversion_Pub"], 
     main = "Total", 
     breaks = "quantile", nbreaks = 9, pal = brewer.pal(9, "Oranges"), key.pos = 1)
plot(proof["IPbpp"], 
     main = "Per Cápita", 
     breaks = "quantile", nbreaks = 9, pal = brewer.pal(9, "Oranges"), key.pos = 1)
dev.off()

pdf("Inversión Publica c.pdf")
plot(proof["Inversion_Pub"], 
     main = "Total", 
     breaks = c, nbreaks = 9, pal = brewer.pal(9, "Oranges"))
plot(proof["IPbpp"], 
     main = "Per Cápita", 
     breaks = c1, nbreaks = 9, pal = brewer.pal(9, "Oranges"))
dev.off()

#Inversion Privada
c <- seq(0,max(proof$Inversion_Priv),max(proof$Inversion_Priv)/9)
c1 <- seq(0,max(proof$IPrpp),max(proof$IPrpp)/9)

pdf("Inversión Privada q.pdf")
plot(proof["Inversion_Priv"], 
     main = "Total", 
     breaks = "quantile", nbreaks = 6, pal = brewer.pal(6, "Purples"),key.pos = 1)
plot(proof["IPrpp"], 
     main = "Per Cápita", 
     breaks = "quantile", nbreaks = 6, pal = brewer.pal(6, "Purples"),key.pos = 1)
dev.off()

pdf("Inversión Privada c.pdf")
plot(proof["Inversion_Priv"], 
     main = "Total", 
     breaks = c, nbreaks = 9, pal = brewer.pal(9, "Purples"),key.pos = 1)
plot(proof["IPrpp"], 
     main = "Per Cápita", 
     breaks = c1, nbreaks = 9, pal = brewer.pal(9, "Purples"),key.pos = 1)
dev.off()

#Labor

c <- seq(0,max(proof$PET2017),max(proof$PET2017)/9)

pdf("Labor cq.pdf")
plot(proof["PET2017"], 
     main = "PET", 
     breaks = c, nbreaks = 9, pal = brewer.pal(9, "Greens"), key.pos = 1)
plot(proof["PET2017"], 
     main = "PET", 
     breaks = "quantile", nbreaks = 9, pal = brewer.pal(9, "Greens"), key.pos = 1)
dev.off()


c <- seq(0,max(proof$POB2017),max(proof$POB2017)/9)

pdf("Población cq.pdf")
plot(proof["POB2017"], 
     main = "Población", 
     breaks = c, nbreaks = 9, pal = brewer.pal(9, "Greens"), key.pos = 1)
plot(proof["POB2017"], 
     main = "Población", 
     breaks = "quantile", nbreaks = 9, pal = brewer.pal(9, "Greens"), key.pos = 1)
dev.off()

################### 3. ESTIMACION DE MATRICES DE PESOS ESPACIALES ##############################

#Matriz de contiguidad
library(spdep)
queen.w <- poly2nb(proof$geometry, row.names = proof$Codigo, queen = TRUE) #Se estima matriz de vencindad tipo queen
queen.wl <- nb2listw(queen.w, style ="W") # Se genera lista de vencidad

#Matriz de vecinos mas cercanso k=5
coords <- st_coordinates(st_centroid(proof$geometry))
kneigh <- knearneigh(coords, k = (nrow(coords)-1), longlat = FALSE)
cc <- nb2listw(knn2nb(kneigh))

k5neigh <- knearneigh(coords, k = 5, longlat = FALSE)
cc5 <- nb2listw(knn2nb(k5neigh))

#Matriz de vecinos mas cercanso k=10
k10neigh <- knearneigh(coords, k = 10, longlat = FALSE)
cc10 <- nb2listw(knn2nb(k10neigh))


# Matriz de pesos inversa
dist.mat <- as.matrix(dist(coords, method = "euclidean"))
dist.mat[1:5, 1:5]

dist.mat.inv <- 1 / dist.mat # 1 / d_{ij}
diag(dist.mat.inv) <- 0 # 0 in the diagonal
dist.mat.inv[1:5, 1:5]


# Matriz de pesos inversa estandarizada
id <- mat2listw(dist.mat.inv, style = "W", row.names = proof$Codigo)
summary(id)


########################## 4. PRUEBAS DE DEPENDENCIA ESPACIAL ##############################

#4.1. Pruebas generales con matriz de contiguidad

#Test de Moran para cada variable Y y X

##### TABLA 4 #####

mo1 <- moran.test(log(proof$VABNPpp), listw = queen.wl)
mo2 <- moran.test(log(proof$IPbpp), listw = queen.wl)
mo3 <- moran.test(log(proof$IPrpp), listw = queen.wl)
mo4 <- moran.test(log(proof$PET2017), listw = queen.wl)

rbind(c(mo1$estimate[1],mo1$p.value),c(mo2$estimate[1],mo2$p.value),c(mo3$estimate[1],mo3$p.value),c(mo4$estimate[1],mo4$p.value))

mo1norm <- moran.test(log(proof$VABNPpp), listw = queen.wl,randomisation=FALSE)
mo2norm <- moran.test(log(proof$IPbpp), listw = queen.wl,randomisation=FALSE)
mo3norm <- moran.test(log(proof$IPrpp), listw = queen.wl,randomisation=FALSE)
mo4norm <- moran.test(log(proof$PET2017), listw = queen.wl,randomisation=FALSE)

pol <- 999
set.seed(8519)
mo1mc <- moran.mc(log(proof$VABNPpp), listw = queen.wl,pol)
(Zmo1mc <- (mo1mc$statistic-mean(mo1mc$res))/sd(mo1mc$res))
mo2mc <- moran.mc(log(proof$IPbpp), listw = queen.wl,pol)
(Zmo2mc <- (mo2mc$statistic-mean(mo2mc$res))/sd(mo2mc$res))
mo3mc <- moran.mc(log(proof$IPrpp), listw = queen.wl,pol)
(Zmo3mc <- (mo3mc$statistic-mean(mo3mc$res))/sd(mo3mc$res))
mo4mc <- moran.mc(log(proof$PET2017), listw = queen.wl,pol)
(Zmo4mc <- (mo4mc$statistic-mean(mo4mc$res))/sd(mo4mc$res))


Sol1 <- t(rbind(c(mo1$estimate,mo1$statistic,mo1$p.value),
               c(mo1norm$estimate,mo1norm$statistic,mo1norm$p.value),
               c(mo1mc$statistic,mean(mo1mc$res),var(mo1mc$res),Zmo1mc,mo1mc$p.value)))
rownames(Sol1) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol1) <- c("Randomization","Normal","Monte Carlo")
Sol1

Sol2 <- t(rbind(c(mo2$estimate,mo2$statistic,mo2$p.value),
                c(mo2norm$estimate,mo2norm$statistic,mo2norm$p.value),
                c(mo2mc$statistic,mean(mo2mc$res),var(mo2mc$res),Zmo2mc,mo2mc$p.value)))
rownames(Sol2) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol2) <- c("Randomization","Normal","Monte Carlo")
Sol2


Sol3 <- t(rbind(c(mo3$estimate,mo3$statistic,mo3$p.value),
                c(mo3norm$estimate,mo3norm$statistic,mo3norm$p.value),
                c(mo3mc$statistic,mean(mo3mc$res),var(mo3mc$res),Zmo3mc,mo3mc$p.value)))
rownames(Sol3) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol3) <- c("Randomization","Normal","Monte Carlo")
Sol3

Sol4 <- t(rbind(c(mo4$estimate,mo4$statistic,mo4$p.value),
                c(mo4norm$estimate,mo4norm$statistic,mo4norm$p.value),
                c(mo4mc$statistic,mean(mo4mc$res),var(mo4mc$res),Zmo4mc,mo4mc$p.value)))
rownames(Sol4) <- c("MI","E[MI]","V[MI]","z-value","p-value")
colnames(Sol4) <- c("Randomization","Normal","Monte Carlo")
Sol4

#Grafico de Morán para cada variable Y y X descartando cantones atípicos

proof_SNQG <- subset(proof, (Codigo!=1701 & Codigo!=901))

queen2.w <- poly2nb(proof_SNQG$geometry, row.names = proof_SNQG$Codigo, queen = TRUE)
queen2.wl <- nb2listw(queen2.w, style ="W")

##### GRAFICO 9 #####

par(mfrow = c(2, 2)) 
am <- moran.plot(log(proof_SNQG$VABNPpp), listw = queen2.wl, xlab = "log(NOGVApc)", ylab = "Wx")
bm <- moran.plot(log(proof_SNQG$IPbpp), listw = queen2.wl, xlab = "log(PubCpc)", ylab = "Wx")
cm <- moran.plot(log(proof_SNQG$IPrpp), listw = queen2.wl, xlab = "log(PrivCpc)", ylab = "Wx")
dm <- moran.plot(log(proof_SNQG$l), listw = queen2.wl, xlab = "log(Labor)", ylab = "Wx")

(table(am$x>mean(am$x) & am$wx>mean(am$wx))[2]+
table(am$x<mean(am$x) & am$wx<mean(am$wx))[2])/length(am$x)

(table(bm$x>mean(bm$x) & bm$wx>mean(bm$wx))[2]+
    table(bm$x<mean(bm$x) & bm$wx<mean(bm$wx))[2])/length(bm$x)

(table(cm$x>mean(cm$x) & cm$wx>mean(cm$wx))[2]+
    table(cm$x<mean(cm$x) & cm$wx<mean(cm$wx))[2])/length(cm$x)

(table(dm$x>mean(dm$x) & dm$wx>mean(dm$wx))[2]+
    table(dm$x<mean(dm$x) & dm$wx<mean(dm$wx))[2])/length(dm$x)

#4.2. Test de Moran sobre rezagos del modelos

##### TABLA 5 Y GRAFICO 10 #####

par(mfrow = c(1,1)) 
modelo_pp.lm <- lm(log(VABNPpp)~log(IPbpp)+log(IPrpp)+log(l),data = Base) #Regresion 
col.moran_pp <- lm.morantest(modelo_pp.lm, queen.wl)
col.moran_pp 


moran.plot(modelo_pp.lm$residuals, listw = queen.wl, xlab = "Errors", ylab = "Wx")



##### TABLA 6 #####

y <- log(proof$VABNPpp)
x1 <- log(proof$IPbpp)
x2 <- log(proof$IPrpp)
x3 <- log(proof$l)


df_aux <- data.frame(y,x1,x2,x3)
summary(df_aux)
pairs(df_aux)

mod.lm <- lm(y~x1+x2+x3)
summary(mod.lm)
logLik(mod.lm)
sum(coef(mod.lm)[2:4])
LM <- lm.LMtests(mod.lm, queen.wl, test="all")
LM$LMerr
LM$LMlag

LMknn <- lm.LMtests(mod.lm, cc, test="all")
LMknn$LMerr
LMknn$LMlag

LMknn5 <- lm.LMtests(mod.lm, cc5, test="all")
LMknn5$LMerr
LMknn5$LMlag

LMknn10 <- lm.LMtests(mod.lm, cc10, test="all")
LMknn10$LMerr
LMknn10$LMlag

LM_id <- lm.LMtests(mod.lm, id, test="all")
LM_id$LMerr
LM_id$LMlag

W_Contiguity <- c(c(LM$LMlag$statistic,LM$LMlag$p.value),
                  c(LM$RLMlag$statistic,LM$RLMlag$p.value),
                  c(LM$LMerr$statistic,LM$LMerr$p.value),
                  c(LM$RLMerr$statistic,LM$RLMerr$p.value))

W_Contiguity1 <- c(c(LM$LMlag$statistic,LM$LMlag$p.value),
                  c(LM$RLMlag$statistic,LM$RLMlag$p.value),
                  c(LM$LMerr$statistic,LM$LMerr$p.value),
                  c(LM$RLMerr$statistic,LM$RLMerr$p.value),
                  c(LM$SARMA$statistic,LM$SARMA$p.value))

W_Distance <- c(c(LMknn$LMlag$statistic,LMknn$LMlag$p.value),
                c(LMknn$RLMlag$statistic,LMknn$RLMlag$p.value),
                c(LMknn$LMerr$statistic,LMknn$LMerr$p.value),
                c(LMknn$RLMerr$statistic,LMknn$RLMerr$p.value))

W_Distance1 <- c(c(LMknn$LMlag$statistic,LMknn$LMlag$p.value),
                c(LMknn$RLMlag$statistic,LMknn$RLMlag$p.value),
                c(LMknn$LMerr$statistic,LMknn$LMerr$p.value),
                c(LMknn$RLMerr$statistic,LMknn$RLMerr$p.value),
                c(LMknn$SARMA$statistic,LMknn$SARMA$p.value))

W_K5 <- c(c(LMknn5$LMlag$statistic,LMknn5$LMlag$p.value),
          c(LMknn5$RLMlag$statistic,LMknn5$RLMlag$p.value),
          c(LMknn5$LMerr$statistic,LMknn5$LMerr$p.value),
          c(LMknn5$RLMerr$statistic,LMknn5$RLMerr$p.value))

W_K51 <- c(c(LMknn5$LMlag$statistic,LMknn5$LMlag$p.value),
          c(LMknn5$RLMlag$statistic,LMknn5$RLMlag$p.value),
          c(LMknn5$LMerr$statistic,LMknn5$LMerr$p.value),
          c(LMknn5$RLMerr$statistic,LMknn5$RLMerr$p.value),
          c(LMknn5$SARMA$statistic,LMknn5$SARMA$p.value))

W_K10 <- c(c(LMknn10$LMlag$statistic,LMknn10$LMlag$p.value),
           c(LMknn10$RLMlag$statistic,LMknn10$RLMlag$p.value),
           c(LMknn10$LMerr$statistic,LMknn10$LMerr$p.value),
           c(LMknn10$RLMerr$statistic,LMknn10$RLMerr$p.value))

W_K101 <- c(c(LMknn10$LMlag$statistic,LMknn10$LMlag$p.value),
           c(LMknn10$RLMlag$statistic,LMknn10$RLMlag$p.value),
           c(LMknn10$LMerr$statistic,LMknn10$LMerr$p.value),
           c(LMknn10$RLMerr$statistic,LMknn10$RLMerr$p.value),
           c(LMknn10$SARMA$statistic,LMknn10$SARMA$p.value))

W_ID <- c(c(LM_id$LMlag$statistic,LM_id$LMlag$p.value),
          c(LM_id$RLMlag$statistic,LM_id$RLMlag$p.value),
          c(LM_id$LMerr$statistic,LM_id$LMerr$p.value),
          c(LM_id$RLMerr$statistic,LM_id$RLMerr$p.value))

W_ID1 <- c(c(LM_id$LMlag$statistic,LM_id$LMlag$p.value),
          c(LM_id$RLMlag$statistic,LM_id$RLMlag$p.value),
          c(LM_id$LMerr$statistic,LM_id$LMerr$p.value),
          c(LM_id$RLMerr$statistic,LM_id$RLMerr$p.value),
          c(LM_id$SARMA$statistic,LM_id$SARMA$p.value))

rbind(W_Contiguity,W_Distance,W_K5,W_K10,W_ID)
rbind(W_Contiguity1,W_Distance1,W_K51,W_K101,W_ID1)


# Wald Test 
summary(lm(y~x1+x2+x3))
sac <- sacsarlm(y~x1+x2+x3, listw = queen.wl,method = "eigen",type="sacmixed")
sdm <- lagsarlm(y~x1+x2+x3, listw = queen.wl,method = "eigen",type = "mixed")
slm <- lagsarlm(y~x1+x2+x3, listw = queen.wl,method = "eigen")
sem <- errorsarlm(y~x1+x2+x3, listw = queen.wl,method = "eigen")

summary(sac)


# Ho:absence of spatial dependence
Wald1.Sarlm(sac)
Wald1.Sarlm(sdm)
Wald1.Sarlm(slm)
Wald1.Sarlm(sem)

LR1.Sarlm(sac)
LR1.Sarlm(sdm)
LR1.Sarlm(slm)
LR1.Sarlm(sem)



# sac vs otros, no hay diferencias, se prefiere el otro:
LR.Sarlm(sac,slm)
LR.Sarlm(sac,sdm)
LR.Sarlm(sac,sem)

# sdm vs otros, no hay diferencias, se prefiere el otro:
LR.Sarlm(sdm,slm)
LR.Sarlm(sdm,sem)
LR.Sarlm(sem,sdm)

summary(sac)
summary(sdm)
summary(slm)
summary(sem)
LL <- NULL
##### TABLA 7 Y 8 #####
set.seed(8519)
{
  
  ols <- lm(y ~ x1+ x2+x3)
  summary(ols)
  
  x1_l <- lag.listw(queen.wl, x1) # rezago espacial de x
  x2_l <- lag.listw(queen.wl, x2) # rezago espacial de x
  x3_l <- lag.listw(queen.wl, x3) # rezago espacial de x
  
  slx <- lm(y~x1+x2+x3+x1_l+x2_l+x3_l)
  slx_Cont <- summary(slx)
  slx_Cont<-slx_Cont$coefficients[,c(1,4)]
  slx_Cont
  
  
  sdm <- lagsarlm(y~x1+x2+x3, listw = queen.wl, Durbin = T)
  sdm_Cont <- summary(sdm)
  Rho_sdm <- c(sdm_Cont$rho,as.numeric(sdm_Cont$LR1$p.value))
  sdm_Cont <- sdm_Cont$Coef[,c(1,4)]
  sdm_Cont <- rbind(sdm_Cont,Rho_sdm)
  sdm_Cont
  
  
  slm <- lagsarlm(y ~ x1 + x2+x3,
                  listw=queen.wl,
                  method = "MC", zero.policy=T)
  slm_Cont <- summary(slm)
  LL <- c(LL,as.numeric(slm$LL))
  Rho <- c(slm_Cont$rho,as.numeric(slm_Cont$LR1$p.value))
  slm_Cont <- slm_Cont$Coef[,c(1,4)]
  slm_Cont <- rbind(slm_Cont,Rho)
  slm_Cont
  
  SIGMA <- vcov(slm) # 3.4 sarrias, pag 67
  MU <- coef(slm)
  
  MASS::mvrnorm(n = 1000, MU, SIGMA)
  
  # spatialreg::impacts(slm,listw = queen.wl)
  
  sem <- errorsarlm(y ~ x1 + x2+x3,
                    
                    listw=queen.wl,
                    method = "MC", zero.policy=T)
  sem_Cont <- summary(sem)
  Lambda <- c(sem_Cont$lambda,as.numeric(sem_Cont$LR1$p.value))
  sem_Cont <- sem_Cont$Coef[,c(1,4)]
  sem_Cont <- rbind(sem_Cont,Lambda)
  sem_Cont
  
  ####***** KNN
  
  # K = 5
  slmKT5 <- lagsarlm(y ~ x1 + x2+x3,
                     
                     listw=cc5,
                     method = "MC", zero.policy=T)
  slmKT5_K5 <- summary(slmKT5)
  LL <- c(LL,as.numeric(slmKT5$LL))
  Rho <- c(slmKT5_K5$rho,as.numeric(slmKT5_K5$LR1$p.value))
  slmKT5_K5 <- slmKT5_K5$Coef[,c(1,4)]
  slmKT5_K5 <- rbind(slmKT5_K5,Rho)
  slmKT5_K5
  
  x1_l <- lag.listw(cc5, x1) # rezago espacial de x
  x2_l <- lag.listw(cc5, x2) # rezago espacial de x
  x3_l <- lag.listw(cc5, x3) # rezago espacial de x
  
  cor(cbind(x1,x1_l,x2,x2_l,x3,x3_l))
  
  slxK5 <- lm(y~x1+x2+x3+x1_l+x2_l+x3_l)
  slx_ContK5 <- summary(slxK5)
  slx_ContK5<-slx_ContK5$coefficients[,c(1,4)]
  slx_ContK5
  
  sdmKT5 <- lagsarlm(y~x1+x2+x3, listw = cc5, Durbin = T)
  sdmT5_K5_Cont <- summary(sdmKT5)
  Rho_sdmT5_K5 <- c(sdmT5_K5_Cont$rho,as.numeric(sdmT5_K5_Cont$LR1$p.value))
  sdmT5_K5_Cont <- sdmT5_K5_Cont$Coef[,c(1,4)]
  sdmT5_K5_Cont <- rbind(sdmT5_K5_Cont,Rho)
  sdmT5_K5_Cont
  
  
  semKT5 <- errorsarlm(y ~ x1 + x2+x3,
                       
                       listw=cc5,
                       method = "MC", zero.policy=T)
  semKT5_K5 <- summary(semKT5)
  Lambda <- c(semKT5_K5$lambda,as.numeric(semKT5_K5$LR1$p.value))
  semKT5_K5 <- semKT5_K5$Coef[,c(1,4)]
  semKT5_K5 <- rbind(semKT5_K5,Lambda)
  semKT5_K5
  
  
  # K = 10
  
  
  slmKT10 <- lagsarlm(y ~ x1 + x2+x3,
                      
                      listw=cc10,
                      method = "MC", zero.policy=T)
  slmKT5_K10 <- summary(slmKT10)
  LL <- c(LL,as.numeric(slmKT10$LL))
  Rho <- c(slmKT5_K10$rho,as.numeric(slmKT5_K10$LR1$p.value))
  slmKT5_K10 <- slmKT5_K10$Coef[,c(1,4)]
  slmKT5_K10 <- rbind(slmKT5_K10,Rho)
  slmKT5_K10
  
  x1_l <- lag.listw(cc10, x1) # rezago espacial de x
  x2_l <- lag.listw(cc10, x2) # rezago espacial de x
  x3_l <- lag.listw(cc10, x3) # rezago espacial de x
  
  slxK10 <- lm(y~x1+x2+x3+x1_l+x2_l+x3_l)
  slx_ContK10 <- summary(slxK10)
  slx_ContK10<-slx_ContK10$coefficients[,c(1,4)]
  slx_ContK10
  
  sdmKT10 <- lagsarlm(y~x1+x2+x3, listw = cc10, Durbin = T)
  sdmT5_K10_Cont <- summary(sdmKT10)
  Rho_sdmT5_K10 <- c(sdmT5_K10_Cont$rho,as.numeric(sdmT5_K10_Cont$LR1$p.value))
  sdmT5_K10_Cont <- sdmT5_K10_Cont$Coef[,c(1,4)]
  sdmT5_K10_Cont <- rbind(sdmT5_K10_Cont,Rho)
  sdmT5_K10_Cont
  
  semKT10 <- errorsarlm(y ~ x1 + x2+x3,
                        
                        listw=cc10,
                        method = "MC", zero.policy=T)
  semKT5_K10 <- summary(semKT10)
  Lambda <- c(semKT5_K10$lambda,as.numeric(semKT5_K10$LR1$p.value))
  semKT5_K10 <- semKT5_K10$Coef[,c(1,4)]
  semKT5_K10 <- rbind(semKT5_K10,Lambda)
  semKT5_K10
  
  
  # K = Total
  
  
  slmKT <- lagsarlm(y ~ x1 + x2+x3,
                    
                    listw=cc,
                    method = "MC", zero.policy=T)
  summary(slmKT)
  slmKT_T <- summary(slmKT)
  LL <- c(LL,as.numeric(slmKT$LL))
  Rho <- c(slmKT_T$rho,as.numeric(slmKT_T$LR1$p.value))
  slmKT_T <- slmKT_T$Coef[,c(1,4)]
  slmKT_T <- rbind(slmKT_T,Rho)
  slmKT_T
  
  x1_l <- lag.listw(cc, x1) # rezago espacial de x
  x2_l <- lag.listw(cc, x2) # rezago espacial de x
  x3_l <- lag.listw(cc, x3) # rezago espacial de x
  
  cor(cbind(x1,x2,x3,x1_l,x2_l,x3_l))
  
  slxKT <- lm(y~x1+x2+x3+x1_l+x2_l+x3_l)
  slx_ContKT <- summary(slxKT)
  slx_ContKT<-slx_ContKT$coefficients[,c(1,4)]
  slx_ContKT
  
  sdmKT <- lagsarlm(y~x1+x2+x3, listw = cc, Durbin = TRUE,method = "MC")
  sdmKT_T_Cont <- summary(sdmKT)
  Rho_sdmKT_T <- c(sdmKT_T_Cont$rho,as.numeric(sdmKT_T_Cont$LR1$p.value))
  sdmKT_T_Cont <- sdmKT_T_Cont$Coef[,c(1,4)]
  sdmKT_T_Cont <- rbind(sdmKT_T_Cont,Rho)
  sdmKT_T_Cont
  
  semKT <- errorsarlm(y ~ x1 + x2+x3,
                      
                      listw=cc,
                      method = "MC", zero.policy=T)
  
  semKT_T <- summary(semKT)
  Lambda <- c(semKT_T$lambda,as.numeric(semKT_T$LR1$p.value))
  semKT_T <- semKT_T$Coef[,c(1,4)]
  semKT_T <- rbind(semKT_T,Lambda)
  semKT_T
  
  
  ##### Inverse distance
  
  slmID <- lagsarlm(y ~ x1 + x2+x3,
                    listw=id,
                    method = "MC", zero.policy=T)
  
  slmID_I <- summary(slmID)
  Lambda <- c(slmID_I$lambda,as.numeric(slmID_I$LR1$p.value))
  slmID_I <- slmID_I$Coef[,c(1,4)]
  slmID_I <- rbind(slmID_I,Lambda)
  slmID_I
  
  LL <- c(LL,as.numeric(slmID$LL))
  
  x1_l <- lag.listw(id, x1) # rezago espacial de x
  x2_l <- lag.listw(id, x2) # rezago espacial de x
  x3_l <- lag.listw(id, x3) # rezago espacial de x
  
  slxID <- lm(y~x1+x2+x3+x1_l+x2_l+x3_l)
  slx_ContID <- summary(slxID)
  slx_ContID<-slx_ContID$coefficients[,c(1,4)]
  slx_ContID
  
  sdmID <- lagsarlm(y~x1+x2+x3, listw = id, Durbin = T)
  sdmID_I_Cont <- summary(sdmID)
  Rho_sdmID_I <- c(sdmID_I_Cont$rho,as.numeric(sdmID_I_Cont$LR1$p.value))
  sdmID_I_Cont <- sdmID_I_Cont$Coef[,c(1,4)]
  sdmID_I_Cont <- rbind(sdmID_I_Cont,Rho)
  sdmID_I_Cont
  
  
  semID <- errorsarlm(y ~ x1 + x2+x3,
                      
                      listw=id,
                      method = "eigen", zero.policy=T)
  semID_I <- summary(semID)
  Lambda <- c(semID_I$lambda,as.numeric(semID_I$LR1$p.value))
  semID_I <- semID_I$Coef[,c(1,4)]
  semID_I <- rbind(semID_I,Lambda)
  semID_I
  
  
}






sol_estSLM <- cbind(slm_Cont,slmKT5_K5,slmKT5_K10,slmKT_T,slmID_I)
colnames(sol_estSLM) <- c("Continuity","Cont_pval","Knn5","Knn5_pval",
                          "Knn10","Knn10_pval","KnnTot","KnnTot_pval",
                          "InverseDistance","InverseDistance_pval")
sol_estSEM <- cbind(sem_Cont,semKT5_K5,semKT5_K10,semKT_T,semID_I)
colnames(sol_estSEM) <- c("Continuity","Cont_pval","Knn5","Knn5_pval",
                          "Knn10","Knn10_pval","KnnTot","KnnTot_pval",
                          "InverseDistance","InverseDistance_pval")
sol_estSDM <- cbind(sdm_Cont,sdmT5_K5_Cont,sdmT5_K10_Cont,sdmID_I_Cont)
colnames(sol_estSDM) <- c("Continuity","Cont_pval","Knn5","Knn5_pval",
                          "Knn10","Knn10_pval","InverseDistance",
                          "InverseDistance_pval")
sol_estSLX<- cbind(slx_Cont,slx_ContK5,slx_ContK10, slx_ContID)
colnames(sol_estSLX) <- c("Continuity","Cont_pval","Knn5","Knn5_pval",
                          "Knn10","Knn10_pval",
                          "InverseDistance","InverseDistance_pval")



sol_estSLM
LL
sol_estSEM
sol_estSDM
sol_estSLX

# ***************************************************************
# *************************** Impacts ***************************
# ***************************************************************
##### TABLA 9 #####

library(coda)

#*** Continuity
W <- as(queen.wl, "CsparseMatrix")
trMatc <- trW(W, type="mult")
impactos_slm <- spatialreg::impacts(slm, tr=trMatc, R=1000, Q=5)
imp_slm <- summary(impactos_slm, zstats=TRUE,short = TRUE,reportQ = TRUE)
imp_slm$Qdirect_sum
imp_slm$pzmat

# x1
o <- 1
slm_cont_imp_x1 <- cbind(c(imp_slm$res$total[o],
                           imp_slm$res$direct[o],
                           imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                      imp_slm$pzmat[o,1],
                                                      imp_slm$pzmat[o,2]))


# x2
o <- 2
slm_cont_imp_x2 <- cbind(c(imp_slm$res$total[o],
                           imp_slm$res$direct[o],
                           imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                      imp_slm$pzmat[o,1],
                                                      imp_slm$pzmat[o,2]))

# x3
o <- 3
slm_cont_imp_x3 <- cbind(c(imp_slm$res$total[o],
                           imp_slm$res$direct[o],
                           imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                      imp_slm$pzmat[o,1],
                                                      imp_slm$pzmat[o,2]))

imp_cont <- rbind(slm_cont_imp_x1,slm_cont_imp_x2,slm_cont_imp_x3)





#*** K5
W <- as(cc5, "CsparseMatrix")
trMatc <- trW(W, type="mult")
set.seed(8519)
impactos_slm <- spatialreg::impacts(slmKT5, tr=trMatc, R=1000, Q=5)
imp_slm <- summary(impactos_slm, zstats=TRUE,short = TRUE)
imp_slm$pzmat

# x1
o <- 1
slm_k5_imp_x1 <- cbind(c(imp_slm$res$total[o],
                         imp_slm$res$direct[o],
                         imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                    imp_slm$pzmat[o,1],
                                                    imp_slm$pzmat[o,2]))


# x2
o <- 2
slm_k5_imp_x2 <- cbind(c(imp_slm$res$total[o],
                         imp_slm$res$direct[o],
                         imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                    imp_slm$pzmat[o,1],
                                                    imp_slm$pzmat[o,2]))

# x3
o <- 3
slm_k5_imp_x3 <- cbind(c(imp_slm$res$total[o],
                         imp_slm$res$direct[o],
                         imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                    imp_slm$pzmat[o,1],
                                                    imp_slm$pzmat[o,2]))

imp_k5 <- rbind(slm_k5_imp_x1,slm_k5_imp_x2,slm_k5_imp_x3)





#*** K10
W <- as(cc10, "CsparseMatrix")
trMatc <- trW(W, type="mult")
set.seed(8519)
impactos_slm <- spatialreg::impacts(slmKT10, tr=trMatc, R=1000, Q=5)
imp_slm <- summary(impactos_slm, zstats=TRUE,short = TRUE)
imp_slm$pzmat

# x1
o <- 1
slm_k10_imp_x1 <- cbind(c(imp_slm$res$total[o],
                          imp_slm$res$direct[o],
                          imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                     imp_slm$pzmat[o,1],
                                                     imp_slm$pzmat[o,2]))


# x2
o <- 2
slm_k10_imp_x2 <- cbind(c(imp_slm$res$total[o],
                          imp_slm$res$direct[o],
                          imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                     imp_slm$pzmat[o,1],
                                                     imp_slm$pzmat[o,2]))

# x3
o <- 3
slm_k10_imp_x3 <- cbind(c(imp_slm$res$total[o],
                          imp_slm$res$direct[o],
                          imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                     imp_slm$pzmat[o,1],
                                                     imp_slm$pzmat[o,2]))

imp_k10 <- rbind(slm_k10_imp_x1,slm_k10_imp_x2,slm_k10_imp_x3)



#*** KTotal
W <- as(cc, "CsparseMatrix")
trMatc <- trW(W, type="mult")
set.seed(8519)
impactos_slm <- spatialreg::impacts(slmKT, tr=trMatc, R=1000, Q=5)
imp_slm <- summary(impactos_slm, zstats=TRUE,short = TRUE)
imp_slm$pzmat

# x1
o <- 1
slm_ktot_imp_x1 <- cbind(c(imp_slm$res$total[o],
                           imp_slm$res$direct[o],
                           imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                      imp_slm$pzmat[o,1],
                                                      imp_slm$pzmat[o,2]))


# x2
o <- 2
slm_ktot_imp_x2 <- cbind(c(imp_slm$res$total[o],
                           imp_slm$res$direct[o],
                           imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                      imp_slm$pzmat[o,1],
                                                      imp_slm$pzmat[o,2]))

# x3
o <- 3
slm_ktot_imp_x3 <- cbind(c(imp_slm$res$total[o],
                           imp_slm$res$direct[o],
                           imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                      imp_slm$pzmat[o,1],
                                                      imp_slm$pzmat[o,2]))

imp_ktot <- rbind(slm_ktot_imp_x1,slm_ktot_imp_x2,slm_ktot_imp_x3)



#*** Inverse distance
W <- as(id, "CsparseMatrix")
trMatc <- trW(W, type="mult")
set.seed(8519)
impactos_slm <- spatialreg::impacts(slmID, tr=trMatc, R=1000, Q=5)
imp_slm <- summary(impactos_slm, zstats=TRUE,short = TRUE)
imp_slm$pzmat

# x1
o <- 1
slm_id_imp_x1 <- cbind(c(imp_slm$res$total[o],
                         imp_slm$res$direct[o],
                         imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                    imp_slm$pzmat[o,1],
                                                    imp_slm$pzmat[o,2]))


# x2
o <- 2
slm_id_imp_x2 <- cbind(c(imp_slm$res$total[o],
                         imp_slm$res$direct[o],
                         imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                    imp_slm$pzmat[o,1],
                                                    imp_slm$pzmat[o,2]))

# x3
o <- 3
slm_id_imp_x3 <- cbind(c(imp_slm$res$total[o],
                         imp_slm$res$direct[o],
                         imp_slm$res$indirect[o]),c(imp_slm$pzmat[o,3],
                                                    imp_slm$pzmat[o,1],
                                                    imp_slm$pzmat[o,2]))

imp_id <- rbind(slm_id_imp_x1,slm_id_imp_x2,slm_id_imp_x3)



sol_impacts <- cbind(imp_cont,imp_k5,imp_k10,imp_ktot,imp_id)
rownames(sol_impacts) <- c("Kpriv_Total","Kpriv_Directo","Kpriv_Indirecto",
                           "Kpub_Total","Kpub_Directo","Kpub_Indirecto",
                           "L_Total","L_Directo","L_Indirecto")
colnames(sol_impacts) <- c("Cont_Estimate","Cont_pval",
                           "K5_Estimate","K5_pval",
                           "K10_Estimate","K10_pval",
                           "KTot_Estimate","KTot_pval",
                           "ID_Estimate","ID_pval")
sol_impacts



#Efectos Fila y Columna

##### GRAFICO 11 TABLA 10 Y 11 #####


#Función para determinar efectos marginales de los modelos

W <- listw2mat(queen.wl)                  # Matriz de distancias por contiguidad en formato matriz
colnames(W) <- rownames(W)                # Se asignan nombres de filas a columna

margin.effect <- function(modelo, W, b, first, Durbin="False") 
  
  #W es matriz de pesos en formato matriz, b es el número del coeficiente que se desdea estima
  # first es el tamaño del top de cantones, Durbin especifica si el modelo es DURBIN o Lag
{
  
  if(Durbin=="False"){                             #Si es un modelo lag se extraen coeficiente de X
    coef <- data.frame(modelo$coefficients)         #Se estraen coeficientes beta del modelo lag
    beta  <-coef[b+1,1]                             #Se toma el coeficiente número b sin contar el intercepto
  } else {                                        #Si es un modelo Durbin se toma coeficiente de X y lag de X                   
    coef  <- data.frame(modelo$coefficients)        #Se estraen coeficientes beta del modelo Durbin
    beta  <-coef[b+1,1]                             #Coeficiente de X
    beta2  <-coef[b+4,1]                            #Coeficiente de lag de X
  }
  
  if(Durbin=="False"){                             #Condicional para modelo Lag y Durbin
    A <- solve(diag(nrow(W)) - modelo$rho*W)      #Se estima matriz A de modelo Lag
    S <- A%*%diag(nrow(W))*beta                 #Se estima matriz S de modelo Lag
    
  } else {
    A <- solve(diag(nrow(W)) - modelo$rho*W)    #Se estima matriz A de modelo Durbin
    S <- A%*%(diag(nrow(W))*beta + W*beta2)     #Se estima matriz S de modelo Durbin
  }
  
  colnames(S) <- rownames(S)                      #Se asigna código de cantón a las columnas
  
  I <- matrix(rep(1, nrow(W)), nrow = 1)          #Se estiman los efectos totales, indirectos y directos
  
  e.total  <- I%*%S%*%t(I)/nrow(S)
  e.dirct  <- sum(diag(S))/nrow(S)
  e.indi   <- e.total - e.dirct
  
  impa <- data.frame(c(e.dirct,e.indi,e.total))   #Se crea base de efectos
  rownames(impa) <- c("efecto directo","efecto indirecto","efecto total")
  colnames(impa) <- c("valor")
  
  #Efecto Columna por Cantón
  
  E.ind.col <- matrix(NA,nrow = ncol(S))         #Se crea contenedor de efectos columna
  
  for (i in 1:ncol(S)) 
  {
    E.ind.col[i] <- sum(S[,i])-S[i,i]           #Se estiman efectos columna
  }
  # head(colSums(S)-diag(S))
  # head(as.numeric(E.ind.col))
  
  rownames(E.ind.col) <- rownames(S)            #Se asigna el codigo de cantón a los efectos columna
  
  datos_ordenados.col <- data.frame(E.ind.col[order(E.ind.col),]) #Se ordena por magnitud del efecto
  datos_ordenados.col <- E.ind.col #Se ordena por magnitud del efecto
  colnames(datos_ordenados.col)[1] <- "Efectos"                      
  principales.col <- tail(datos_ordenados.col, first)             #Se toman los primero "first" cantones con mayor efecto
  bar.col<- barplot(as.numeric(principales.col), names.arg = rownames(principales.col), ylab = "Valores", xlab = "Categorías", main = "Efecto indirecto sobre otros cantones")
  
  #Efecto Fila por Cantón
  
  E.ind.row <-matrix(NA,nrow = ncol(S))         #Se crea contenedor de efectos columna
  
  for (i in 1:nrow(S)) 
  {
    E.ind.row[i] <- sum(S[i,])-S[i,i]           #Se estiman efectos columna
  }
  
  rownames(E.ind.row) <- rownames(S)            #Se asigna el codigo de cantón a los efectos columna
  
  datos_ordenados.row <- data.frame(E.ind.row[order(E.ind.row),]) #Se ordena por magnitud del efecto
  datos_ordenados.row <- E.ind.row #Se ordena por magnitud del efecto
  colnames(datos_ordenados.row)[1] <- "Efectos"                      
  principales.row <- tail(datos_ordenados.row, first)             #Se toman los primero "first" cantones con mayor efecto
  bar.row<- barplot(as.numeric(principales.row), names.arg = rownames(principales.row), ylab = "Valores", xlab = "Categorías", main = "Efecto indirecto de otros cantones sobre el canton")
  
  return(list(A=A, S=S, efectos = impa,
              plot.col = bar.col, efectos.margin.col = datos_ordenados.col, efectos.margin.col.first=principales.col,
              plot.row = bar.row, efectos.margin.row = datos_ordenados.row, efectos.margin.row.first=principales.row))
}

#Efectos en el modelo Lag
Lag.effect <- margin.effect(slm, W, 2, 20, Durbin="False")   #Se obtienen los efectos marginales del modelo lag                

#Para comprobar comparamos los efectos directos, indirectos y totales con la funcion impacts

Lag.effect$efectos                     #Estimación con la funcion creada
impacts(slm, listw = queen.wl)    #Estimación con la función impats

#Efectos Columna
Lag.effect.col <- Lag.effect$efectos.margin.col
columna <- data.frame(Lag.effect.col)
columna$Codigo <- rownames(Lag.effect.col)

#Efectos Fila
Lag.effect.row <- Lag.effect$efectos.margin.row
fila <- data.frame(Lag.effect.row)
fila$Codigo <- rownames(Lag.effect.row)

proof <- sp::merge(proof, columna, by = "Codigo", all.x = T)  #Se unifica en una sola base con datos de geo referenciación
colnames(proof)[33] <- "Efecto Columna"
proof <- sp::merge(proof, fila, by = "Codigo", all.x = T)
colnames(proof)[34] <- "Efectos Fila"

Efectos <- proof[, -(9:21)] #Se mantienen variables de efectos, tamano poblacional y economico
Efectos <- Efectos[, -(5:7)]
Efectos <- Efectos[, -(7:16)]
Efectos <- data.frame(Efectos)
Efectos <- Efectos[, -9]

#Se exporta resultados de Efectos
#****write.csv(Efectos, "efectos.csv")

#Se realiza mapa de efectos

pdf("Efectos.pdf")
plot(proof["Efecto Columna"], 
     main = "Efectos Columna (hacia cantones externos)", 
     breaks = "quantile", nbreaks = 9, pal = brewer.pal(9, "Reds"), key.pos = 1)
plot(proof["Efectos Fila"], 
     main = "Efectos Fila (desde cantones externos)", 
     breaks = "quantile", nbreaks = 9, pal = brewer.pal(9, "Reds"), key.pos = 1)
dev.off()


