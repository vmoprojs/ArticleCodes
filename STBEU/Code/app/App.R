rm(list = ls())
graphics.off()
cat("\014")
library(GeoModels)
library(STBEU)
library(scatterplot3d)
# devtools::install_github("andrewzm/STRbook")
data("Medwind_data",package = "STRbook")

# plot(density(unlist(Edat$EUdat)),main = "Data")

# The data here correspond to 28 time periods 
# from 00:00 UTC on 29 January 2005 to 18:00
# UTC on 04 February 2005 (every 6 hours).

# EUdat: Data frame containing the east–west (u) component of the ECMWF
# wind vector (in units of m/s) in time-wide format

coords = Edat$ECMWFxylocs

datos = matrix(unlist(Edat$EUdat),ncol = ncol(Edat$EUdat),nrow = nrow(Edat$EUdat))
datos = t(datos)


coords_ll = coords #lon-lat coords

prj = mapproj::mapproject(coords[ ,1],coords[ ,2],projection ="sinusoidal")
coords = cbind(prj$x,prj$y) # Projected coords
coords = coords*6371

time <- 1:nrow(datos)

#*********************************** sep = 0.0 *****************************#

#### **************************    Estimation

################################################
#   parameters for the subsampling ####
###############################################
coordx=coords[,1]
coordy=coords[,2]
LX=abs(range(coordx)[1]-range(coordx)[2])
LY=abs(range(coordy)[1]-range(coordy)[2])
lato_fin=400  #changing window size
lx=lato_fin          #lunghezza lato x quadrato subfinestra
ly=lato_fin          #lunghezza lato y quadrato subfinestra
winc=c(lx/sqrt(LX),ly/sqrt(LY))
winstp= 1###   1/lato_fin complete overlapping in space  1 "no" overlapping in space
###############################################
winc_t=6   ###  length of temporal window
winstp_t=1 ###   0.5 half overlapping  1 "no" overlapping

(maxdist <-  40)
(maxtime <- 1*winc_t)
###############################################
weighted=0
type_dist=1 ### type of distance 1:euclidean
type_subs=1 ### type of subsampling  1=in space    2= in time


smooth_t=0
scale_t=20
scale_s=350
power2_t=3.5+smooth_t  +1
power_s=2
power2_s=2.5+2*smooth_t
sep=0

sill=var(c(datos),na.rm = TRUE)
nugget=0
mean=mean(datos,na.rm=TRUE)

start=list(scale_s=scale_s,scale_t=scale_t,sill=sill)
fix=c( nugget=nugget,power_s=power_s,mean = mean,
       power2_s=power2_s,
       power2_t=power2_t,smooth_t=smooth_t,
       sep=sep)
param <- c(start,fix)

summary(dist(coords))
max(dist(coords))/maxdist
max(dist(time))/maxtime
summary(dist(time))
cc = 3



sol <- list()


#2 : STBEU in OpenCL framework with CPU
tGPU1 = proc.time()
res1_0=STBEUFit(start,fix,coords,time,cc,datos,
              type_dist,maxdist ,maxtime,
              winc,winstp,0,0,type_subs,weighted
              # ,GPU =0, local = c(1,1)
              ,varest = TRUE
              )
tGPU1 = proc.time()-tGPU1
print(tGPU1)
print(res1_0$par)


###### pairwise likelihood ########
fixed=as.list(fix)
gTime = proc.time()
res2_0=GeoFit(data=datos,coordx=coords,coordt=time,
            corrmodel="Wen_time",
            start=start,fixed=fixed,
            maxdist=maxdist ,maxtime=maxtime,varest = TRUE)
gTime = proc.time()-gTime


res1_0$par
res1_0$stderr #STBEU
res2_0$param
res2_0$stderr #PL




#*********************************** sep = 0.5 *****************************#
sep=0.5
start=list(scale_s=scale_s,scale_t=scale_t,sill=sill)
fix=c( nugget=nugget,power_s=power_s,mean = mean,
       power2_s=power2_s,
       power2_t=power2_t,smooth_t=smooth_t,
       sep=sep)


tGPU1 = proc.time()
res1_05=STBEUFit(start,fix,coords,time,cc,datos,
                type_dist,maxdist ,maxtime,
                winc,winstp,0,0,type_subs,weighted
                # ,GPU =0, local = c(1,1)
                ,varest = TRUE
                )
tGPU1 = proc.time()-tGPU1
print(tGPU1)


###### pairwise likelihood ########
fixed=as.list(fix)

res3_05 = GeoFit(data=datos,coordx=coords,coordt=time,
                corrmodel="Wen_time",
                start=start,fixed=fixed,
                maxdist=maxdist ,maxtime=maxtime,varest = TRUE)


res1_05$par;res1_05$stderr
res3_05$param;res3_05$stderr




#*********************************** sep = 1**********************************#
sep=1
start=list(scale_s=scale_s,scale_t=scale_t,sill=sill)
fix=c( nugget=nugget,power_s=power_s,mean = mean,
       power2_s=power2_s,
       power2_t=power2_t,smooth_t=smooth_t,
       sep=sep)


tGPU1 = proc.time()
res1_1=STBEUFit(start,fix,coords,time,cc,datos,
                 type_dist,maxdist ,maxtime,
                 winc,winstp,0,0,type_subs,weighted
                 # ,GPU =0, local = c(1,1)
                 ,varest = TRUE
                )
tGPU1 = proc.time()-tGPU1
print(tGPU1)


###### pairwise likelihood ########
fixed=as.list(fix)

res3_1 = GeoFit(data=datos,coordx=coords,coordt=time,
                 corrmodel="Wen_time",
                 start=start,fixed=fixed,
                 maxdist=maxdist ,maxtime=maxtime,varest = TRUE)

res1_0$value
res2_0$logCompLik

res1_05$value
res3_05$logCompLik

res1_1$value
res3_1$logCompLik

res1_0$value
res2_0$logCompLik

res1_0$par;res1_0$stderr
res2_0$param;res2_0$stderr


res1_05$par;res1_05$stderr
res3_05$param;res3_05$stderr

res1_1$par;res1_1$stderr
res3_1$param;res3_1$stderr





############################################################################
#### ************************ VARIOGRAM ********************************####
############################################################################
maxdist <-  1500
maxtime <- 18
time <- 1:nrow(datos)


### checking model assumptions: ST variogram model


time = 1:nrow(datos)
ttV = proc.time()
fit = GeoVariogram(data=(datos),
                   coordx=coords, coordt=time,
                   maxdist=maxdist,maxtime=maxtime)
(ttV = proc.time()-ttV) #variogram estimation time

# user  system elapsed 
# 371.382   0.910 373.240 

#### **************************    Estimation

################################################
#   parameters for the subsampling ####
###############################################
coordx=coords[,1]
coordy=coords[,2]
LX=abs(range(coordx)[1]-range(coordx)[2])
LY=abs(range(coordy)[1]-range(coordy)[2])
lato_fin=400  #changing window size
lx=lato_fin          #lunghezza lato x quadrato subfinestra
ly=lato_fin          #lunghezza lato y quadrato subfinestra
winc=c(lx/sqrt(LX),ly/sqrt(LY))
winstp= 1###   1/lato_fin complete overlapping in space  1 "no" overlapping in space
###############################################
winc_t=6   ###  length of temporal window
winstp_t=1 ###   0.5 half overlapping  1 "no" overlapping

(maxdist <-  40)
(maxtime <- 1*winc_t)
###############################################

#################################################################
weighted=0
type_dist=1 ### type of distance 1:euclidean
type_subs=1 ### type of subsampling  1=in space    2= in time


smooth_t=0
scale_t=20
scale_s=350
power2_t=3.5+smooth_t  +1
power_s=2
power2_s=2.5+2*smooth_t
sep=0.5

sill=var(c(datos),na.rm = TRUE)
nugget=0
mean=mean(datos,na.rm=TRUE)
# mean = 0

start=list(scale_s=scale_s,scale_t=scale_t,sill=sill)
fix=c( nugget=nugget,power_s=power_s,mean = mean,
       power2_s=power2_s,
       power2_t=power2_t,smooth_t=smooth_t,
       sep=sep)


summary(dist(coords))
max(dist(coords))/maxdist
max(dist(time))/maxtime
summary(dist(time))
cc = 3


#maxdist1=50
#maxtime1=3


maxdist1=40#50
maxtime1=6

#2 : STBEU in OpenCL framework with CPU
tGPU1 = proc.time()
res3=STBEUFit(start,fix,coords,time,cc,datos,
              type_dist,maxdist1 ,maxtime1,
              winc,winstp,0,0,type_subs,weighted,varest = TRUE)
tGPU1 = proc.time()-tGPU1
print(tGPU1)

###### pairwise likelihood ########
fixed=as.list(fix)
gTime = proc.time()
res2=GeoFit(data=datos,coordx=coords,coordt=time,
            corrmodel="Wen_time",
            start=start,fixed=fixed,
            maxdist=maxdist1 ,maxtime=maxtime1,varest = TRUE)
gTime = proc.time()-gTime

# -------------------------- ****** PLOTS: 1 ******* --------------------------#

S=res2$param['scale_s']
T=res2$param['scale_t']
V=res2$param['sill']

sc_t=res3$par['scale_t']
sc_s=res3$par['scale_s']
vv=res3$par['sill']
tt=seq(0,max(fit$bint),0.1)
ss=seq(0,max(fit$centers),1)


par(mfrow=c(1,2))
# Results: Marginal spatial empirical  semi-variogram
plot(fit$centers, fit$variograms, xlab='h', ylab=expression(gamma(h)),
     ylim=c(0, max(fit$variograms)), xlim=c(0, max(fit$centers)),
     pch=20,main="Marginal spatial semi-variogram",cex.axis=.8)

lines(ss,vv*(1 - 1/(1+ss/sc_s)^(power2_s))) #STBEU
lines(ss,V*(1 - 1/(1+ss/S)^(power2_s)),lty=2) #PL


# Results: Marginal temporal empirical  semi-variogram
plot(fit$bint, fit$variogramt, xlab='t', ylab=expression(gamma(t)),
     ylim=c(0, max(fit$variogramt)),xlim=c(0,max(fit$bint)),
     pch=20,main="Marginal temporal semi-variogram",cex.axis=.8)

lines(tt,vv*(1-(1-tt/sc_t)^(power2_t)*I(tt<sc_t)))#STBEU
lines(tt,V*(1-(1-tt/T)^(power2_t)*I(tt<T)),lty=2)#PL
par(mfrow=c(1,1))






# -------------------------- ****** PLOTS: 2 ******* --------------------------#
NT <- nrow(coords)*length(time)
alpha = 0.975

S=res2$param['scale_s']
T=res2$param['scale_t']
V=res2$param['sill']

S.1=res2$param['scale_s']+qt(alpha,NT)*res2$stderr['scale_s']
T.1=res2$param['scale_t']+qt(alpha,NT)*res2$stderr['scale_t']
V.1=res2$param['sill']+qt(alpha,NT)*res2$stderr['sill']
V.1=res2$param['sill']+qt(alpha,NT)*1

S.2=res2$param['scale_s']-qt(alpha,NT)*res2$stderr['scale_s']
T.2=res2$param['scale_t']-qt(alpha,NT)*res2$stderr['scale_t']
V.2=res2$param['sill']-qt(alpha,NT)*res2$stderr['sill']
V.2=res2$param['sill']-qt(alpha,NT)*1

sc_t=res3$par['scale_t']
sc_s=res3$par['scale_s']
vv=res3$par['sill']

sc_t.1=res3$par['scale_t']+qt(alpha,NT)*res3$stderr['scale_t']
sc_s.1=res3$par['scale_s']+qt(alpha,NT)*res3$stderr['scale_s']
vv.1=res3$par['sill']+qt(alpha,NT)*res3$stderr['sill']
vv.1=res3$par['sill']+qt(alpha,NT)*1

sc_t.2=res3$par['scale_t']-qt(alpha,NT)*res3$stderr['scale_t']
sc_s.2=res3$par['scale_s']-qt(alpha,NT)*res3$stderr['scale_s']
vv.2=res3$par['sill']-qt(alpha,NT)*res3$stderr['sill']
vv.2=res3$par['sill']-qt(alpha,NT)*1


tt=seq(0,max(fit$bint),0.1)
ss=seq(0,max(fit$centers),1)


setwd("~/Documents/Articles/STBEU/STBEU-03-12-20/Figures")
pdf(file = "variCI.pdf",
    width = 12)

par(mfrow=c(1,2))
# Results: Marginal spatial empirical  semi-variogram
plot(fit$centers, fit$variograms, xlab='h', ylab=expression(gamma(h)),
     ylim=c(0, max(fit$variograms)+4), xlim=c(0, max(fit$centers)),
     pch=20,main="Marginal spatial semi-variogram",cex.axis=.8)

lu <- vv.1*(1 - 1/(1+ss/sc_s.1)^(power2_s))
li <- vv.2*(1 - 1/(1+ss/sc_s.2)^(power2_s))

# lines(ss,vv*(1 - 1/(1+ss/sc_s)^(power2_s))) #STBEU
lines(ss,lu) #STBEU+
lines(ss,li) #STBEU-

polygon(c(ss,rev(ss)),c(lu,rev(li))
        ,col=gray(0.8,alpha = 0.1),border = NA)

lupl = V.1*(1 - 1/(1+ss/S.1)^(power2_s))
lipl = V.2*(1 - 1/(1+ss/S.2)^(power2_s))

lines(ss,lupl,lty=2) #PL+
lines(ss,lipl,lty=2) #PL-

polygon(c(ss,rev(ss)),c(lupl,rev(lipl))
        ,col=gray(0.6,alpha = .1), border = NA)

# Results: Marginal temporal empirical  semi-variogram
plot(fit$bint, fit$variogramt, xlab='t', ylab=expression(gamma(t)),
     ylim=c(0, max(fit$variogramt)+4),xlim=c(0,max(fit$bint)),
     pch=20,main="Marginal temporal semi-variogram",cex.axis=.8)

lu = vv.1*(1-(1-tt/sc_t.1)^(power2_t)*I(tt<sc_t.1))
lu[is.na(lu)] <- lu[min(which(is.na(lu)))-1]
li = vv.2*(1-(1-tt/sc_t.2)^(power2_t)*I(tt<sc_t.2))
li[is.na(li)] <- li[min(which(is.na(li)))-1]


lines(tt,lu)#STBEU+
lines(tt,li)#STBEU-

polygon(c(tt,rev(tt)),c(lu,rev(li))
        ,col=gray(0.8,alpha = 0.1),border = NA)

lupl = V.1*(1-(1-tt/T)^(power2_t)*I(tt<T))
lipl = V.2*(1-(1-tt/T)^(power2_t)*I(tt<T))

lines(tt,lupl,lty=2)#PL+
lines(tt,lipl,lty=2)#PL-

polygon(c(tt,rev(tt)),c(lupl,rev(lipl))
        ,col=gray(0.6,alpha = .1), border = NA)
par(mfrow=c(1,1))
dev.off()

