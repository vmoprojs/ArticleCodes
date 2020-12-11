#********************************************** REGULAR
# ***************** REGULAR SPACE 1.2
rm(list=ls())
setwd("~/Documents/Articles/Coding/STBEU/Data/spacetime")
# mse<-function(x,a)  (var(x)+(mean(x)-a)^2) # Table 10
mse<-function(x,a)  median(abs(x-a)) # Table 4

#*********** ST Discarded functions:
# mse<-function(x,a)
# {
#   qq = quantile(x)
#   xstar = x[x>qq[2] & x<qq[4]]
#   return(var(xstar)+(mean(xstar)-a)^2)
# }
# mse<-function(x,a)
# {
#   qq = quantile(x, probs = c(0.05,0.95))
#   xstar = as.numeric(dist(qq))
#   return(xstar)
# }
# mse<-function(x,a)  sqrt(var(x)+(mean(x)-a)^2)
# mse<-function(x,a)  sum(log(cosh(x-a)))
#*********** END Discarded functions:

dat1 <- read.csv("regular_results_spacetimeDE15.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.2/3,1.2/3,1) #ST1
param <- c(3/3,3/3,1) #ST2
param <- c(3/3,3/3,1) #ST3 con lat y winc_t iguales
param <- c(.5/3,4/3,1) #ST3 con lat y winc_t iguales
param <- c(3.1/3,3.1/3,1) #ST3 con lat y winc_t iguales

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario
table(dat1$setting)

# **************************scenario 0/4A main NOT weighted estimation***
# **************************scenario 1/4 b = 2, no overlaping  ***
# **************************scenario 2/4 b = 4, no overlaping ***
# **************************scenario 3/4 b = 2, overlaping (.5) ***
# **************************scenario 4/4 b = 4, overlaping (.5) ***

# boxplot(dat1$mean~dat1$setting)
boxplot(dat1$scale_s~dat1$setting)
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~dat1$setting)
abline(h = param[2],col="blue")
boxplot(dat1$sill~dat1$setting)
abline(h = param[3],col="blue")


# MSE by PARAM and Scenario:
par.mse = NULL
for(i in 1:3)
{
  par.mse = cbind(par.mse,tapply(dat1[,names.param[i]],dat1[,"setting"],mse,param[i]))
}
colnames(par.mse) = names.param
ratio.par.mse = cbind(par.mse[1,1]/par.mse[,1],
par.mse[1,2]/par.mse[,2],
par.mse[1,3]/par.mse[,3])
colnames(ratio.par.mse) = names.param
#*****
a1 = ratio.par.mse[c(3,4)+1,1] #Overlapping mean
a2 = ratio.par.mse[c(1,2)+1,1] #NON Overlapping mean

b1 = ratio.par.mse[c(3,4)+1,2] #Overlapping scale_s
b2 = ratio.par.mse[c(1,2)+1,2] #NON Overlapping scale_s

c1 = ratio.par.mse[c(3,4)+1,3] #Overlapping scale_t
c2 = ratio.par.mse[c(1,2)+1,3] #NON Overlapping scale_t


prange1 = rbind(a1,a2,b1,b2,c1,c2)
prange1


#==== Overall measure
overall1 <- matrix(NA, ncol=2,nrow=2)
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]),use = "na.or.complete")) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall1[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall1[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=4, overlapping
overall1[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall1[2,2] <- round(((A/B)^(1/4)),3)
overall1 #regular double space 1.2


# ***************** REGULAR SPACE 1.8
dat1 <- read.csv("regular_results_spacetimeDE2.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.8/3,1.8/3,1)
param <- c(4/3,4/3,1)
param <- c(4/3,4/3,1) #ST3 con lat y winc_t iguales
param <- c(.8/3,5/3,1) #ST3 con lat y winc_t iguales
param <- c(4/3,4/3,1) #ST3 con lat y winc_t iguales

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario
table(dat1$setting)

boxplot(dat1$scale_s~dat1$setting)
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~dat1$setting)
abline(h = param[2],col="blue")
boxplot(dat1$sill~dat1$setting)
abline(h = param[3],col="blue")


# MSE by PARAM and Scenario:
par.mse = NULL
for(i in 1:3)
{
  par.mse = cbind(par.mse,tapply(dat1[,names.param[i]],dat1[,"setting"],mse,param[i]))
}
colnames(par.mse) = names.param
ratio.par.mse = cbind(par.mse[1,1]/par.mse[,1],
                      par.mse[1,2]/par.mse[,2],
                      par.mse[1,3]/par.mse[,3])
colnames(ratio.par.mse) = names.param
#*****
a1 = ratio.par.mse[c(3,4)+1,1] #Overlapping scale_s
a2 = ratio.par.mse[c(1,2)+1,1] #NON Overlapping scale_s

b1 = ratio.par.mse[c(3,4)+1,2] #Overlapping scale_t
b2 = ratio.par.mse[c(1,2)+1,2] #NON Overlapping scale_t

c1 = ratio.par.mse[c(3,4)+1,3] #Overlapping sill
c2 = ratio.par.mse[c(1,2)+1,3] #NON Overlapping sill

prange2 = rbind(a1,a2,b1,b2,c1,c2)
prange2

regS = rbind(prange1,prange2) #Space Regular for Double Exponenctial, 1.2 and 1.8


#==== Overall measure
overall2 <- matrix(NA, ncol=2,nrow=2)
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]),use = "na.or.complete")) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall2[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall2[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=4, overlapping
overall2[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall2[2,2] <- round(((A/B)^(1/4)),3)
overall2 #regular double space 1.8



# ******************************************** IRREGULAR SPACE Double 1.2

dat1 <- read.csv("Irregular_results_spacetimeDE15.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.2/3,1.2/3,1)
param <- c(3/3,3/3,1)
param <- c(3/3,3/3,1) #ST3 con lat y winc_t iguales
param <- c(.5/3,4/3,1) #ST3 con lat y winc_t iguales
param <- c(3.1/3,3.1/3,1) #ST3 con lat y winc_t iguales

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario
table(dat1$setting)

boxplot(dat1$scale_s~dat1$setting)
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~dat1$setting)
abline(h = param[2],col="blue")
boxplot(dat1$sill~dat1$setting)
abline(h = param[3],col="blue")


# MSE by PARAM and Scenario:
par.mse = NULL
for(i in 1:3)
{
  par.mse = cbind(par.mse,tapply(dat1[,names.param[i]],dat1[,"setting"],mse,param[i]))
}
colnames(par.mse) = names.param
ratio.par.mse = cbind(par.mse[1,1]/par.mse[,1],
                      par.mse[1,2]/par.mse[,2],
                      par.mse[1,3]/par.mse[,3])
colnames(ratio.par.mse) = names.param
#*****
a1 = ratio.par.mse[c(3,4)+1,1] #Overlapping scale_s
a2 = ratio.par.mse[c(1,2)+1,1] #NON Overlapping scale_s

b1 = ratio.par.mse[c(3,4)+1,2] #Overlapping scale_t
b2 = ratio.par.mse[c(1,2)+1,2] #NON Overlapping scale_t

c1 = ratio.par.mse[c(3,4)+1,3] #Overlapping sill
c2 = ratio.par.mse[c(1,2)+1,3] #NON Overlapping sill

prange3 = rbind(a1,a2,b1,b2,c1,c2)
# xtable::xtable(prange,digits=3)



#==== Overall measure
overall3 <- matrix(NA, ncol=2,nrow=2)
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]),use = "na.or.complete")) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall3[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall3[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=4, overlapping
overall3[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall3[2,2] <- round(((A/B)^(1/4)),3)
overall3 #irregular double space 1.2



# ******************************************** IRREGULAR SPACE Double 1.8
dat1 <- read.csv("Irregular_results_spacetimeDE2.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.8/3,1.8/3,1)
param <- c(4/3,4/3,1)
param <- c(4/3,4/3,1) #ST3 con lat y winc_t iguales
param <- c(.8/3,5/3,1) #ST3 con lat y winc_t iguales
param <- c(4/3,4/3,1) #ST3 con lat y winc_t iguales

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario
table(dat1$setting)

boxplot(dat1$scale_s~dat1$setting)
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~dat1$setting)
abline(h = param[2],col="blue")
boxplot(dat1$sill~dat1$setting)
abline(h = param[3],col="blue")


# MSE by PARAM and Scenario:
par.mse = NULL
for(i in 1:3)
{
  par.mse = cbind(par.mse,tapply(dat1[,names.param[i]],dat1[,"setting"],mse,param[i]))
}
colnames(par.mse) = names.param
ratio.par.mse = cbind(par.mse[1,1]/par.mse[,1],
                      par.mse[1,2]/par.mse[,2],
                      par.mse[1,3]/par.mse[,3])
colnames(ratio.par.mse) = names.param
#*****
a1 = ratio.par.mse[c(3,4)+1,1] #Overlapping scale_s
a2 = ratio.par.mse[c(1,2)+1,1] #NON Overlapping scale_s

b1 = ratio.par.mse[c(3,4)+1,2] #Overlapping scale_t
b2 = ratio.par.mse[c(1,2)+1,2] #NON Overlapping scale_t

c1 = ratio.par.mse[c(3,4)+1,3] #Overlapping sill
c2 = ratio.par.mse[c(1,2)+1,3] #NON Overlapping sill


prange4 = rbind(a1,a2,b1,b2,c1,c2)
# xtable::xtable(prange,digits=3)
IrregS = rbind(prange3,prange4) #SpaceTime Regular for Double Exponential, 04 and 12

DouSpace <- cbind(regS,IrregS)


#==== Overall measure
overall4 <- matrix(NA, ncol=2,nrow=2)
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]),use = "na.or.complete")) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall4[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall4[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=4, overlapping
overall4[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall4[2,2] <- round(((A/B)^(1/4)),3)
overall4 #irregular double space 1.8

rbind(cbind(overall1,overall3))
rbind(cbind(overall2,overall4))
# cbind(regS,IrregS)
rbind(cbind(prange1,prange3),rbind(cbind(overall1,overall3)))
rbind(cbind(prange2,prange4),rbind(cbind(overall2,overall4)))

SPACETIMEDOU <- rbind(rbind(cbind(prange1,prange3),rbind(cbind(overall1,overall3))),
               rbind(cbind(prange2,prange4),rbind(cbind(overall2,overall4))))
SPACETIMEDOU


# =======================================================
# =======================================================
# =======================================================
# =======================================================
# =======================================================






#********************************************** GNEITING REGULAR
# ***************** REGULAR SPACE 1.2
dat1 <- read.csv("regular_results_spacetimeGN15.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.2/3,1.2/19,1)
param <- c(3/3,3/19,1) #ST3 maxdist =maxtime 3,4

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario
table(dat1$setting)

# **************************scenario 0/4A main NOT weighted estimation***
# **************************scenario 1/4 b = 2, no overlaping  ***
# **************************scenario 2/4 b = 4, no overlaping ***
# **************************scenario 3/4 b = 2, overlaping (.5) ***
# **************************scenario 4/4 b = 4, overlaping (.5) ***

# boxplot(dat1$mean~dat1$setting)
boxplot(dat1$scale_s~dat1$setting)
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~dat1$setting)
abline(h = param[2],col="blue")
boxplot(dat1$sill~dat1$setting)
abline(h = param[3],col="blue")


# MSE by PARAM and Scenario:
par.mse = NULL
for(i in 1:3)
{
  par.mse = cbind(par.mse,tapply(dat1[,names.param[i]],dat1[,"setting"],mse,param[i]))
}
colnames(par.mse) = names.param
ratio.par.mse = cbind(par.mse[1,1]/par.mse[,1],
                      par.mse[1,2]/par.mse[,2],
                      par.mse[1,3]/par.mse[,3])
colnames(ratio.par.mse) = names.param
#*****
a1 = ratio.par.mse[c(3,4)+1,1] #Overlapping mean
a2 = ratio.par.mse[c(1,2)+1,1] #NON Overlapping mean

b1 = ratio.par.mse[c(3,4)+1,2] #Overlapping scale_s
b2 = ratio.par.mse[c(1,2)+1,2] #NON Overlapping scale_s

c1 = ratio.par.mse[c(3,4)+1,3] #Overlapping scale_t
c2 = ratio.par.mse[c(1,2)+1,3] #NON Overlapping scale_t


prange1 = rbind(a1,a2,b1,b2,c1,c2)
prange1


#==== Overall measure
overall1 <- matrix(NA, ncol=2,nrow=2)
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]),use = "na.or.complete")) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall1[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall1[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=4, overlapping
overall1[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall1[2,2] <- round(((A/B)^(1/4)),3)
overall1 #regular double space 1.2


# ***************** REGULAR SPACE 1.8
dat1 <- read.csv("regular_results_spacetimeGN2.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.8/3,1.8/19,1)
param <- c(4/3,4/19,1)

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario
table(dat1$setting)

boxplot(dat1$scale_s~dat1$setting)
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~dat1$setting)
abline(h = param[2],col="blue")
boxplot(dat1$sill~dat1$setting)
abline(h = param[3],col="blue")


# MSE by PARAM and Scenario:
par.mse = NULL
for(i in 1:3)
{
  par.mse = cbind(par.mse,tapply(dat1[,names.param[i]],dat1[,"setting"],mse,param[i]))
}
colnames(par.mse) = names.param
ratio.par.mse = cbind(par.mse[1,1]/par.mse[,1],
                      par.mse[1,2]/par.mse[,2],
                      par.mse[1,3]/par.mse[,3])
colnames(ratio.par.mse) = names.param
#*****
a1 = ratio.par.mse[c(3,4)+1,1] #Overlapping scale_s
a2 = ratio.par.mse[c(1,2)+1,1] #NON Overlapping scale_s

b1 = ratio.par.mse[c(3,4)+1,2] #Overlapping scale_t
b2 = ratio.par.mse[c(1,2)+1,2] #NON Overlapping scale_t

c1 = ratio.par.mse[c(3,4)+1,3] #Overlapping sill
c2 = ratio.par.mse[c(1,2)+1,3] #NON Overlapping sill

prange2 = rbind(a1,a2,b1,b2,c1,c2)
prange2

regS = rbind(prange1,prange2) #Space Regular for Double Exponenctial, 1.2 and 1.8


#==== Overall measure
overall2 <- matrix(NA, ncol=2,nrow=2)
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]),use = "na.or.complete")) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall2[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall2[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=4, overlapping
overall2[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]),use = "na.or.complete")) 
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall2[2,2] <- round(((A/B)^(1/4)),3)
overall2 #regular double space 1.8



# ******************************************** IRREGULAR SPACE Double 1.2
dat1 <- read.csv("Irregular_results_spacetimeGN15.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.2/3,1.2/19,1)
param <- c(3/3,3/19,1)

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario
table(dat1$setting)

boxplot(dat1$scale_s~dat1$setting)
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~dat1$setting)
abline(h = param[2],col="blue")
boxplot(dat1$sill~dat1$setting)
abline(h = param[3],col="blue")


# MSE by PARAM and Scenario:
par.mse = NULL
for(i in 1:3)
{
  par.mse = cbind(par.mse,tapply(dat1[,names.param[i]],dat1[,"setting"],mse,param[i]))
}
colnames(par.mse) = names.param
ratio.par.mse = cbind(par.mse[1,1]/par.mse[,1],
                      par.mse[1,2]/par.mse[,2],
                      par.mse[1,3]/par.mse[,3])
colnames(ratio.par.mse) = names.param
#*****
a1 = ratio.par.mse[c(3,4)+1,1] #Overlapping scale_s
a2 = ratio.par.mse[c(1,2)+1,1] #NON Overlapping scale_s

b1 = ratio.par.mse[c(3,4)+1,2] #Overlapping scale_t
b2 = ratio.par.mse[c(1,2)+1,2] #NON Overlapping scale_t

c1 = ratio.par.mse[c(3,4)+1,3] #Overlapping sill
c2 = ratio.par.mse[c(1,2)+1,3] #NON Overlapping sill

prange3 = rbind(a1,a2,b1,b2,c1,c2)
prange3
# xtable::xtable(prange,digits=3)



#==== Overall measure
overall3 <- matrix(NA, ncol=2,nrow=2)
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]),use = "na.or.complete")) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=2, overlapping
overall3[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall3[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=4, overlapping
overall3[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall3[2,2] <- round(((A/B)^(1/4)),3)
overall3 #irregular double space 1.2



# ******************************************** IRREGULAR SPACE Double 1.8
dat1 <- read.csv("Irregular_results_spacetimeGN2.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.8/3,1.8/19,1)
param <- c(4/3,4/19,1)

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario
table(dat1$setting)


boxplot(dat1$scale_s~dat1$setting)
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~dat1$setting)
abline(h = param[2],col="blue")
boxplot(dat1$sill~dat1$setting)
abline(h = param[3],col="blue")


# MSE by PARAM and Scenario:
par.mse = NULL
for(i in 1:3)
{
  par.mse = cbind(par.mse,tapply(dat1[,names.param[i]],dat1[,"setting"],mse,param[i]))
}
colnames(par.mse) = names.param
ratio.par.mse = cbind(par.mse[1,1]/par.mse[,1],
                      par.mse[1,2]/par.mse[,2],
                      par.mse[1,3]/par.mse[,3])
colnames(ratio.par.mse) = names.param
#*****
a1 = ratio.par.mse[c(3,4)+1,1] #Overlapping scale_s
a2 = ratio.par.mse[c(1,2)+1,1] #NON Overlapping scale_s

b1 = ratio.par.mse[c(3,4)+1,2] #Overlapping scale_t
b2 = ratio.par.mse[c(1,2)+1,2] #NON Overlapping scale_t

c1 = ratio.par.mse[c(3,4)+1,3] #Overlapping sill
c2 = ratio.par.mse[c(1,2)+1,3] #NON Overlapping sill


prange4 = rbind(a1,a2,b1,b2,c1,c2)
# xtable::xtable(prange,digits=3)
IrregS = rbind(prange3,prange4) #SpaceTime Regular for Double Exponential, 04 and 12

DouSpace <- cbind(regS,IrregS)


#==== Overall measure
overall4 <- matrix(NA, ncol=2,nrow=2)
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]),use = "na.or.complete")) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=2, overlapping
overall4[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall4[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=4, overlapping
overall4[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]),use = "na.or.complete"))
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall4[2,2] <- round(((A/B)^(1/4)),3)
overall4 #irregular double space 1.8

rbind(cbind(overall1,overall3))
rbind(cbind(overall2,overall4))
# cbind(regS,IrregS)
rbind(cbind(prange1,prange3),rbind(cbind(overall1,overall3)))
rbind(cbind(prange2,prange4),rbind(cbind(overall2,overall4)))

SPACETIMEGN <- rbind(rbind(cbind(prange1,prange3),rbind(cbind(overall1,overall3))),
                  rbind(cbind(prange2,prange4),rbind(cbind(overall2,overall4))))
SPACETIMEGN

SPACETIME <- cbind(SPACETIMEDOU,SPACETIMEGN)
SPACETIME



sum(SPACETIME>=0.7)/(dim(SPACETIME)[1]*dim(SPACETIME)[2])

min(SPACETIME)


