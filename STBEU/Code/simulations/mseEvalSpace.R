#********************************************** REGULAR
# ***************** REGULAR SPACE 1.2
rm(list=ls())
setwd("~/Documents/Articles/Coding/STBEU/Data/space")
# mse<-function(x,a)  var(x)+(mean(x)-a)^2 # TABLE 8
mse<-function(x,a)  median(abs(x-a)) # TABLE 2

#*********** ST Discarded functions:
# mse<-function(x,a)
# {
#   qq = quantile(x, probs = c(0.05,0.95))
#   xstar = as.numeric(dist(qq))
#   return(xstar)
# }

# mse<-function(x,a)
# {
#   qq = quantile(x, probs = c(0.25,0.75))
#   xstar = as.numeric(dist(qq))
#   return(xstar)
# }
# mse<-function(x,a)  sqrt(var(x)+(mean(x)-a)^2)
# mse<-function(x,a)  sum(log(cosh(x-a)))
# 
# mse<-function(x,a,q = 0.5)
# {
#   # qq = quantile(x,probs = q)
#   x = rnorm(1000)
#   a = 0
#   q = 0.5
#   xstar = x
#   xstar1 = xstar[a<x]
#   xstar2 = xstar[a>=x]
#   (sol = sum((q-1)*abs(a-xstar1))+sum((q)*abs(a-xstar2)))
#   mean(abs(x-a))
#   # mean(mean(abs(xstar1-a)*(1-q)),mean(abs(xstar2-a)*q))
#   return(sol)
# }
#*********** END Discarded functions:

dat1 <- read.csv("regular_results_spaceDE15.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.2/3,1.2/3,1)


# N = 500
# tval = qnorm(0.975)

# intVal <- function(x,N = 500,tval = qnorm(0.975))
# {
#   x.m = mean(x)
#   sem = sd(x)/sqrt(N)*tval
#   upper = x.m+sem
#   lower = x.m-sem
#   return(c(lower = lower,mean = x.m,upper = upper))
# }
# x = rnorm(1000)

# Median Abs error
# Rango intercuartil
# 95 decile range


# tapply(dat1[,names.param[1]],dat1[,"setting"],intVal)

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario
table(dat1$setting)

# **************************scenario 0/4A main NOT weighted estimation***
# **************************scenario 1/4 b = 2, no overlaping  ***
# **************************scenario 2/4 b = 4, no overlaping ***
# **************************scenario 3/4 b = 2, overlaping (.5) ***
# **************************scenario 4/4 b = 4, overlaping (.5) ***

aux = ifelse(dat1$setting==0,"Base",
             ifelse(dat1$setting==1,"b=2,p=1",
                    ifelse(dat1$setting==2,"b=4,p=1",
                           ifelse(dat1$setting==3,"b=2,p=0.5",
                                  "b=4,p=0.5"))))

par(mfrow = c(3,1))
boxplot(dat1$scale_s~aux,ylab = "scale_s",main = "Regular",
        xlab = "Scenario")
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~aux,ylab = "scale_t",main = "Regular",
        xlab = "Scenario")
abline(h = param[2],col="blue")
boxplot(dat1$sill~aux,ylab = "sill",main = "Regular",
        xlab = "Scenario")
abline(h = param[3],col="blue")
par(mfrow = c(1,1))


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
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]))) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall1[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall1[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, overlapping
overall1[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall1[2,2] <- round(((A/B)^(1/4)),3)
overall1 #regular double space 1.2



base <- aggregate(dat1[,3:5],list(dat1$setting),mean)
base <- base[1,2:4]

bb<-NULL
for(i in 1:nrow(dat1))
{
  bb <- rbind(bb,base)
}

# str(apply(as.matrix(dat1[dat1$setting==1,3:5]),1,function(x){x-base}))
aux <- (cbind(dat1$setting,dat1[,3:5]-bb))
colnames(aux) <- c("setting",names(base))
plot(density(aux[aux$setting==1,2]*4000))


# ***************** REGULAR SPACE 1.8
dat1 <- read.csv("regular_results_spaceDE2.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.8/3,1.8/3,1)

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario

aux = ifelse(dat1$setting==0,"Base",
             ifelse(dat1$setting==1,"b=2,p=1",
                    ifelse(dat1$setting==2,"b=4,p=1",
                           ifelse(dat1$setting==3,"b=2,p=0.5",
                                  "b=4,p=0.5"))))

boxplot(dat1$scale_s~aux,ylab = "scale_s",main = "Regular",
        xlab = "Scenario")
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~aux,ylab = "scale_t",main = "Regular",
        xlab = "Scenario")
abline(h = param[2],col="blue")
boxplot(dat1$sill~aux,ylab = "sill",main = "Regular",
        xlab = "Scenario")
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
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]))) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall2[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall2[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, overlapping
overall2[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall2[2,2] <- round(((A/B)^(1/4)),3)
overall2 #regular double space 1.8



# ******************************************** IRREGULAR SPACE Double 1.2
dat1 <- read.csv("Irregular_results_spaceDE15.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.2/3,1.2/3,1)

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario


aux = ifelse(dat1$setting==0,"Base",
             ifelse(dat1$setting==1,"b=2,p=1",
                    ifelse(dat1$setting==2,"b=4,p=1",
                           ifelse(dat1$setting==3,"b=2,p=0.5",
                                  "b=4,p=0.5"))))

boxplot(dat1$scale_s~aux,ylab = "scale_s",main = "Irregular",
        xlab = "Scenario")
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~aux,ylab = "scale_t",main = "Irregular",
        xlab = "Scenario")
abline(h = param[2],col="blue")
boxplot(dat1$sill~aux,ylab = "sill",main = "Irregular",
        xlab = "Scenario")
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
prange3


#==== Overall measure
overall3 <- matrix(NA, ncol=2,nrow=2)
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]))) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall3[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall3[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, overlapping
overall3[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall3[2,2] <- round(((A/B)^(1/4)),3)
overall3 #irregular double space 1.2



# ******************************************** IRREGULAR SPACE Double 1.8
dat1 <- read.csv("Irregular_results_spaceDE2.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.8/3,1.8/3,1)

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario


aux = ifelse(dat1$setting==0,"Base",
             ifelse(dat1$setting==1,"b=2,p=1",
                    ifelse(dat1$setting==2,"b=4,p=1",
                           ifelse(dat1$setting==3,"b=2,p=0.5",
                                  "b=4,p=0.5"))))

boxplot(dat1$scale_s~aux,ylab = "scale_s",main = "Irregular",
        xlab = "Scenario")
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~aux,ylab = "scale_t",main = "Irregular",
        xlab = "Scenario")
abline(h = param[2],col="blue")
boxplot(dat1$sill~aux,ylab = "sill",main = "Irregular",
        xlab = "Scenario")
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
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]))) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall4[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall4[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, overlapping
overall4[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall4[2,2] <- round(((A/B)^(1/4)),3)
overall4 #irregular double space 1.8

rbind(cbind(overall1,overall3))
rbind(cbind(overall2,overall4))
# cbind(regS,IrregS)
rbind(cbind(prange1,prange3),rbind(cbind(overall1,overall3)))
rbind(cbind(prange2,prange4),rbind(cbind(overall2,overall4)))

SPACEDOU <- rbind(rbind(cbind(prange1,prange3),rbind(cbind(overall1,overall3))),
               rbind(cbind(prange2,prange4),rbind(cbind(overall2,overall4))))
SPACEDOU

sum(SPACEDOU>=0.9)/64

# =======================================================
# =======================================================
# =======================================================
# =======================================================
# =======================================================






#********************************************** GNEITING REGULAR
# ***************** REGULAR SPACE 1.2
dat1 <- read.csv("regular_results_spaceGN15.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.2/3,1.2/19,1)

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario
table(dat1$setting)

# **************************scenario 0/4A main NOT weighted estimation***
# **************************scenario 1/4 b = 2, no overlaping  ***
# **************************scenario 2/4 b = 4, no overlaping ***
# **************************scenario 3/4 b = 2, overlaping (.5) ***
# **************************scenario 4/4 b = 4, overlaping (.5) ***

# boxplot(dat1$mean~dat1$setting)
aux = ifelse(dat1$setting==0,"Base",
             ifelse(dat1$setting==1,"b=2,p=1",
                    ifelse(dat1$setting==2,"b=4,p=1",
                           ifelse(dat1$setting==3,"b=2,p=0.5",
                                  "b=4,p=0.5"))))

boxplot(dat1$scale_s~aux,ylab = "scale_s",main = "Regular",
        xlab = "Scenario")
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~aux,ylab = "scale_t",main = "Regular",
        xlab = "Scenario")
abline(h = param[2],col="blue")
boxplot(dat1$sill~aux,ylab = "sill",main = "Regular",
        xlab = "Scenario")
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
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]))) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall1[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall1[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, overlapping
overall1[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall1[2,2] <- round(((A/B)^(1/4)),3)
overall1 #regular double space 1.2


# ***************** REGULAR SPACE 1.8
dat1 <- read.csv("regular_results_spaceGN2.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.8/3,1.8/19,1)

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario

aux = ifelse(dat1$setting==0,"Base",
             ifelse(dat1$setting==1,"b=2,p=1",
                    ifelse(dat1$setting==2,"b=4,p=1",
                           ifelse(dat1$setting==3,"b=2,p=0.5",
                                  "b=4,p=0.5"))))

boxplot(dat1$scale_s~aux,ylab = "scale_s",main = "Regular",
        xlab = "Scenario")
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~aux,ylab = "scale_t",main = "Regular",
        xlab = "Scenario")
abline(h = param[2],col="blue")
boxplot(dat1$sill~aux,ylab = "sill",main = "Regular",
        xlab = "Scenario")
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
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]))) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall2[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall2[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, overlapping
overall2[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall2[2,2] <- round(((A/B)^(1/4)),3)
overall2 #regular double space 1.8



# ******************************************** IRREGULAR SPACE Double 1.2
dat1 <- read.csv("Irregular_results_spaceGN15.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.2/3,1.2/19,1)

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario


aux = ifelse(dat1$setting==0,"Base",
             ifelse(dat1$setting==1,"b=2,p=1",
                    ifelse(dat1$setting==2,"b=4,p=1",
                           ifelse(dat1$setting==3,"b=2,p=0.5",
                                  "b=4,p=0.5"))))

boxplot(dat1$scale_s~aux,ylab = "scale_s",main = "Irregular",
        xlab = "Scenario")
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~aux,ylab = "scale_t",main = "Irregular",
        xlab = "Scenario")
abline(h = param[2],col="blue")
boxplot(dat1$sill~aux,ylab = "sill",main = "Irregular",
        xlab = "Scenario")
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
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]))) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall3[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall3[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, overlapping
overall3[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall3[2,2] <- round(((A/B)^(1/4)),3)
overall3 #irregular double space 1.2



# ******************************************** IRREGULAR SPACE Double 1.8
dat1 <- read.csv("Irregular_results_spaceGN2.csv")
names(dat1)
# mean = 0;scale_s=1; scale_t=1;sill=1
names.param = c("scale_s","scale_t","sill")
param <- c(1.8/3,1.8/19,1)

mse.scenario = tapply(dat1[,names.param[1]],dat1[,"setting"],mse,param[1])
mse.scenario[1]/mse.scenario


aux = ifelse(dat1$setting==0,"Base",
             ifelse(dat1$setting==1,"b=2,p=1",
                    ifelse(dat1$setting==2,"b=4,p=1",
                           ifelse(dat1$setting==3,"b=2,p=0.5",
                                  "b=4,p=0.5"))))

boxplot(dat1$scale_s~aux,ylab = "scale_s",main = "Irregular",
        xlab = "Scenario")
abline(h = param[1],col="blue")
boxplot(dat1$scale_t~aux,ylab = "scale_t",main = "Irregular",
        xlab = "Scenario")
abline(h = param[2],col="blue")
boxplot(dat1$sill~aux,ylab = "sill",main = "Irregular",
        xlab = "Scenario")
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
A = det(var(as.matrix(dat1[dat1$setting==0,c(3:5)]))) #PL
B = det(var(as.matrix(dat1[dat1$setting==3,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, overlapping
overall4[1,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==1,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=2, non overlapping
overall4[2,1] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==4,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, overlapping
overall4[1,2] <- round(((A/B)^(1/4)),3)

B = det(var(as.matrix(dat1[dat1$setting==2,c(3:5)]))) 
round(((A/B)^(1/4)),3) #b=4, non overlapping
overall4[2,2] <- round(((A/B)^(1/4)),3)
overall4 #irregular double space 1.8

rbind(cbind(overall1,overall3))
rbind(cbind(overall2,overall4))
# cbind(regS,IrregS)
rbind(cbind(prange1,prange3),rbind(cbind(overall1,overall3)))
rbind(cbind(prange2,prange4),rbind(cbind(overall2,overall4)))

SPACEGN <- rbind(rbind(cbind(prange1,prange3),rbind(cbind(overall1,overall3))),
                  rbind(cbind(prange2,prange4),rbind(cbind(overall2,overall4))))
SPACEGN

SPACE <- cbind(SPACEDOU,SPACEGN)
SPACE
sum(SPACE>=1)/(dim(SPACE)[1]*dim(SPACE)[2])
min(SPACE)
