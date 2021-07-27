rm(list = ls())
graphics.off()
cat("\014")
library(STBEU)



#################################********   SPACE   ***####################################### 

#=======================Double Exponential=======================#

rm(list=ls())
########################R package#######################
require(GeoModels)
require(MCMCpack)
require(STBEU)
####location sites ########################################
lambda=8
xx=seq(-lambda,lambda);
coords=as.matrix(expand.grid(xx,xx))   ###regular
# plot(coords)
#set.seed(15)                                                                      ### not regular
#pp<-runifpoint(4*(lambda)^2, win=owin(c(-lambda,lambda),c(-lambda,lambda)))       ### not regular
#coords<-cbind(pp$x,pp$y)  
### not regular
####temporal instants ########################################
times=seq(1,5,1)
##################################################################


type_dist=1                ### type of distance     1=euclidean 2=chordal  3=geodesic 
maxdist=2            ## compact support in weights function for pairwise liklihood
maxtime=2

model=1  #   1=double exponential       2 =gneiting

if (model == 1) {
  # exponential model
  cov.model <-"exp_exp"
  cc=1
  #####
  mean=0
  nugget=0
  scale_s<-1.5/3
  scale_t<-1.5/3
  sill=1
  param=list(nugget=nugget,mean=mean,scale_t=scale_t,scale_s=scale_s,sill=sill)  
  fixed=list(nugget=0)
  fix=c(nugget=nugget)
}
##################################################################
set.seed(276)
#####################################################################################################      
# Simulation of the spatial Gaussian random field:
data <- GeoSim(coordx=coords,coordt=times,corrmodel=cov.model, param=param)$data
mm=mean(c(data))
vv=var(c(data))
###### Composite likelihood based on pairs estimation ###############################################     

start=list(mean=mm,scale_s=scale_s,scale_t=scale_t,sill=vv)
################################################
#   parameters for the subsampling ####
###############################################
coordx=coords[,1]
coordy=coords[,2]
LX=abs(range(coordx)[1]-range(coordx)[2])
LY=abs(range(coordy)[1]-range(coordy)[2])

lato_fin=3  #changing window size
lx=lato_fin          #lunghezza lato x quadrato subfinestra
ly=lato_fin          #lunghezza lato y quadrato subfinestra
winc=c(lx/sqrt(LX),ly/sqrt(LY))
winstp= 1  ###   1/lato_fin complete overlapping   1 "no" overlapping
###############################################

#####################################################################################################      
theta=start                #starting value  

weighted=0
### eucliden likelihood ################ 
type_subs=1    ### type of subsampling  1=in space    2= in time
tCPU = proc.time()
# names(fix) = c("nugget");names(theta) = c("mean","sill","scale_s","scale_t")
res=STBEUFit(theta,fixed = fix,coords,times,cc,data,type_dist,maxdist ,maxtime
             ,winc,winstp,0,0,type_subs,weighted)
tCPU = proc.time()-tCPU;tCPU
res$par

coordx=coords[,1];coordy=coords[,2];ncoords=nrow(coords);ntime=length(times)
npar=length(theta)

subs <- type_subs
if(subs==1) type_sub="SubSamp_space"
if(subs==2) type_sub="SubSamp_time"
if(subs==3) type_sub="SubSamp_spacetime"

sol <- eucl_st_ocl(theta=theta,fixed = fix,coordx = coordx,coordy = coordy,
                   ncoords = ncoords,times = times, ntime = ntime,
                   cc = cc, datos = data, type_dist = type_dist,
                   maxdist=maxdist, maxtime=maxtime, winc_s=winc, winstp_s=winstp, 
                   winc_t=NULL, winstp_t=NULL, weighted=weighted, 
                   type_sub=type_sub, local=c(1,1), GPU=NULL, kernel= "DouExp.cl")



res$par

#********************** SCALE S **********************#
t_aux <- theta
vals <- seq(0.2,2,0.01)

sol_scale_s <- NULL

for(i in 1:length(vals))
{
  t_aux$scale_s <- vals[i]
  
  sol_aux <- eucl_st_ocl(theta=t_aux,fixed = fix,coordx = coordx,coordy = coordy,
                         ncoords = ncoords,times = times, ntime = ntime,
                         cc = cc, datos = data, type_dist = type_dist,
                         maxdist=maxdist, maxtime=maxtime, winc_s=winc, winstp_s=winstp, 
                         winc_t=NULL, winstp_t=NULL, weighted=weighted, 
                         type_sub=type_sub, local=c(1,1), GPU=NULL, kernel= "DouExp.cl")
  sol_scale_s <- c(sol_scale_s,sol_aux)
}

plot(vals,sol_scale_s,t = "l", main = "Objetive function for scale s")
abline(v = c(res$par["scale_s"],scale_s), col =c("red","blue"))
legend("bottomright",c("Estimated","Theoretical"), col = c("red","blue"),lty = 1)


#********************** SCALE t **********************#
t_aux <- theta
vals <- seq(0.2,2,0.01)

sol_scale_t <- NULL

for(i in 1:length(vals))
{
  t_aux$scale_t <- vals[i]
  
  sol_aux <- eucl_st_ocl(theta=t_aux,fixed = fix,coordx = coordx,coordy = coordy,
                         ncoords = ncoords,times = times, ntime = ntime,
                         cc = cc, datos = data, type_dist = type_dist,
                         maxdist=maxdist, maxtime=maxtime, winc_s=winc, winstp_s=winstp, 
                         winc_t=NULL, winstp_t=NULL, weighted=weighted, 
                         type_sub=type_sub, local=c(1,1), GPU=NULL, kernel= "DouExp.cl")
  sol_scale_t <- c(sol_scale_t,sol_aux)
}

plot(vals,sol_scale_t,t = "l", main = "Objetive function for scale t")
abline(v = c(res$par["scale_t"],scale_t), col =c("red","blue"))
legend("bottomright",c("Estimated","Theoretical"), col = c("red","blue"),lty = 1)




#********************** SILL **********************#
t_aux <- theta
vals <- seq(0.01,2,0.02)

sol_sill <- NULL

for(i in 1:length(vals))
{
  t_aux$sill <- vals[i]
  
  sol_aux <- eucl_st_ocl(theta=t_aux,fixed = fix,coordx = coordx,coordy = coordy,
                         ncoords = ncoords,times = times, ntime = ntime,
                         cc = cc, datos = data, type_dist = type_dist,
                         maxdist=maxdist, maxtime=maxtime, winc_s=winc, winstp_s=winstp, 
                         winc_t=NULL, winstp_t=NULL, weighted=weighted, 
                         type_sub=type_sub, local=c(1,1), GPU=NULL, kernel= "DouExp.cl")
  sol_sill <- c(sol_sill,sol_aux)
}

plot(vals,sol_sill,t = "l", main = "Objetive function for sill")
abline(v = c(res$par["sill"],sill), col =c("red","blue"))
legend("bottomright",c("Estimated","Theoretical"), col = c("red","blue"),lty = 1)



#********************** mean **********************#
t_aux <- theta
vals <- seq(-1,1,0.01)

sol_mean <- NULL

for(i in 1:length(vals))
{
  t_aux$mean <- vals[i]
  
  sol_aux <- eucl_st_ocl(theta=t_aux,fixed = fix,coordx = coordx,coordy = coordy,
                         ncoords = ncoords,times = times, ntime = ntime,
                         cc = cc, datos = data, type_dist = type_dist,
                         maxdist=maxdist, maxtime=maxtime, winc_s=winc, winstp_s=winstp, 
                         winc_t=NULL, winstp_t=NULL, weighted=weighted, 
                         type_sub=type_sub, local=c(1,1), GPU=NULL, kernel= "DouExp.cl")
  sol_mean <- c(sol_mean,sol_aux)
}

plot(vals,sol_mean,t = "l", main = "Objetive function for mean")
abline(v = c(res$par["mean"],mean), col =c("red","blue"))
legend("bottomright",c("Estimated","Theoretical"), col = c("red","blue"),lty = 1)
