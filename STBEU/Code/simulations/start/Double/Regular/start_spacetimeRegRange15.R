###########################################
rm(list=ls())

#*** Info:
'   
PRACTICAL RANGE = 0.4/3
12 scenarios:
b1: 3,10 for window size in space and time.
b2: 4,20 for window size in space and time.
Step window: 0.5 for space && no overlap (two in total)

maxdist=.25*lato_fin
maxtime=ceiling(.25*winc_t)
'
#*** End Info:

########################R package#######################
require(CompRandFld)
require(MCMCpack)
require(STBEU)
dirsol = "~/Documents/Articles/Coding/STBEU/Data/all" # Directory where results will be
###############################################
# setting 1  (many locations in space a few in time)
###############################################

####location sites ########################################
lambda=5
xx=seq(-lambda,lambda);
coords=as.matrix(expand.grid(xx,xx))   ###regular
# plot(coords)

####temporal instants ########################################
nt = 50 # Time points 18
times=seq(1,nt,1)
nrow(coords)
nrow(coords)*nt
# bn = 2,4 Same Space
###############################################


##################################################################
model=1  #   1=double exponential       2 =gneiting

if (model == 1) 
{
  # exponential model
  cov.model <-"exp_exp"
  cc=1
  #####
  mean=0
  nugget=0
  scale_s<-3.1/3
  scale_t<-3.1/3
  sill=1
  param=list(nugget=nugget,mean=mean,scale_t=scale_t,scale_s=scale_s,sill=sill)
  fixed=list(nugget=nugget,mean=mean)
  fix=c(nugget=nugget,mean=mean)
}

################################################
#   parameters for the subsampling ####
###############################################
coordx=coords[,1]
coordy=coords[,2]
LX=abs(range(coordx)[1]-range(coordx)[2])
LY=abs(range(coordy)[1]-range(coordy)[2])
type_subs=3    ### type of subsampling  1=in space    2= in time 3= spacetime

type_dist=1                ### type of distance     1=euclidean 2=chordal  3=geodesic 
maxdistPL=max(dist(coords))*.25                  ## compact support in weights function for pairwise liklihood
maxtimePL=ceiling(max(dist(times))*.25)

i=1
nsim<-500   ## number of simulation
results=matrix(NA,ncol=4,nrow=5*nsim);label = 0:4
set.seed(276)
while(i<=nsim)
{
  #####################################################################################################      
  # Simulation of the spatial Gaussian random field:
  data <- RFsim(coordx=coords,coordt=times,corrmodel=cov.model, param=param)$data
  mm=mean(c(data))
  vv=var(c(data))
  start=list(scale_s=scale_s,scale_t=scale_t,sill=vv)
  ##################################################################################################### 
  
  ###### Composite likelihood based on pairs estimation ###############################################     
  theta=c(sill=vv,scale_s=scale_s,scale_t=scale_t)                #starting value  
  
  
  # Maximum composite-likelihood fitting of the random field:
  
  # **************************scenario 0/4A main NOT weighted estimation***
  fitA <- FitComposite(data=data,coordx=coords,coordt=times,
                       corrmodel=cov.model,maxtime=maxtimePL,maxdist=maxdistPL,
                       likelihood="Marginal",type="Pairwise",
                       start=start,fixed=fixed,weighted=F)
  eu_par0=c(fitA$param[1],fitA$param[2],fitA$param[3])
  print(fitA$param)
  names(eu_par0)=names(fitA$param)
  # **************************scenario 1/4 bs = 3,bt = 10, no overlaping  ***
  ################################################
  #   parameters for the subsampling ####
  ###############################################
  
  lato_fin=2  #changing window size 2,4
  lx=lato_fin          #lunghezza lato x quadrato subfinestra
  ly=lato_fin          #lunghezza lato y quadrato subfinestra
  winc=c(lx/sqrt(LX),ly/sqrt(LY))
  winstp= 1  ###   1/lato_fin complete overlapping   1 "no" overlapping
  ###############################################
  winc_t=2
  winstp_t= 1  ###   1/lato_fin complete overlapping   1 "no" overlapping
  ###############################################
  
  weighted=0
  maxdist=lato_fin
  maxtime=ceiling(winc_t)
  
  ### eucliden likelihood ################ 
  res1=STBEUFit(theta,fix,coords,times,cc,data,type_dist,maxdist ,maxtime,winc_s=winc,winstp_s = winstp,winc_t = winc_t,winstp_t = winstp_t,type_subs,weighted)
  eu_par1=c(res1$par[2],res1$par[3],res1$par[1])
  names(eu_par1)=names(fitA$param)
  # print(eu_par1)
  
  # **************************scenario 2/4 bs = 4,bt = 20, no overlaping  ***
  
  ################################################
  #   parameters for the subsampling ####
  ###############################################
  
  lato_fin=3  #changing window size 2,4
  lx=lato_fin          #lunghezza lato x quadrato subfinestra
  ly=lato_fin          #lunghezza lato y quadrato subfinestra
  winc=c(lx/sqrt(LX),ly/sqrt(LY))
  winstp= 1  ###   1/lato_fin complete overlapping   1 "no" overlapping
  ###############################################
  winc_t=3
  winstp_t= 1  ###   1/lato_fin complete overlapping   1 "no" overlapping
  ###############################################
  
  maxdist=lato_fin
  maxtime=ceiling(winc_t)
  
  ### eucliden likelihood ################ 
  res2=STBEUFit(theta,fix,coords,times,cc,data,type_dist,maxdist ,maxtime,winc_s=winc,winstp_s = winstp,winc_t = winc_t,winstp_t = winstp_t,type_subs,weighted)
  eu_par2=c(res2$par[2],res2$par[3],res2$par[1])
  names(eu_par2)=names(fitA$param)
  # print(eu_par2)
  
  # **************************scenario 3/4 bs = 3,bt=10 overlaping (.5) ***
  
  ################################################
  #   parameters for the subsampling ####
  ###############################################
  
  lato_fin=2  #changing window size 2,4
  lx=lato_fin          #lunghezza lato x quadrato subfinestra
  ly=lato_fin          #lunghezza lato y quadrato subfinestra
  winc=c(lx/sqrt(LX),ly/sqrt(LY))
  winstp= .5  ###   1/lato_fin complete overlapping   1 "no" overlapping
  ###############################################
  winc_t=2
  winstp_t= .5  ###   1/lato_fin complete overlapping   1 "no" overlapping
  ###############################################
  
  maxdist=lato_fin
  maxtime=ceiling(winc_t)
  
  ### eucliden likelihood ################ 
  res3=STBEUFit(theta,fix,coords,times,cc,data,type_dist,maxdist ,maxtime,winc_s=winc,winstp_s = winstp,winc_t = winc_t,winstp_t = winstp_t,type_subs,weighted)
  eu_par3=c(res3$par[2],res3$par[3],res3$par[1])
  names(eu_par3)=names(fitA$param)
  # print(eu_par3)
  
  # **************************scenario 4/4 bs = 4,bt=20, overlaping (.5), weighted. ***
  
  ################################################
  #   parameters for the subsampling ####
  ###############################################
  
  lato_fin=3  #changing window size 2,4,8
  lx=lato_fin          #lunghezza lato x quadrato subfinestra
  ly=lato_fin          #lunghezza lato y quadrato subfinestra
  winc=c(lx/sqrt(LX),ly/sqrt(LY))
  winstp= .5  ###   1/lato_fin complete overlapping   1 "no" overlapping
  ###############################################
  winc_t=3
  winstp_t= .5  ###   1/lato_fin complete overlapping   1 "no" overlapping
  ###############################################
  
  maxdist=lato_fin
  maxtime=ceiling(winc_t)
  
  ### eucliden likelihood ################ 
  res4=STBEUFit(theta,fix,coords,times,cc,data,type_dist,maxdist ,maxtime,winc_s=winc,winstp_s = winstp,winc_t = winc_t,winstp_t = winstp_t,type_subs,weighted)
  eu_par4=c(res4$par[2],res4$par[3],res4$par[1])
  names(eu_par4)=names(fitA$param)
  # print(eu_par4)
  
  results[(5*(i)-4):((5*(i)-4)+4),]=cbind(label,rbind(eu_par0=eu_par0,eu_par1,eu_par2,eu_par3,eu_par4))
  colnames(results)=c("setting",names(fitA$param))
  print(i)
  i=i+1
  setwd(dirsol)
  write.csv(results, "regular_results_spacetimeDE15.csv")
}
