
###########################################
rm(list=ls())

#*** Info:
'
PRACTICAL RANGE = 5
6 scenarios:
b: 2,4,8 for window size in time (3 in total).
Weighted: 1 (Yes), 0 (No) (two in total)
Step window: no overlap (one in total)'
#*** End Info:

########################R package#######################
require(CompRandFld)
require(MCMCpack)
require(spatstat)
require(STBEU)
dirsol = "~/Documents/Articles/Coding/STBEU/Data/all" # Directory where results will be 



###############################################
# setting 2  (few locations in space many in time)
###############################################


####location sites ########################################
lambda=2
set.seed(1985)                                                                      ### not regular
pp<-runifpoint(25, win=owin(c(-lambda,lambda),c(-lambda,lambda)))       ### not regular
coords<-cbind(pp$x,pp$y)  
plot(coords)
### not regular
####temporal instants ########################################
nt = 210 # Time points
times=seq(1,nt,1)
nrow(coords)
nrow(coords)*nt
# bnt = 2,4,6. For Time


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
type_subs=2    ### type of subsampling  1=in space    2= in time 3= spacetime

type_dist=1                ### type of distance     1=euclidean 2=chordal  3=geodesic 
maxdist=max(dist(coords))*.25                  ## compact support in weights function for pairwise liklihood
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
                       corrmodel=cov.model,maxtime=maxtimePL,maxdist=maxdist,
                       likelihood="Marginal",type="Pairwise",
                       start=start,fixed=fixed,weighted=F)
  eu_par0=c(fitA$param[1],fitA$param[2],fitA$param[3])
  print(fitA$param)
  names(eu_par0)=names(fitA$param)
  # **************************scenario 1/4 b = 4, no overlaping  ***
  ################################################
  #   parameters for the subsampling ####
  ###############################################
  winc_t=2
  winstp_t= 1  ###   1/lato_fin complete overlapping   1 "no" overlapping
  ###############################################
  
  weighted=0
  maxtime=ceiling(winc_t)
  # maxdist=1.5
  
  ### eucliden likelihood ################ 
  res1=STBEUFit(theta,fix,coords,times,cc,data,type_dist,maxdist ,maxtime,winc_s=0,winstp_s = 0,winc_t = winc_t,winstp_t = winstp_t,type_subs,weighted)
  eu_par1=c(res1$par[2],res1$par[3],res1$par[1])
  names(eu_par1)=names(fitA$param)
  # print(eu_par1)
  
  # **************************scenario 2/4 b = 8, no overlaping ***
  
  ################################################
  #   parameters for the subsampling ####
  ###############################################
  
  winc_t=3
  winstp_t= 1  ###   1/lato_fin complete overlapping   1 "no" overlapping
  ###############################################
  
  maxtime=ceiling(winc_t)
  
  ### eucliden likelihood ################ 
  res2=STBEUFit(theta,fix,coords,times,cc,data,type_dist,maxdist ,maxtime,winc_s=0,winstp_s = 0,winc_t = winc_t,winstp_t = winstp_t,type_subs,weighted)
  eu_par2=c(res2$par[2],res2$par[3],res2$par[1])
  names(eu_par2)=names(fitA$param)
  # print(eu_par2)
  
  # **************************scenario 3/4 b = 4, overlaping (.5) ***
  
  ################################################
  #   parameters for the subsampling ####
  ###############################################
  
  winc_t=2
  winstp_t= .5  ###   1/lato_fin complete overlapping   1 "no" overlapping
  ###############################################
  
  maxtime=ceiling(winc_t)
  
  ### eucliden likelihood ################ 
  res3=STBEUFit(theta,fix,coords,times,cc,data,type_dist,maxdist ,maxtime,winc_s=0,winstp_s = 0,winc_t = winc_t,winstp_t = winstp_t,type_subs,weighted)
  eu_par3=c(res3$par[2],res3$par[3],res3$par[1])
  names(eu_par3)=names(fitA$param)
  # print(eu_par3)
  
  # **************************scenario 4/4 b = 8, overlaping (.5), weighted. ***
  
  ################################################
  #   parameters for the subsampling ####
  ###############################################
  
  winc_t=3
  winstp_t= .5  ###   1/lato_fin complete overlapping   1 "no" overlapping
  ###############################################
  
  maxtime=ceiling(winc_t)
  
  ### eucliden likelihood ################ 
  res4=STBEUFit(theta,fix,coords,times,cc,data,type_dist,maxdist ,maxtime,winc_s=0,winstp_s = 0,winc_t = winc_t,winstp_t = winstp_t,type_subs,weighted)
  eu_par4=c(res4$par[2],res4$par[3],res4$par[1])
  names(eu_par4)=names(fitA$param)
  # print(eu_par4)
  
  results[(5*(i)-4):((5*(i)-4)+4),]=cbind(label,rbind(eu_par0=eu_par0,eu_par1,eu_par2,eu_par3,eu_par4))
  colnames(results)=c("setting",names(fitA$param))
  print(i)
  i=i+1
  setwd(dirsol)
  write.csv(results, "Irregular_results_timeDE15.csv")
}
