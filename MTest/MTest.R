#*********************** MTEST *****************+

# ***** Data Generation
library(MASS)
rm(list = ls())
graphics.off()

# Sample size:
n <- 10000
# vector means
medias <- c(0,0,0) 
# Correlation structure:
rho_12 <- -.945 # -.94
rho_13 <- .3
rho_23 <- -.5
# Coefficients of the regression:
betas <- c(10,-5,3,9) 
s.d <- 3 # deviation of the residual



(Sigma <- matrix(c(1,rho_12,rho_13,rho_12,1,rho_23,rho_13,rho_23,1),3,3))
set.seed(247)
# Predictors simulation:
X <- mvrnorm(n = n, medias, Sigma)
M <- cbind(1,X)
# Output simulation
y <- M %*% betas + rnorm(n,0,s.d)

datos <- data.frame(y,X)



cor(datos)

head(datos)
m1 <- lm(y~.,data = datos)
coef(m1)
betas
m1Sum <- summary(m1)

library(car)
(vif.vals <- vif(m1))

Raux <- (vif.vals-1)/vif.vals
Rglobal <- m1Sum$r.squared
1/(1-Raux)


############## 


pairwise.ks.test <- function(X,alternative="two.sided")
{
  #Returns the p value of the pairwise KS test of X columns
  n <- ncol(X)
  sol <- matrix(NA, ncol = n, nrow = n)
  for(i in 1:(n))
  {
    for(j in (1):n)
    {
      # print(c(i,j))
      a <- suppressWarnings(ks.test(X[,i],X[,j],alternative = alternative))
      # print(a$p.value)
      sol[i,j] <- a$p.value
    }
  }
  if(alternative=="less") print("alternative hypothesis: the CDF of x lies below that of y. Rows are `x` and Columns are `y`")
  if(alternative=="greater") print("alternative hypothesis: the CDF of x lies above that of y. Rows are `x` and Columns are `y`")
  if(alternative=="two.sided") print("alternative hypothesis: two-sided")
  colnames(sol) <- colnames(X)
  rownames(sol) <- colnames(X)
  return(sol)
}

Mtest <- function(datos, nboot = 500,
                  nsam = NULL,trace = TRUE,seed = NULL,
                  valor_vif = 0.9)
{
  if(is.null(nsam)){nsam = nrow(datos)*3}
  
  vals <- 1:nrow(datos)
  
  if(!is.null(seed)) {set.seed(seed)}
  
  sol.rsq <- NULL
  sol.vif <- NULL
  i = 1
  while(i <=nboot)
  {
    sam <- sample(vals,nsam,replace = TRUE)
    aux <- datos[sam,]
    maux <- lm(y~.,data = aux)
    sm  <- summary(maux)
    vif.vals <- vif(maux)
    Raux <- (vif.vals-1)/vif.vals
    
    s1 <- c(sm$r.squared,Raux)
    sol.rsq <- rbind(sol.rsq,s1)
    
    sol.vif <- rbind(sol.vif,vif.vals)
    
    if(trace)
    {
      cat("Iteration",i,"out of ",nboot,"\n")
    }
    i = i+1
  }
  
  pval_vif <- NULL
  for(j in 2:ncol(sol.rsq))
  {
    pval_vif <- c(pval_vif,sum(sol.rsq[,j]>valor_vif)/nboot)
  }
  names(pval_vif) <- colnames(sol.rsq)[2:ncol(sol.rsq)]
  pval_klein <- NULL
  for(z in 2:ncol(sol.rsq))
  {
    pval_klein <- c(pval_klein,sum(sol.rsq[,1]<sol.rsq[,z])/nboot)
  }
  names(pval_klein) <- colnames(sol.rsq)[2:ncol(sol.rsq)]
  
  colnames(sol.rsq) <- c("global",paste(names(datos)[-1],sep =""))
  rownames(sol.rsq) <- 1:nrow(sol.rsq)
  return(list(Bvals= sol.rsq,pval_vif = pval_vif,pval_klein=pval_klein))
}

# colMeans(Mtest(datos,trace=FALSE,seed = 1))

boot.sol <- Mtest(datos,trace=FALSE,seed = 1,nboot = 1000)
boot.sol$pval_vif
boot.sol$pval_klein

setwd("~/Documents/Articles/EPN/MTest/Figures")
pdf("fig1.pdf")
par(mfrow = c(2,2))
# aux <- reshape2::melt(boot.sol$Bvals[,2:ncol(boot.sol$Bvals)])
# library(ggplot2)
# p <- ggplot(aux, aes(x=Var2, y=value, color=Var2)) +
#   geom_boxplot()
# p
boxplot(boot.sol$Bvals[,1],ylab = expression(paste("y")))
boxplot(boot.sol$Bvals[,2],ylab = expression(paste("X")[1]))
boxplot(boot.sol$Bvals[,3],ylab = expression(paste("X")[2]))
boxplot(boot.sol$Bvals[,4],ylab = expression(paste("X")[3]))
par(mfrow = c(1,1))
dev.off()

round(pairwise.ks.test(X = boot.sol$Bvals),5)
# round(pairwise.ks.test(X = boot.sol$Bvals, alternative = "less"),5)
round(pairwise.ks.test(X = boot.sol$Bvals, alternative = "greater"),5)


round(c(Rglobal,Raux),5)
Raux>Rglobal
round(vif.vals,5)


summary(boot.sol$Bvals[,4])
summary(boot.sol$Bvals[,2])

ks.test(boot.sol$Bvals[,4],boot.sol$Bvals[,2],
        alternative ="less")

wilcox.test(x = boot.sol$Bvals[,4],y = boot.sol$Bvals[,2],
            alternative ="greater")

wilcox.test(x = boot.sol$Bvals[,4],y = boot.sol$Bvals[,2])






# ******* Application


uu <- "https://raw.githubusercontent.com/vmoprojs/DataLectures/master/GA/tabla10_8.csv"
datos<- read.csv(url(uu),sep=";",header=TRUE)

datos <- datos[,c("Y","X1","X2","X3","X4","X5","TIME")]
names(datos) <- tolower(names(datos))

ajuste.2 <- lm(y~., data = datos)
saj <- summary(ajuste.2)

(vif.vals <- car::vif(ajuste.2))

Raux <- (vif.vals-1)/vif.vals
Rglobal <- saj$r.squared
1/(1-Raux)

round(c(Rglobal,Raux),3)
round(vif.vals,0)

Raux>Rglobal
round(0.9955122,3)

boot.sol <- Mtest(datos,trace=FALSE,seed = 1,nboot = 1000)
boot.sol$pval_vif
boot.sol$pval_klein
round(pairwise.ks.test(X = boot.sol$Bvals),5)
# round(pairwise.ks.test(X = boot.sol, alternative = "less"),5)
round(pairwise.ks.test(X = boot.sol$Bvals, alternative = "greater"),5)



