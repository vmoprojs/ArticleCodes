#-----------------------------------------------------------------
# Institution: IDCE Consulting (https://www.idceconsulting.com/)
# Proyect: CLARABD. 
# Author: Víctor Morales-Oñate (https://sites.google.com/site/moralesonatevictor/)
# Date: 06-03-2018
# Article: Una técnica de agrupación robusta 
#     para un enfoque Big Data: CLARABD para Tipos de datos mixtos
#-----------------------------------------------------------------

claraBD <- function(x,k, metric = "euclidean", samples = 5, clus = FALSE,mm = NULL)
{# aguments have almost the description as "clara" in cluster package
  
  # library requirements it parallel computing is asked:
  if(clus)
  {
    require(doMC)
    require(foreach)
  }
  
  
  df <- x # dataframe to be used
  n <- nrow(x)
  dd.name = metric
  
  # ST: sample size is computed with min(n, 40 + 2 * k) 
  # as explained in Kaufman and Rousseeuw (pag 145)
  size <- NULL # set the sample size if NULL
  if(is.null(size))
  {
    size <- min(n, 40 + 2 * k)
  }
  # END: sample size
  if(is.null(samples)) samples = n
  # cat("size: ",size,"\n")
  
  
  clustering <- NULL #saves the final partition
  Md <- NULL # medoids per sample
  closest.d.MOi <- NULL # dissimilarity value D(Oi,M)
  costo1 <- NULL # average dissimilarity per k group
  costo0 <- NULL # MEAN average dissimilarity per k group
  rownames(df) <- 1:nrow(df) # names are in the order of initial "x" or "df" 
  
  # metric for internal assignation
  # if(all(sapply(df, class)=="numeric") | all(sapply(df, class)=="integer")  )
  # {
  #   dd.int <- "euclidean"
  #   if(dd.name=="manhattan") {dd.int <- "manhattan"}
  # }else
  # {
  #   dd.int <- "gower"
  #   if(dd.name=="manhattan") {dd.int <- "manhattan"}
  # }
  dd.int <- metric
  # cat("metric",metric,(dd.int),"\n")
  rsam <- NULL #saver of final random sample
  #ST: Iterarion "samples" times
  for(sam in 1:samples) #OJO
  {
    # draw a sample from data:
    ss <- sample(1:nrow(df),size)
    # cat("muestra: ",length(ss),"\n")
    rsam <- cbind(rsam,ss)
    # compute distance
    # gower_dist <- daisy(df[ss,],metric = dd.name)
    # compute medoids in initial sample
    # pam_fit <- pam(gower_dist,diss = TRUE,k = k)$id.med
    pam_fit <- pam(df[ss,],k = k)$id.med
    # cat("pa_fit: ",pam_fit,"\n")
    pam_fit <- as.numeric(rownames(df[ss,])[pam_fit])
    # pam_fit <- as.numeric(pam_fit)
    
    if(!is.null(mm))
    {
      pam_fit <- mm
    }
    
    
    # ST: each object not belonging to the sample is 
    # assigned to the nearest of the k representative objects
    
    if(clus ==TRUE)
    {# Begin parallel version
      registerDoMC(cores=8)
      sol2 <- foreach(med=1:k,.combine='cbind')%:%
        foreach(sel=1:n, .combine='c') %dopar% 
        {
          fil <- pam_fit[med]
          bb <- df[fil,]
          
          # if(sel <= 1)
          # {
          #   daisy(rbind(bb,df[sel:(sel+1),]),metric = dd.int)[1]
          # }
          # if(sel > (n-1))
          # {
          #   daisy(rbind(bb,df[(sel-1):sel,]),metric = dd.int)[2]
          # }
          # if(sel >1 & sel<n)
          # {
          #   daisy(rbind(bb,df[(sel-1):(sel+1),]),metric = dd.int)[2]
          # }
          daisy(rbind(bb,df[(sel-1):(sel+1),]),metric = dd.int)[2]
          # daisy(rbind(bb,df[sel,]),metric = dd.int)[1]
        }
    }
    if(clus ==FALSE)
    {# Begin serial version
      sol2 <- matrix(NA, ncol = k, nrow = n)
      for(med in 1:k)
      {
        for(sel in 1:n)
        {
          fil <- pam_fit[med]
          bb <- df[fil,]
          
          if(sel == 1)
          {
            sol2[sel,med] <- daisy(rbind(bb,df[sel:(sel+1),]),metric = dd.int)[1]
          }
          if(sel == n)
          {
            sol2[sel,med] <- daisy(rbind(bb,df[(sel-1):sel,]),metric = dd.int)[2]
          }
          if(sel >1 & sel<n)
          {
            sol2[sel,med] <- daisy(rbind(bb,df[(sel-1):(sel+1),]),metric = dd.int)[2]
          }
        }
      }
      
    }
    
    # END: each object not belonging to the sample is 
    # assigned to the nearest of the k representative objects
    
    # posclus: each object not belonging to the sample is 
    # assigned to the nearest of the k representative objects
    posclus <- apply(sol2,1,which.min) #cluster
    pam_fit[posclus]
    MOi <- apply(sol2,1,min) #distance in posclus
    
    # costo1: average distance between each object of 
    # the data set and its representative objec
    costo1 <- cbind(costo1,tapply(MOi,posclus,mean))
    #cost0: lowest average distance
    # costo0 <- c(costo0,mean(tapply(MOi,posclus,mean)))# esta es la funcion adecuada
    costo0 <- c(costo0,mean(MOi))# esta es la funcion adecuada
    
    
    
    # cat("costo0: ",costo0,"\n")
    # cat("costo0 Min: ",min(costo0),"\n")
    closest.d.MOi <- cbind(closest.d.MOi,MOi)
    clustering <- cbind(clustering,posclus)
    Md <- cbind(Md, pam_fit)
    
    # if(sam >1)
    # {
    #   costo0[sam]<costo0[sam-1]
    #   print("BREAAAk")
    #   break
    #   
    # }
    
    
    
  }
  
  cs <- which.min(costo0)# chosen sample
  
  
  # OUTPUT: K&R pag132
  #random sample:
  rsam <- rsam[,cs]
  i.med <- Md[,cs] #final medoids id
  medoids <- df[i.med,]
  av_diss <- costo1[,cs] #average distance to each medoid
  tot_av_diss <- (costo0[cs]) # average distance for entire dataset & Objective function
  clustering <- clustering[,cs]
  clsize <- as.numeric(table(clustering))#cluster size
  # cat("COSTO0",costo0[1],cs,"\n")
  clusinfo <- cbind(size = as.numeric(clsize), av_diss = as.numeric(av_diss))
  
  return(list(sample = rsam, medoids = medoids,i.med = i.med,
              clustering = clustering,clusinfo = clusinfo,tot_av_diss = tot_av_diss))
}