####################################################################################
####################################################################################
## AUXILIARY FUNCTIONS TO DEFINE MICROSIMULATION INPUT                            ##
## SZ, November 2013                                                              ##
####################################################################################
####################################################################################

## Set simulation horizon
setSimHorizon <- function(startDate, endDate){
 dts <- c(startDate,endDate)
 simHorizon <- chron(dates=dts,format=c(dates="d/m/Y"),out.format=c(dates="d/m/year"))
 return(simHorizon)
}
##------------------------------------------------------------------------------------
#
# Construct matrix indicating transition pattern and naming the corresponding transition rate functions.
buildTransitionMatrix <- function(allTransitions,absTransitions,stateSpace){
 if(is.vector(absTransitions))
   absTransitions <- matrix(absTransitions, ncol=2, nrow=1)
 absStates <- absTransitions[,1]
 if(is.null(dim(stateSpace)))
  stateSpace <- matrix(stateSpace, ncol=1)
 transitionMatrix <- matrix(0,nrow=dim(stateSpace)[1], ncol=dim(stateSpace)[1]+length(absStates))
 colnames(transitionMatrix) <- c(apply(stateSpace,1,paste,collapse="/"),absStates)
 rownames(transitionMatrix) <- apply(stateSpace,1,paste,collapse="/")
 for(i in 1:length(absStates)){
   ia <- which(colnames(transitionMatrix)==absStates[i])
   transitionMatrix[,ia] <- absTransitions[i,2]
 }
 if(!is.null(allTransitions)){
   tr <- do.call(rbind,strsplit(allTransitions[,1],"->"))
   for(i in 1: dim(stateSpace)[1]){
    nn <- stateSpace[i,]
    for(j in 1:length(nn)){
      inff <- which(tr[,1] %in% as.character(unlist(nn[j])))
      dSS <- tr[inff,2,drop=F]
      dFF <- allTransitions[inff,2,drop=F]
      if(dim(dSS)[1]>0){
       nc <- nn
       for(k in 1:dim(dSS)[1]){
         nc[j] <- dSS[k]
         d <- which(colnames(transitionMatrix) %in% paste(sapply(nc, as.character), collapse="/"))
         transitionMatrix[i,d] <- dFF[k]
         nc <- nn
       }
      }
     }
    }
  }
  return(transitionMatrix)
}
#------------------------------------------------------------------------------------

# Build the command line for running the MicCore1.0.jar 
buildCommand <- function(pathInputData, pathOutputData, pathMicCore, startDate, stopDate, seed=2365, mig=T){
  c1 <- "java -Xmx1100m -Xms1100m -jar"
  c2 <- "MicCore1.0.jar"#paste(pathMicCore,"MicCore1.0.jar",sep="/")
  c3 <- paste(seed, startDate, stopDate, sep=" ")
  c4 <- paste(pathInputData,"trRates.txt",sep="/")
  c5 <- paste(pathInputData,"initPop.txt",sep="/")
  if(mig){
    c6 <- paste(pathInputData,"immigrPop.txt",sep="/")
  } else {
    c6 <- "\"\""
  }
  c7 <- pathOutputData
  command <- paste(c(c1,c2,c3,c4,c5,c6,c7),collapse=" ")
  return(command)
}
#------------------------------------------------------------------------------------


# Write Input files for MicCore.jar
writeInputMicCore <- function(pathOut, mig=F){
  initPopPath <- paste(pathOut,"initPop.txt",sep="/")
  initPopR <- initPop[,-1]
  initPopR[,2] <- gsub(pattern="/", replacement=". ", x=initPopR[,2])
  initPopR[,2] <- paste("(",initPopR[,2],")",sep="")
  initPopR[,1] <- paste(as.numeric(as.character(years(initPopR[,1]))), 
                        as.numeric(months(initPopR[,1])),
                        as.numeric(as.character(days(initPopR[,1]))),sep="-")
  write.table(initPopR, file=initPopPath, quote=FALSE, row.names=FALSE)
  
  if(mig){
    immigrPopPath <- paste(pathOut,"immigrPop.txt",sep="/")
    immigrPopR <- immigrPop[,-1] 
    immigrPopR[,3] <- gsub(pattern="/", replacement=". ", x=immigrPopR[,3])
    immigrPopR[,3] <- paste("(",immigrPopR[,3],")",sep="")
    immigrPopR[,1] <- paste(as.numeric(as.character(years(immigrPopR[,1]))), 
                            as.numeric(months(immigrPopR[,1])),
                            as.numeric(as.character(days(immigrPopR[,1]))),sep="-")
    immigrPopR[,2] <- paste(as.numeric(as.character(years(immigrPopR[,2]))), 
                            as.numeric(months(immigrPopR[,2])),
                            as.numeric(as.character(days(immigrPopR[,2]))),sep="-")
    write.table(immigrPopR, file=immigrPopPath, quote=FALSE, row.names=FALSE)
  }
  
  oStates <- c()
  dStates <- c()
  ratesArray <- array(NA, dim=c(maxAge+1,sum(transitionMatrix!="0"),diff(as.numeric(as.character(years(simHorizon))))+1))
  ages <- 0:maxAge
  index <- 1
  for(i in 1:dim(transitionMatrix)[1]){
    for(j in 1:dim(transitionMatrix)[2]){
      if(transitionMatrix[i,j]!="0"){
        oStates <- c(oStates, rownames(transitionMatrix)[i])
        dStates <- c(dStates, colnames(transitionMatrix)[j])
        for(year in as.numeric(as.character(years(simHorizon[1]))):as.numeric(as.character(years(simHorizon[2])))){
          res <- eval(do.call(transitionMatrix[i,j], args=list(age=ages,calTime=year,duration=0)))
          res[res==Inf] <- 1000
          ratesArray[,index,year-as.numeric(as.character(years(simHorizon[1]))) +1] <- res
        }
        index <- index+1
      }
    }
  }
  
  trRatesPath <- paste(pathOut, "trRates.txt",sep="/")
  if (file.exists(trRatesPath))
    file.remove(trRatesPath)
  zz <- file(trRatesPath,"a")
  cat(as.numeric(as.character(years(simHorizon))), file = zz, sep = " ", fill = FALSE, labels = NULL, append = TRUE)
  cat("\n", file = zz, sep="", fill = FALSE, labels = NULL, append = TRUE)
  cat(paste("(",paste(initStatesFem, collapse=". "),")",sep=""), file = zz, sep="\n", fill = FALSE, labels = NULL, append = TRUE)
  cat(initStatesProbFem, file = zz, sep="\n", fill = FALSE, labels = NULL, append = TRUE)
  cat(paste("(",paste(initStatesMale, collapse=". "),")",sep=""), file = zz, sep="\n", fill = FALSE, labels = NULL, append = TRUE)
  cat(initStatesProbMale, file = zz, sep="\n", fill = FALSE, labels = NULL, append = TRUE)
  cat(sexRatio, file = zz, sep="\n", fill = FALSE, labels = NULL, append = TRUE)
  cat("(dead)", file = zz, sep="\n", fill = FALSE, labels = NULL, append = TRUE)
  cat("(rest)", file = zz, sep="\n", fill = FALSE, labels = NULL, append = TRUE)
  cat(maxAge, file = zz, sep="\n", fill = FALSE, labels = NULL, append = TRUE)
  cat(paste("(",gsub(pattern="/", replacement=". ", x=oStates), ")",sep=""), file = zz, sep=" ", fill = FALSE, labels = NULL, append = TRUE)
  cat("\n", file = zz, sep="", fill = FALSE, labels = NULL, append = TRUE)
  cat(paste("(",gsub(pattern="/", replacement=". ", x=dStates), ")",sep=""), file = zz, sep=" ", fill = FALSE, labels = NULL, append = TRUE)
  cat("\n", file = zz, sep="", fill = FALSE, labels = NULL, append = TRUE)
  for(year in as.numeric(as.character(years(simHorizon[1]))):as.numeric(as.character(years(simHorizon[2])))){
    write.table(ratesArray[,,year-as.numeric(as.character(years(simHorizon[1]))) +1], file=zz, append=TRUE, quote=FALSE, 
                col.names=FALSE, row.names=FALSE) 
    cat("\n", file = zz, sep="", fill = FALSE, labels = NULL, append = TRUE)
  }
  close(zz)
  rm(zz)
}



  
