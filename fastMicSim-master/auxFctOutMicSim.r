####################################################################################
####################################################################################
## AUXILIARY FUNCTIONS TO CONSTRUCT EVENT HISTORIES FROM MICROSIMULATION OUTPUT   ##
## SZ, January 2014                                                              ##
####################################################################################
####################################################################################

# date format
dateForm <- c(dates = "m/d/year", times= "h:m:s")

getLatestFiles <- function(pathOutputData){ 
  #pathOutputData <- "C:/Users/Biene/Documents/MicCoreInR/res"
  fileList <- list.files(pathOutputData)
  birthFiles <- fileList[which(grepl("birth",fileList))] 
  phaseFiles <- fileList[which(grepl("phase",fileList))] 
  datE <- matrix(unlist(strsplit(phaseFiles, split="Sim")), ncol=2, byrow=T)[,2]
  datE <- gsub(x =datE, replacement="", pattern=".txt")
  matE <- matrix(unlist(strsplit(datE, split="_")),ncol=2,byrow=2)
  createChron <- function(vec){
    cr <- chron(dates.=vec[1], times.=vec[2], format=c(dates="dMy", times="hms"), out.format=c(dates="d/m/y", times="h:m:s"))
    return(cr)  
  }
  inO <- order(apply(matE,1,createChron),decreasing = TRUE)[1]
  bF <- paste(pathOutputData,birthFiles[inO],sep="/")
  pF <- paste(pathOutputData,phaseFiles[inO],sep="/")  
  bL <- list(birthfile=bF, phasefile=pF)
  return(bL)  
}

# Using readBirthFile() we read in microsimulation output on birth dates of
# simulated individuals.
readBirthFile <- function(birthfile) {
  bf <- scan(file = birthfile ,sep=";", skipNul=T)
  bmat <- matrix(bf, ncol = 6, byrow = TRUE)[, -6]
  # bmat <- matrix(bf, ncol = 5, byrow = TRUE)
  return(bmat)
}

# Using readPhaseFile() we read in microsimulation output on transition dates
# and related destination states
readPhaseFile <- function(phasefile) {
  pf=scan(file = phasefile , what= character (), sep=";", skipNul=T)
  pmat <- matrix(pf, ncol = 6, byrow = TRUE)[, -6]
  #pmat <- matrix(pf, ncol = 5, byrow = TRUE)
  return(pmat)
}

# Using readData() we preparate the output data of the MicMac microsimulation.
readData <- function(birthfile, phasefile){
  bm <- readBirthFile(birthfile)
  pm <- readPhaseFile(phasefile)  
  pd <- pm[, - c(1,2)]
  pd1 <- pd[ order(as.numeric(pd[,1])), ]
  bd <- bm[, - c(1,2,4)] 
  return(list(p.d=pd1,b.d=bd))
}

isLeapYear <- function(year) {  
  if (((year %% 4 == 0) & (year %% 100 != 0)) || (year %% 400 == 0))
    return(TRUE) 
  return(FALSE) 
}

giveCorrectAge <- function(bd, cd) {  
  b.y <- as.numeric(as.character(years(bd)))
  b.m <- as.numeric(months(bd))
  b.d <- as.numeric(as.character(days(bd)))
  c.y <- as.numeric(as.character(years(cd)))
  c.m <- as.numeric(months(cd))
  c.d <- as.numeric(as.character(days(cd)))
  completeYears <-  c.y - b.y - 1  
  daysInBY <- ifelse(isLeapYear(b.y), 366, 365)
  daysInCY <- ifelse(isLeapYear(c.y), 366, 365)
  fracBY <- as.numeric(chron(paste(31,"/",12,"/",b.y), format=c(dates="d/m/y")) - bd)/daysInBY
  fracCY <- as.numeric(cd - chron(paste(1,"/",1,"/",c.y), format=c(dates="d/m/y")) + 1)/daysInCY  
  return(completeYears +  fracBY  + fracCY)
}

# This function creates event histories of simulated individuals.
# Output argument: matrix containing event histories of simulated individuals
constructEventHistories <- function(data) { 
  birth.data <- data$b.d
  phase.data <- data$p.d 
  bds <- birth.data[order(birth.data[,1]),]
  colnames(bds) <- c("ID","BirthDate")
  phs <- phase.data[order(as.numeric(as.vector(phase.data[,1]))), ]
  colnames(phs) <- c("ID","TrDate","NewState") 
  phs <- as.data.frame(phs) 
  phs[,1] <- as.numeric(as.vector(phs[,1]))
  dat <- merge(as.data.frame(bds), as.data.frame(phs), by="ID")
  dat.s <- dat[order(as.numeric(as.vector(dat[,1])), dat[,3]),]
  bdDate <- chron(dat.s[,2]/1000/60/60/24 + 1/24, format=dateForm)
  phDate <- chron(as.numeric(as.vector(dat.s[,3]))/1000/60/60/24 + 1/24, format=dateForm)  
  ageAtTrY <- round(giveCorrectAge(bdDate, phDate),digits=2) # in years
  datN <-   data.frame(ID  = dat.s[,1], BirthDate = bdDate, TrDate = phDate, 
                       AgeAtTr = ageAtTrY, NewState = dat.s[,4])  
  datN <- datN[order(datN[,1], datN[,3]),]
  print("Transformation has been successfully done!")
  return(datN)
}


