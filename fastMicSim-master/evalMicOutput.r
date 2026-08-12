####################################################################################
####################################################################################
## 3. GET OUTPUT OF MICROSIMULATION RUN                                           ##
##                                                                                ##
## Date: July 2019                                                                ##
## Author: Sabine Zinn                                                            ##
## Email: szinn@diw.de                                                            ##
##                                                                                ##
####################################################################################
####################################################################################

# Tell R where to find the auxiliary files to read the MicCore.jar output 
# (MicCore generates two output files: one with birth information and one with information 
# on transitions during simulation.)
path <- "C:/Users/Biene/Documents/MicCoreInR/R/" # TODO: CHANGE TO PERSONAL SETTINGS
source(paste(path,"auxFctOutMicSim.r",sep=""))

# Tell R where to find the MicCore.jar output files.
pathOutputData <- "C:/Users/Biene/Documents/MicCoreInR/res" # TODO: CHANGE TO PERSONAL SETTINGS

# Here R reads the two output files that have been generated latest.
files <- getLatestFiles(pathOutputData)
# Give the path+name of the birth and subsequently the transition file to the function "readData" as follows.
data <- readData(files$birthfile, files$phasefile)
# Constuct matrix with a row for each transition (as the output of the MicSim package). 
eventHistories <- constructEventHistories(data)

