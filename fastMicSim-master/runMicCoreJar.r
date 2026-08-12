####################################################################################
####################################################################################
## 2. EXECUTION OF MICROSIMULATION MODEL                                          ##
##                                                                                ##
## Date: July 2019                                                                ##
## Author: Sabine Zinn                                                            ##
## Email: szinn@diw.de                                                            ##
##                                                                                ##
####################################################################################
####################################################################################

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
# A. DEFINE PATHS & SET PARAMETERS FOR MicCore 
# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

# Define path to aux functions for running microsimulation
path <- "C:/Users/Biene/Documents/MicCoreInR/R/" # TODO: CHANGE TO PERSONAL SETTINGS

# Load aux functions for running microsimulation
source(paste(path,"auxFctInMicSim.r",sep=""))

# Tell R where to find the input files for the MicCore.jar
pathInputData <- "C:/Users/Biene/Documents/MicCoreInR/MicCoreIn/" # TODO: CHANGE TO PERSONAL SETTINGS

# Tell R where to store the output of the MicCore.jar (microsimulation output)
pathOutputData <- "C:/Users/Biene/Documents/MicCoreInR/res" # TODO: CHANGE TO PERSONAL SETTINGS

# Tell R where to find the MicCore.jar (executable to run the microsimulation)
pathMicCore <- "C:/Users/Biene/Documents/MicCoreInR/MicCore" # TODO: CHANGE TO PERSONAL SETTINGS

# Tell R when to start the simulation
startDate <- "01/01/2000" # TODO OPTIONAL: CHANGE TO PERSONAL SETTINGS
# Tell R when to stop the simulation 
stopDate <- "31/12/2010" # TODO OPTIONAL: CHANGE TO PERSONAL SETTINGS
# (Both dates have to be equal to the dates defined for the input files.)

# Set a random seed
seed <- 23456 # TODO OPTIONAL: CHANGE TO PERSONAL SETTINGS

# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------
# B. RUN THE MICCORE
# ----------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------

# Just run this without changing anything.
runMicCommand <- buildCommand(pathInputData=pathInputData, pathOutputData=pathOutputData,
  pathMicCore=pathMicCore, startDate=startDate, stopDate=stopDate,
  seed=seed, mig=T)
setwd(pathMicCore)
Sys.time()
system(command=runMicCommand, intern= TRUE, show.output.on.console = TRUE, invisible = TRUE)
Sys.time()
