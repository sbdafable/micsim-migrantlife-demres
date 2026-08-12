
MICROSIMUALTION SOFTWARE FOR CONTINUOUS-TIME MODELS BASED ON MARKOV PROCESSES.

----------------------------------------------------------------------------------------

THIS IS THE FAST VERSION OF THE MICROSIMULATION IMPLEMENTATION OF THE R PACKAGE MicSim.  

THIS IMPLEMENTATION USES JAVA SOURCE CODE (packed in MicCore.jar) THAT IS EXECUTED       
VIA CALLING IT IN R.                                                                     	
											   			
 Date: July 2019	
 
 Author: Sabine Zinn									   
 
 Email: szinn(at)diw.de									   	
 
----------------------------------------------------------------------------------------

The MicCore and the MicSim package are described in detail in

 - Zinn, S., Gampe, J., Himmelspach, J., & Uhrmacher, A. M. (2009, December). Mic-Core: A tool for microsimulation. In Proceedings of the 2009 Winter Simulation Conference (WSC) (pp. 992-1002). IEEE.
 - Zinn, S. (2014). The MicSim package of R: an entry-level toolkit for continuous-time microsimulation. International Journal of Microsimulation, 7(3), 3-32.
  
-------------------------------------------------------

Processing for running the (fast) microsimulation via R

-------------------------------------------------------

Beware that for running the java executable (MicCore.jar) on your computer a JVM (java virtual machine) has to be installed.
(Is this the case? -> Look at System -> Programs installed -> Java should be installed. 
Otherwise install it: https://java.com/de/download/)



A. DOWNLOAD OF FILES (from GitHub repos)

You have to download the folling three auxiliary files and to store on your compter. 

- auxFctInMicSim.r (Auxiliary functions for defining the microsimulation model.)

- MicCore1.0.jar (The java executable running the microsimulation fast.)

- auxFctOutMicSim.r (Auxiliary functions for transforming the output files received from the java implementation MicCore.)

(Note: The paths to each of these files have to be set in the files below. The exact location is marked in the respective file.)



B. DEFINE AND RUN MICROSIMULATION, AND GET OUTPUT: An EXAMPLE 

Run an microsimulation example by running the following (example) files in the order 1. -> 2. -> 3.


1. Load defineMicInput.r (downloadable from GitHub repos as well)

(Here you have to define the microsimulation input, i.e., the base population and transition rates (and optionally number of migrants). This is done in the same way as when using the MicSim package. An example is given in the file. Finally, the function writeInputMicCore() writes the input files for MicCore.jar. These include a file with the base population, a file with transition rates, and optionally a file with numbers of migrants.) 


2. runMicCoreJar.r (downloadable from GitHub repos as well)

(Runs the MicCore.jar, gives two output files (one with information on the birth dates and states of the simulated individuals and one with information on their transitions during simulation. Here you only have to adapt the simulation horizon and the paths to your personal settings. An example is given in the file.)

3. evalMicOutput.r (downloadable from GitHub repos as well)

(Transforms MicCore output in R matrix with a row for every event. Here you only have to adapt the paths to your personal settings.)


