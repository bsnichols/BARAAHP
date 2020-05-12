# BARAAHP
R code for paper "Between a rock and a hard place: adaptive sensing and site-specific dispersal" 
Nichols, B.S., Leubner-Metzger, G. and Jansen, V.A.A.

In this folder you will find the R script "BetweenARockAndAHardPlace.R"
To run the R script, you will need packages "cowplot" and "reshape 2"

FunA is the basal function that runs the model simulation 1000 times with one set of variables. It outputs the final Q.

FunB is runs FunA, changing d1 and d2 each time and recoding the max fitness Q. I leave the heatmap plotting switched off when I run the rest of the code. When you first open it, this will be switched off. It outputs the max Q for all d1 and d2 combinations.

FunC produces Fig. 4 by maping the max Q from FunB and mapping the corresponding d1 and d2 against fecundity of sub-habitat S2. It records the zones produced (as shown in the main manuscript) and outputs these.

FunD produces Fig. 5 by running FunC at a range of severities and plots the change in zones as outputted by Fig. 4. 
