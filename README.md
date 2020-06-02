# BARAAHP
R code for paper "Between a rock and a hard place: adaptive sensing and site-specific dispersal" 
Nichols, B.S., Leubner-Metzger, G. and Jansen, V.A.A.(DOI:10.1111/ele.13564)

Licence: CC-BY (https://creativecommons.org/licenses/by/4.0/)

Any problems, email: bethany.nichols.2017@live.rhul.ac.uk

Cite: Nichols, B.S., Leubner-Metzger, G. and Jansen, V.A.A. (2020). Between a rock and a hard place: adaptive sensing and site-specific dispersal. Ecology Letters.

In this folder you will find the R script "BetweenARockAndAHardPlace.R"
To run the R script, you will need packages "cowplot" and "reshape 2". This is a bit of an interactive code, so if you want plots to save at each point, you will need to switch on the sections as listed below.

FunA is the basal function that runs the model simulation 1000 times with one set of variables. It outputs the final Q.

FunB is runs FunA, changing d1 and d2 each time and recoding the max fitness Q. I leave the heatmap plotting (lines 97-120) switched off when I run the rest of the code. When you first open it, this will be switched off. It outputs the max Q for all d1 and d2 combinations.

FunC produces Fig. 4 by maping the max Q from FunB and mapping the corresponding d1 and d2 against fecundity of sub-habitat S2. It records the zones produced (as shown in the main manuscript) and outputs these. I switch off the plot when I run FunD() (lines 144-187)

FunD produces Fig. 5 by running FunC at a range of severities and plots the change in zones as outputted by Fig. 4. 
