## required packages ##
library(cowplot)
library(reshape2)



## FunA - runs the basic model simulation with one set of variables
## FunB - generates Fig.3 heat maps looking at change in Q with changes in d1 and d2
## FunC - generates Fig. 4 plot by taking max Q with d1 and d2 from FunB at different S2
## FunD - generates Fig. 5 plot by looking at zone changes in FunC with changes in severity (v)



## FunA - the basal function that runs the model simulation with one set of input variables ##
FunA <- function(d1 = 0.2, d2 = 0.2, S1 = 18, 
                 S2 = 21, m = 0.76, v = 21, f1 = 5, c = 0.1) {
  
  ## setting up the environment with a population of 1 in each sub-habitat
  N <- c(1, 1) #starting population sizes
  Q <- 0
  f <- f1
  
  ## MatG is the matrix for good years
  ## MatB is the matrix for bad years
  MatG <- matrix(c((1-d1*(1-c))*(S1+(v/f)), ((1-m)*d1*(S1+(v/f)))*(1-c), 
                   ((1-m)*d2*S2)*(1-c), (1-d2*(1-c))*S2), nrow = 2, ncol = 2)
  MatB <- matrix(c((1-d1*(1-c))*(S1-(1-(1/f))*v), ((1-m)*d1*(S1-(1-(1/f))*v))*(1-c), 
                   ((1-m)*d2*S2)*(1-c), (1-d2*(1-c))*S2), nrow = 2, ncol = 2)
  
  ## Runs the simulation over 1000 iterations, if you change this to speed it up, make sure
  ## you change the number you're dividing Q by in line 44
  for (a in seq(1, 1000)){
    if (a%%f1 == 0){ ## if multiple of f
      N <- MatB %*% N ## multiplies N by MatB
      Q <- Q + log(N[1]+N[2]) ## stores log of N1+N2 as Q
      N <- c(N[1]/(N[1]+N[2]), N[2]/(N[1]+N[2])) ## sets new population for next run
    } else { ## otherwise 
      N <- MatG %*% N ## multiplies N by MatG
      Q <- Q + log(N[1]+N[2])
      N <- c(N[1]/(N[1]+N[2]), N[2]/(N[1]+N[2]))
    }
  }
  
  Q <- Q/1000 ## calculates final fitness
  return(Q) ##outputs the final fitness
  
}



## FunB generates heat maps Fig.3, investigating change in Q with different d1 and d2
FunB <- function(Severity = 35, Frequency = 3, S1 = 18, S2 = 21) {
  
  ## Sets up data frame to store Q against different d1 and d2
  QData1 <- data.frame(matrix(NA, ncol = 3, nrow =10201))
  names(QData1) <- c('D1', 'D2', 'Q')
  QData1$D1 <- rep(seq(0., 1., 0.01), 101, each = 1)
  QData1$D2 <- rep(seq(0., 1., 0.01), 1, each = 101)

  ## Runs FunA with change in d1 and d2 with each iteration
  for (b in seq(1, length(QData1$D1))) {
    Q <- FunA(d1 = QData1$D1[b], d2 = QData1$D2[b], S1 = S1, 
              S2 = S2, m = 0., v = Severity, f1 = Frequency,  c = 0.0)
    QData1$Q[b] <- Q ## stores output from FunA in dataframe
  }
  
  ## Stores the maximum value of Q along with associated d1 and d2 in dataframe
  ## called max1
  max1 <- which.max(QData1$Q)
  max1 <- QData1[max1,]
  
  ## newdata looks for the maximum value of Q if d1=d2
  newdata <- data.frame(matrix (NA, ncol = 3, nrow = 10201))
  names(newdata) <- c('D1', 'D2', 'Q')
  
  ## loop looks for all occurrances of d1=d2 and stores them with their Q 
  ## value in newdata
  for (a in seq(1, length(QData1$D1))) {
    if (QData1$D1[a] == QData1$D2[a]) {
      newdata$D1[a] <- QData1$D1[a]
      newdata$D2[a] <- QData1$D2[a]
      newdata$Q[a] <- QData1$Q[a]
    } else {
      newdata$D1[a] <- "NA"
    }
  }
  
  ## removes rows where d1=/=d2
  newdata<- newdata[rowSums(is.na(newdata)) ==0,]
  
  ##finds the maximum Q in newdata  where d1=d2 and stores this as max2
  max2 <- which.max(newdata$Q)
  max2 <- newdata[max2,]
  max2 <- as.numeric(as.character(max2))
  
  
  ## draws the heatmap of Q with changes in d1 and d2 - I leave this switched off when
  ## running FunC and FunD
  # HeatMap1 <- ggplot(data = QData1, aes(x = D1, y = D2, z = Q)) +
  #   geom_tile(aes(fill = Q)) +
  #   scale_fill_gradientn(colours = c("navyblue", "royalblue4", "steelblue3",
  #                                    "steelblue1", "springgreen3", "springgreen",
  #                                    "yellow1", "orange1", "red")) +
  #   geom_point(data = max1, aes(x = D1, y = D2), shape = 8, size = 3) + ##shows max Q where d1=/=d2
  #   geom_point(aes(x = max2[1], y = max2[2]), shape = 17, size = 3) + ##shows max Q where d1=d2
  #   geom_line(aes(x = D1, y = D1), size=1) + ##shows line where d1=d2
  #   theme(axis.ticks = element_blank(), panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         panel.background = element_blank(),
  #         axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  #   labs(title = "", x = "Dispersal Rate from Habitat One",
  #        y = "Dispersal Rate from Habitat Two", fill = 'Fitness') +
  #        scale_x_continuous(expand = c(0,0)) +
  #        scale_y_continuous(expand = c(0,0))
  # 
  # ## stores plot with variables information
  # HeatMapName <- paste("Severity-", Severity,"-YearOrder-", Frequency, "-S1-", S1, "-S2-", S2, ".png", sep="")
  # png(file = HeatMapName)
  # plot(HeatMap1)
  # dev.off()
  
  ## outputs max1
  return(max1)
  
}


## FunC produces Fig. 4 by taking the max Q from FunB along with corresponding d1 and d2
## and maps Q with a change in fecundity of sub-habitat 2 (S2)
FunC <- function(v = 21) {
  ## creates a dataframe to store data for S2, and d1 and d2 from maxdata from FunB
  SevData <- data.frame(matrix(NA, ncol = 3, nrow =48))
  names(SevData) <- c('S2', 'D1', 'D2')
  SevData$S2 <- seq(0.5,24,0.5)
  
  ## runs through different S2 in FunB and saves resulting max d1 and d2 from FunB output
  for (loopy in seq(1, length(SevData$S2))){
    MaxData <- FunB(Severity = v, Frequency = 5, S1 = 18, S2 = SevData$S2[loopy])
    SevData$D1[loopy] <- MaxData$D1
    SevData$D2[loopy] <- MaxData$D2
  }
  
  ## This whole next bit is for drawing Fig. 4. I leave it turned off for running FunD.
  # ## prepares background zones - if you change the inputs for this function, in order to 
  # ## move the zones, you'll need to read off the bottom of the graph and manually move these
  # ## to the correct locations
  ## removes parts of graph where the maths suggests these regions are unstable
  ## don't run these three lines (148-150) if you're running FunD()
  # SevData$D2[1:9] <- NA
  # SevData$D1[37:48] <- NA
  # SevData$D1[37] <- SevData$D1[36]
  
  # rects <- data.frame(xstart = c(0, 4.5, 18), xend = c(4.5, 18, 24), col = letters[1:3])
  # 
  # ## Plots the results
  # Plot <- ggplot() +
  #   ## this first section sets the position for the background zones
  #   geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
  #                               ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
  #   scale_fill_manual(values = alpha(c("chocolate", "burlywood2", "peachpuff",
  #                                      0.8))) + guides(fill = FALSE) 
  # Plot <- Plot +
  #   ## this section then adds the dashed lines to the background
  #   geom_vline(xintercept = 4.5, linetype = "dashed", color = "chocolate3", size = 1) +
  #   geom_vline(xintercept = 18, linetype = "dashed", color = "burlywood3", size = 1) +
  #   ## here is where the lines are then added
  #   geom_line(data = SevData, aes(x = S2, y = D1,
  #                                 colour = "D1"), size  = 2) +
  #   geom_line(data = SevData, aes(x = S2, y = D2,
  #                                 colour = "D2"), size = 2) +
  #   ## and tidying up the graph
  #   labs(title = "", x = "S2", y = "Dispersal Rate", color = " ") +
  #   theme(legend.position = "right",
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         panel.background = element_blank(),
  #         axis.text=element_text(size=12),
  #         axis.title=element_text(size=14,face="bold")) +
  #   scale_color_manual(values = c("springgreen3", "steelblue")) +
  #   ylim(0., 1.5) + scale_x_continuous(expand = c(0,0))
  # 
  # ## saving Fig4.png
  # PlotName <- "Fig4.png"
  # png(file = PlotName)
  # plot(Plot)
  # dev.off()
  # SevData <- na.omit(SevData)
  
  ## the zones are then recorded as FoundIt for FunD and outputted
  FoundIt <- c(0, 0, 0, 0)
  for (findit in seq(1, length(SevData$S2))) {
    if (SevData$D1[findit]==0. & SevData$D1[findit+1]>0.) {
      FoundIt[1] <- SevData$S2[findit]
    }
    if (SevData$D2[findit+1]==0. & SevData$D2[findit]>0.) {
      FoundIt[3] <- SevData$S2[findit+1]
    }
    if (SevData$D1[findit+1]==1. & SevData$D1[findit]<1.) {
      FoundIt[2] <- SevData$S2[findit+1]
    }
    if (SevData$D2[findit]==1. & SevData$D2[findit+1]<1.) {
      FoundIt[4] <- SevData$S2[findit]
    }
  }
  
  return(FoundIt) 
}



## FunD generates Fig. 5 by looking at the change in zone width from FunC and mapping this
## with a change in severity
FunD <- function() {
  ## setting up data frame for change in severity and zone changes
  vData <- data.frame(matrix(NA, ncol = 5, nrow =46))
  names(vData) <- c('v', 'P1', 'P2', 'P3', 'P4')
  vData$v <- seq(0.5,23,0.5)
  
  
  ## runs FunC and saves the zone positions against the severity they were run against
  for (runtime in seq(1, 46)) { 
    FoundIt <- FunC(v = vData$v[runtime])
    vData$P1[runtime] <- FoundIt[1] 
    vData$P2[runtime] <- FoundIt[2] 
    vData$P3[runtime] <- FoundIt[3]
    vData$P4[runtime] <- FoundIt[4]
    print(FoundIt)
  }
  
  
  ## plotting the results
  Plot3 <- ggplot(data = vData, aes(y = P1, x = v)) +
    ##setting up the three zones 
    geom_ribbon(data = vData, aes(ymin = P3, ymax = 24), alpha = 0.4, fill = "peachpuff") + 
    geom_ribbon(data = vData, aes(ymin = P1, ymax = P3), alpha = 0.4, fill = "burlywood2") +    
    geom_ribbon(data = vData, aes(ymin = 0, ymax = P1), alpha = 0.4, fill = "chocolate") +
    ##tidying up the graph
    labs(title = "", x = "v", y = "", color = " ") +  scale_x_continuous(expand = c(0,0)) +
     scale_y_continuous(expand = c(0,0))
  ## Adds dashed lines
  Plot3 <- Plot3 + geom_line(aes(x = vData$v, y = vData$P1), color = "chocolate3", size = 1, linetype = "dashed")
  Plot3 <- Plot3 + geom_line(aes(x = vData$v, y = vData$P3), color = "burlywood3", size = 1, linetype = "dashed")
  Plot3 + coord_flip() 
  
  ## saving Fig5.png
  PlotName <- "Fig5.png"
  png(file = PlotName)
  plot(Plot3)
  dev.off()
  
}
