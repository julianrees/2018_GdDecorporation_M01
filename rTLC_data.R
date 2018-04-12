library(ggplot2)
library(reshape2)
library(xlsx)
library(plyr)

#---- LOAD THE RTLC DATA ----
setwd('/Volumes/Seagate Backup Plus Drive/Projects/Gd/GdDOTA_18-M01/')
wedta <- read.csv('Data/TLC/JR-A-189-02.txt')
noedta <- read.csv('Data/TLC/JR-A-189-03.txt')

#---- ADJUST THE POSITION FOR SOLVENT FRONT ----
# plate B, without EDTA, went 6 mm
# plate A, with EDTA, went 8 mm

#---- COMBINE THE TRACES ----

tlc <- wedta
tlc <- cbind(tlc, edta = 'EDTA')
noedta <- cbind(noedta, edta = 'NO EDTA')
tlc <- rbind(tlc, noedta)
colnames(tlc) <- c('Position', 'Counts', 'EDTA')

#---- PLOT THE DATA ----



ggplot(tlc, aes(x = Position, y = Counts)) + 
  geom_line(aes(color = EDTA))
