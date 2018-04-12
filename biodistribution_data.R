#---- Header content ----
library(ggplot2)
library(dplyr)
library(reshape2)
library(outliers)
# library(xlsx)
library(readxl)
library(readr)

setwd('./')

#---- Data import ----
samplelist <- read_excel('../Data/18-M01 LSC sample order.xlsx')
rcounts <- list()
rcounts[[1]] <- samplelist[-1,1:2]



c27 <- samplelist[-1,1:2]
colnames(c27) <- c('Sample','Group')
c27$Group[3:11] <- 'Standard'
c27$Group <- as.factor(c27$Group)
c27 <- c27[1:49,]
c27 <- cbind(c27, read_csv('../Data/20180327_1613/ExperTable.csv', skip = 5)[,5])
c27 <- c27[which(is.na(c27$Group) == FALSE),]
c27 <- cbind(c27, Day = NA)


days <- c('Standard',1,2)
c27$Day[1:9] <- days[1]
c27$Day[10:27] <- days[2]
c27$Day[28:45] <- days[3]
c27$Sample[10:45] <- "Urine"
str(c27)


c29 <- samplelist[-1,3:4]
colnames(c29) <- c('Sample','Group')
c29$Group[3:11] <- 'Standard'
c29$Group <- as.factor(c29$Group)
c29 <- c29[1:72,]
c29 <- cbind(c29, read_csv('../Data/20180329_1108/ExperTable.csv', skip = 5)[,5])
c29 <- c29[which(is.na(c29$Group) == FALSE),]
c29 <- cbind(c29, Day = NA)


days <- c('Standard',3,4,5,6)
c29$Day[1:9] <- days[1]
c29$Day[10:27] <- days[2]
c29$Day[28:45] <- days[3]
c29$Day[46:63] <- days[4]
c29$Day[64:66] <- days[5]
c29$Sample[10:65] <- "Urine"

cpm <- rbind(c27,c29)

ggplot(cpm[which(cpm$Sample == "Urine"),], aes(x = Day, y = CPMA, by = Group)) + 
  geom_col(aes(fill = Group), position = "dodge")
