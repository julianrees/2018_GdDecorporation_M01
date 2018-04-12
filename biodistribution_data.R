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
days <- list()

std_bottles = 9
groups = 18






days[[1]] <- c('Standard',1,2)
days[[2]] <- c('Standard',3,4,5,6)
i = 1
for (i in length(days)){
  j = (2*i)-1
  k = 2*i
  rcounts[[i]] <- samplelist[-1,j:k]
  colnames(rcounts[[i]]) <- c('Sample','Group')
  rcounts[[i]]$Group[3:11] <- 'Standard'
  rcounts[[i]]$Group <- as.factor(rcounts[[i]]$Group)
  rcounts[[i]] <- rcounts[[i]][1:49,]
  rcounts[[i]] <- cbind(rcounts[[i]], read_csv('../Data/20180327_1613/ExperTable.csv', skip = 5)[,5])
  rcounts[[i]] <- rcounts[[i]][which(is.na(rcounts[[1]]$Group) == FALSE),]
  rcounts[[i]] <- cbind(rcounts[[i]], Day = NA)
  
  
  rcounts[[i]]$Day[1:9] <- days[[i]][1]
  rcounts[[i]]$Day[10:27] <- days[[i]][2]
  rcounts[[i]]$Day[28:45] <- days[[i]][3]
  rcounts[[i]]$Sample[10:45] <- "Urine"
}



rcounts[[1]] <- samplelist[-1,colm[[1]]]
colnames(rcounts[[1]]) <- c('Sample','Group')
rcounts[[1]]$Group[3:11] <- 'Standard'
rcounts[[1]]$Group <- as.factor(rcounts[[1]]$Group)
rcounts[[1]] <- rcounts[[1]][1:49,]
rcounts[[1]] <- cbind(rcounts[[1]], read_csv('../Data/20180327_1613/ExperTable.csv', skip = 5)[,5])
rcounts[[1]] <- rcounts[[1]][which(is.na(rcounts[[1]]$Group) == FALSE),]
rcounts[[1]] <- cbind(rcounts[[1]], Day = NA)


rcounts[[1]]$Day[1:9] <- days[1]
rcounts[[1]]$Day[10:27] <- days[2]
rcounts[[1]]$Day[28:45] <- days[3]
rcounts[[1]]$Sample[10:45] <- "Urine"
str(rcounts[[1]])





rcounts[[1]]



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
