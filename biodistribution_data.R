#---- Header content ----
library(ggplot2)
library(dplyr)
#library(plyr)
library(reshape2)
library(outliers)
# library(xlsx)
library(readxl)
library(readr)

setwd('./')
#---- Experiment details ----
nGroups = 18
groupSize = 3
excretaDays = 8

#---- Data import ----
samplelist <- read_excel('LSC_sample_order.xlsx', col_names = FALSE)
counts <- read_csv('../Data/LSC/18M01_lsc1234.csv', col_names = FALSE)[,3]
pe_weights <- read_excel('../Data/18-M01_PE_Weights.xlsx', sheet = 2)

pe_weights <- cbind(pe_weights, sample_scale_factor = 
                    1 - (pe_weights$Sample - pe_weights$Tare) / (pe_weights$Total - pe_weights$Tare))

samples <- cbind(samplelist, counts)
colnames(samples) <- c('Group', 'Organ', 'Individual','CPM')
str(samples)
route <- array(dim = nrow(samples))
route[which(samples$Group == 'B' | samples$Group == 'C' | samples$Group == 'D' | samples$Group == 'E' | 
        samples$Group == 'F' | samples$Group == 'G' | samples$Group == 'H' | samples$Group == 'I')] <- 'IP'
route[which(samples$Group == 'K' | samples$Group == 'L' | samples$Group == 'M' | samples$Group == 'N' | 
              samples$Group == 'O' | samples$Group == 'P' | samples$Group == 'Q' | samples$Group == 'R')] <- 'PO'
samples <- cbind(samples, Route = as.factor(route))
standards <- samples[which(samples$Group == 'STD'),]
standards <- cbind(standards, pe_weights[1:nrow(standards),6])
pe_weights <- pe_weights[-1:-nrow(standards),]

samples <- samples[-which(samples$Group == 'STD'),]
samples <- samples[-which(is.na(samples$Organ) == 'TRUE'),]

tissues <- samples[which(samples$Organ != 'Feces' & samples$Organ != 'Urine'),]
tissues <- cbind(tissues, sample_scale_factor = 1)
tissues$sample_scale_factor[which(tissues$Organ == 'Soft')] <- 
  pe_weights$sample_scale_factor[(nGroups*excretaDays+1):(nGroups*excretaDays+(nGroups*groupSize))]
tissues$sample_scale_factor[which(tissues$Organ == 'Skeleton')] <- 
  pe_weights$sample_scale_factor[(nGroups*excretaDays+(nGroups*groupSize)+1):(nGroups*excretaDays+(2*nGroups*groupSize))]
tissues <- cbind(tissues, Scaled_CPM = tissues$CPM / tissues$sample_scale_factor)

excreta <- samples[which(samples$Organ == 'Feces' | samples$Organ == 'Urine'),]
colnames(excreta) <- c('Group','Type','Day','CPM','Route') 
excreta <- cbind(excreta, sample_scale_factor = 1)
excreta$sample_scale_factor[which(excreta$Type == 'Urine')] <- 
  pe_weights$sample_scale_factor[(1:(nGroups*excretaDays))]
excreta <- cbind(excreta, Scaled_CPM = excreta$CPM / excreta$sample_scale_factor)



# Compile all the mice and convert to the %RD 

mice <- dcast(tissues[,c(1:3,5,7)], Individual+Group+Route ~ Organ, value.var = 'Scaled_CPM')
gr_excreta <- dcast(excreta[,c(1:3,5,7)], Group+Route+Type ~ Day, value.var = 'Scaled_CPM')

gr_excreta <- cbind(gr_excreta, rowSums(gr_excreta[,-1:-3]))
tripl_excreta <- rbind(gr_excreta[,c(1,3,12)],gr_excreta[,c(1,3,12)],gr_excreta[,c(1,3,12)])
tripl_excreta <- tripl_excreta[order(tripl_excreta$Group),]

mice <- mice[order(mice$Group),]

mice <- cbind(mice, t(matrix(tripl_excreta[,3]/3, nrow = 2, ncol = 54, byrow = FALSE)))
mice <- cbind(mice, Total = rowSums(mice[,4:15]))

doub_totals <- rbind(aggregate(Total ~ Group, mice, sum), aggregate(Total ~ Group, mice, sum))
doub_totals <- doub_totals[order(doub_totals$Group),]

gr_excreta <- cbind(gr_excreta[,-12], Group.Totals = doub_totals[,2])

mice[,4:16] <- mice[,4:16] / mice[,16] * 100
gr_excreta[,4:12] <- gr_excreta[,4:12] / gr_excreta[,12] * 100

colnames(mice) <- c(colnames(mice)[1:13], 'Feces', 'Urine','Total')

rd <- melt(mice, id = c('Group','Individual','Route'))
colnames(rd) <- c(colnames(rd)[1:3],'Organ','RD')

rd_org <- rd[which(rd$Organ != 'Urine' & rd$Organ != 'Feces' & rd$Organ != 'Total'),]

mexcreta <- melt(gr_excreta[,-12], id = c('Group', 'Route', 'Type'))
colnames(mexcreta)[4:5] <- c('Day','RD')



ggplot(rd_org, aes(x = Group, y = RD)) + 
  geom_jitter(aes(color = Group)) + 
  theme_bw() + 
  facet_wrap(~Organ) + 
  ylim(0,max(rd_org$RD))

ggplot(mexcreta, aes(x = Day, y = RD)) + 
  geom_col(aes(fill = Group), position = 'dodge') +
  theme_bw() + 
  facet_wrap(~Type)

ggplot(mexcreta, aes(x = Day, y = RD)) + 
  geom_col(aes(fill = Type), position = 'dodge') +
  theme_bw() + 
  facet_wrap(~Group)

ggplot(mexcreta, aes(x = Group, y = RD)) + 
  geom_col(aes(fill = Day)) + 
  theme_bw() + 
  facet_wrap(~Type)
