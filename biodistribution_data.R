#---- Header content ----
## @knitr header
library(ggplot2)
library(dplyr)
#library(plyr)
library(reshape2)
library(outliers)
library(multcomp)
# library(xlsx)
library(readxl)
library(readr)
library(knitr)

## @knitr data_processing
setwd('./')

nGroups = 18
groupSize = 3
excretaDays = 8
treatments = c('Control','-24h','-6h','-1h','+1h','+6h','+24h','+48h','+6d')
# does one comment hurt?
samplelist <- read_excel('LSC_sample_order.xlsx', col_names = FALSE)
counts <- read_csv('../Data/LSC/18M01_lsc1234.csv', col_names = FALSE)[,3]
pe_weights <- read_excel('../Data/18-M01_PE_Weights.xlsx', sheet = 2)

pe_weights <- cbind(pe_weights, sample_scale_factor = 
                    1 - (pe_weights$Sample - pe_weights$Tare) / (pe_weights$Total - pe_weights$Tare))

samples <- cbind(samplelist, counts)
colnames(samples) <- c('Group', 'Organ', 'Individual','CPM')
route <- array(dim = nrow(samples), 'IP')

route[which(samples$Group == 'J' | samples$Group == 'K' | samples$Group == 'L' | samples$Group == 'M' | samples$Group == 'N' | 
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
mice <- cbind(mice, Treatment = as.factor(rbind(treatments, treatments, treatments)))
mice$Treatment <- factor(mice$Treatment, levels = c('Control','-24h','-6h','-1h','+1h','+6h','+24h','+48h','+6d'))

levels(mice$Treatment)

rd <- melt(mice, id = c('Group','Individual','Route','Treatment'))
colnames(rd) <- c(colnames(rd)[1:4],'Organ','RD')

rd_org <- rd[which(rd$Organ != 'Urine' & rd$Organ != 'Feces' & rd$Organ != 'Total'),]


gr_excreta <- cbind(gr_excreta, Treatment = as.factor(rbind(treatments, treatments)))
gr_excreta$Treatment <- factor(gr_excreta$Treatment, levels = c('Control','-24h','-6h','-1h','+1h','+6h','+24h','+48h','+6d'))
mexcreta <- melt(gr_excreta[,-12], id = c('Group', 'Route', 'Type','Treatment'))
colnames(mexcreta)[5:6] <- c('Day','RD')

rd_org_select <- rd_org[which(rd_org$Organ == 'ART' | 
               rd_org$Organ == 'Kidneys' | 
               rd_org$Organ == 'Liver' | 
               rd_org$Organ == 'Skeleton' | 
               rd_org$Organ == 'Soft'),]



## @knitr plotting_headers
w = 0.65
fwid = 9
fhei = 6
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())

treatpalette = c('#41ad5b','#08519c','#3182bd','#6baed6','#9ecae1','#a50f15','#de2d26','#fb6a4a','#fc9272')
dayspalette = c('#f7fcf5','#e5f5e0','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#005a32')
dayspalette <- dayspalette[8:1]
##









ggplot(rd_org, aes(x = Group, y = RD)) + 
  geom_boxplot(aes(color = Group)) + 
  theme_bw() + 
  facet_wrap(~Organ, scales = "free")

ggplot(rd_org, aes(x = Treatment, y = RD)) + 
  geom_jitter(aes(color = Treatment, fill = Treatment)) + 
  theme_bw() + 
  facet_grid(Organ~Route, scales = "free") + 
  scale_color_manual(values = treatpalette)


## @knitr biod_pointsbytreatment
ggplot(rd_org, aes(x = Treatment, y = RD)) + 
  geom_jitter(aes(color = Treatment), width = 0.2) + 
  facet_grid(Organ~Route) +#, scales = "free") + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  scale_y_continuous(name = expression(''^153*Gd~'Content (% RD)')) +
  scale_color_manual(values = treatpalette)
  


## @knitr biod_pointsbyorgan
ggplot(rd_org, aes(x = Organ, y = RD)) + 
  geom_jitter(aes(color = Treatment), width = 0.2) + 
  facet_grid(Treatment~Route) +#, scales = "free") + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  scale_y_continuous(name = expression(''^153*Gd~'Content (% RD)')) +
  scale_color_manual(values = treatpalette)



## @knitr biod_points_selectedorgans
ggplot(rd_org_select, aes(x = Treatment, y = RD)) + 
  geom_jitter(aes(color = Treatment), width = 0.1) + 
  facet_grid(Organ~Route, scales = "free") + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  scale_y_continuous(name = expression(''^153*Gd~'Content (% RD)')) +
  scale_color_manual(values = treatpalette)

## @knitr biod_boxes_byorgan
ggplot(rd_org_select, aes(x = Organ, y = RD)) + 
  geom_boxplot(aes(fill = Route, color = Route), 
               position = position_dodge2(preserve = "total")) + 
  scale_fill_manual(values = c('#d95f02', '#1b9e77')) +
  scale_color_manual(values = c('#d95f02', '#1b9e77')) +
  scale_y_continuous(name = expression(''^153*Gd~'Content (% RD)')) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  facet_grid(rows = vars(Treatment))

## @knitr biod_boxes_bytreatment
ggplot(rd_org_select, aes(x = Treatment, y = RD)) + 
  geom_boxplot(aes(fill = Route, color = Route), 
               position = position_dodge2(preserve = "total")) + 
  scale_fill_manual(values = c('#d95f02', '#1b9e77')) +
  scale_color_manual(values = c('#d95f02', '#1b9e77')) +
  scale_y_continuous(name = expression(''^153*Gd~'Content (% RD)')) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  facet_grid(rows = vars(Organ))
## @knitr end

ggplot(rd_org, aes(x = Group, y = RD)) + 
  geom_jitter(aes(color = Group)) + 
  theme_bw() + 
  facet_wrap(~Organ) + 
  ylim(0,max(rd_org$RD))

ggplot(mexcreta, aes(x = Day, y = RD)) + 
  geom_col(aes(fill = Treatment), position = 'dodge') +
  theme_bw() + 
  facet_grid(Type~Route)

ggplot(mexcreta, aes(x = Day, y = RD)) + 
  geom_col(aes(fill = Route), position = 'dodge') +
  theme_bw() + 
  facet_grid(Type~Treatment)

ggplot(mexcreta, aes(x = Day, y = RD)) + 
  geom_col(aes(fill = Type), position = 'dodge') +
  theme_bw() + 
  facet_grid(Treatment~Route)

ggplot(mexcreta, aes(x = Day, y = RD)) + 
  geom_col(aes(fill = Type), position = 'dodge') +
  theme_bw() + 
  ylim(0,5) +
  facet_grid(Treatment~Route)




ggplot(mexcreta, aes(x = Day, y = RD)) + 
  geom_point(aes(color = Type), position = 'jitter') +
  theme_bw() + 
  facet_grid(Treatment~Route) 

ggplot(mexcreta, aes(x = Day, y = RD)) + 
  geom_col(aes(fill = Type), position = 'dodge') +
  theme_bw() + 
  facet_wrap(~Group)

ggplot(mexcreta, aes(x = Treatment, y = RD)) + 
  geom_col(aes(fill = Day)) + 
  scale_y_continuous(limits = c(0,105), expand = c(0,0)) + 
  geom_hline(yintercept = 100) + 
  scale_fill_manual(values = dayspalette) +
  facet_wrap(Route~Type)

## @knitr excreta_by_day
ggplot(mexcreta, aes(x = Treatment, y = RD)) + 
  geom_col(aes(fill = Day)) + 
  scale_y_continuous(limits = c(0,105), expand = c(0,0), name = expression(''^153*Gd~'Content (% RD)')) + 
  geom_hline(yintercept = 100, linetype = 2) + 
  scale_fill_manual(values = dayspalette) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  facet_wrap(Type~Route)

## @knitr total_excreta
ggplot(mexcreta, aes(x = Treatment, y = RD)) + 
  geom_col(aes(fill = Type)) + 
  facet_wrap(~Route) + 
  scale_y_continuous(limits = c(0,105), expand = c(0,0), name = expression(''^153*Gd~'Content (% RD)')) + 
  geom_hline(yintercept = 100, linetype = 2) + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  scale_fill_manual(values = c('chartreuse3','darkorchid2'))
  

## @knitr statistics
fit <- aov(RD ~ Treatment, data = rd_org[which(rd_org$Organ == 'Skeleton' & rd_org$Route == 'IP'),])
summary(glht(fit, linfct=mcp(Treatment="Dunnett")))
