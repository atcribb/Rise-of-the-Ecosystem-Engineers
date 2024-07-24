#Plotting output for relative abundance of taxa and feeding modes for bioturbating ecosystem engineers

library(divDyn)
data('stages', package='divDyn')
colors <- unique(stages$systemCol)[2:7]

library(ggplot2)
library(egg)

paleo_stages <- stages$stage[4:51]
paleo_mids <- stages$mid[4:51]

library(deeptime)

library(tidyr)

#======== feeding modes ==========#
#=== load data ===#
#proportion of genera that are ecosystem engineers in each formation
load("Output/bioturbator_feeding_relativeabundance.RData")

feeding_relabund_df <- as.data.frame(pivot_longer(infauna_feeding_relabund, cols=3:ncol(infauna_feeding_relabund),
                                                  names_to='feeding mode', values_to='proportion'))
unique(feeding_relabund_df$`feeding mode`)
feeding_relabund_df$`feeding mode` <- factor(feeding_relabund_df$`feeding mode`, 
                                             levels=c('suspension feeder',
                                                      'deposit feeder',
                                                      'mixed deposit/suspension feeder',
                                                      'grazer',
                                                      'predator',
                                                      'other'))
feeding.ra.cols <- c('#30638e', #suspension feeder
                     '#e07a5f', #deposit feeder
                     '#f2cc8f', #mixed
                     '#81b29a', #grazer
                     '#6d597a', #predator
                     '#c2c5aa') #other

relabund_feeding <- ggplot(data=feeding_relabund_df) +
  geom_area(aes(x=mid_ma, y=proportion, fill=`feeding mode`)) +
  scale_fill_manual(values=feeding.ra.cols) +
  scale_x_reverse() +
  coord_geo(pos='bottom', dat='periods', size='auto', abbrv=FALSE, height=unit(1,'line')) +
  theme_classic() +
  ylab('Relative Abundance') +
  xlab('Time (Mya)') +
  ggtitle('Bioturbating ecosystem engineer feeding modes') +
  theme(plot.title=element_text(hjust=0.5, face='bold'),
        axis.title.x=element_blank(),
        panel.border=element_rect(colour='black', fill=NA, size=1))
relabund_feeding

#======== taxonomic groups =========#
load("Output/bioturbator_taxa_relativeabundance.RData")

taxa_relabund_df <- as.data.frame(pivot_longer(infauna_taxa_relabund, cols=3:ncol(infauna_taxa_relabund),
                                               names_to='phylum', values_to='proportion'))
unique(taxa_relabund_df$phylum)

infauna.relabundcolors      <- c('goldenrod2',
                                 '#b5838d',
                                 '#548c2f',
                                 '#b21e35',
                                 '#01497c')
relabund_infauna <- ggplot(data=taxa_relabund_df) +
  geom_area(aes(x=mid_ma, y=proportion, fill=phylum)) +
  scale_fill_manual(values=infauna.relabundcolors) +
  scale_x_reverse() +
  coord_geo(pos='bottom', dat='periods', size='auto', abbrv=FALSE, height=unit(1,'line')) +
  theme_classic() +
  ylab('Relative Abundance') +
  xlab('Time (Mya)') +
  ggtitle('Bioturbating ecosystem engineer groups') +
  theme(plot.title=element_text(hjust=0.5, face='bold'),
        axis.title.x=element_blank(),
        panel.border=element_rect(colour='black', fill=NA, size=1))
relabund_infauna