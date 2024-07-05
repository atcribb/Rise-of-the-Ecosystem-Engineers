#Plotting output for relative abundance of reef-building ecosystem engineer groups

library(divDyn)
data('stages', package='divDyn')
colors <- unique(stages$systemCol)[2:7]

library(ggplot2)
library(egg)

paleo_stages <- stages$stage[4:51]
paleo_mids <- stages$mid[4:51]

library(deeptime)
library(tidyr)

#=================#
setwd("~/Desktop/Manucripts/Palass_ecosystemengineering")

#=== load data ===#
load('Output/reef_relativeabundance_04-07-2024.RData')

reef_relabund_df <- as.data.frame(pivot_longer(reef_relabund, cols=3:ncol(reef_relabund), 
                                               names_to='reef_builder', values_to='proportion'))
table(reef_relabund_df$reef_builder)

head(reef_relabund_df)

reef_relabund_df$`reef builder` <- rep(NA, nrow(reef_relabund_df))
reef_relabund_df$`reef builder`[which(reef_relabund_df$reef_builder=='Glass_sponges')] <- 'Glass sponges'
reef_relabund_df$`reef builder`[which(reef_relabund_df$reef_builder=='Archaeocyaths')] <- 'Archaeocyathids'
reef_relabund_df$`reef builder`[which(reef_relabund_df$reef_builder=='Stromatoporoids')] <- 'Stromatoporoids'
reef_relabund_df$`reef builder`[which(reef_relabund_df$reef_builder=='Hydrozoans')] <- 'Hydrozoans'
reef_relabund_df$`reef builder`[which(reef_relabund_df$reef_builder=='Rudist_bivalves')] <- 'Rudist bivalves'
reef_relabund_df$`reef builder`[which(reef_relabund_df$reef_builder=='Tube_worms')] <- 'Tube worms'
reef_relabund_df$`reef builder`[which(reef_relabund_df$reef_builder=='Rugose_corals')] <- 'Rugose corals'
reef_relabund_df$`reef builder`[which(reef_relabund_df$reef_builder=='Tabulate_corals')] <- 'Tabulate corals'
reef_relabund_df$`reef builder`[which(reef_relabund_df$reef_builder=='Stony_corals')] <- 'Stony corals'
reef_relabund_df$`reef builder`[which(reef_relabund_df$reef_builder=='Chaetetids')] <- 'Chaetetids'
table(reef_relabund_df$`reef builder`)

reef_relabund_df$`reef builder` <- factor(reef_relabund_df$`reef builder`, levels=c(
  'Archaeocyathids',
  'Glass sponges', 
  'Stromatoporoids', 
  'Chaetetids', 
  'Rudist bivalves', 
  'Hydrozoans',
  'Tube worms', 
  'Rugose corals', 
  'Tabulate corals',
  'Stony corals')) 


reef_builder.relabundcolors <- c('#6e1423',
                                 '#b21e35',
                                 '#e01e37',
                                 '#b5838d',
                                 '#b084cc',
                                 'goldenrod2',
                                 '#548c2f',
                                 '#01497c',
                                 '#2c7da0',
                                 '#a9d6e5')



relabund_builders <- ggplot(data=reef_relabund_df) +
  geom_area(aes(x=mid_ma, y=proportion, fill=`reef builder`)) +
  geom_rect(aes(xmin=492.6, xmax=488.92500, ymin=0, ymax=1), fill='gray95') +
  scale_fill_manual(values=reef_builder.relabundcolors) +
  scale_x_reverse() +
  coord_geo(pos='bottom', dat='periods', size='auto', abbrv=FALSE, height=unit(1,'line')) +
  theme_classic() +
  ylab('Relative Abundance') +
  xlab('Time (Mya)') +
  ggtitle('Relative abundance of reef-builder groups') +
  theme(plot.title=element_text(hjust=0.5, face='bold'),
        panel.border=element_rect(colour='black', fill=NA, size=1))
relabund_builders

ggsave(filename='Output/Figures/Revision1/Figure5_reefs_relativeabundance.pdf', plot=relabund_builders,
       width=13, height=4)
