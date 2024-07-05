#Plotting output of ecosystem engineer dominance

library(divDyn)
data("stages", package="divDyn")
period.cols <- unique(stages[4:94,]$systemCol)

library(ggplot2)
library(egg)

paleo_stages <- stages$stage[4:51]
paleo_mids <- stages$mid[4:51]

#=================#
setwd("~/Desktop/Manucripts/Palass_ecosystemengineering")

#=== load data ===#
load('Output/infauna_dominance_04-07-2024.RData')

#add periods
EEdominance <- infaunadom_df
for(i in 1:nrow(EEdominance)){
  
  this.stage <- EEdominance$stage[i]
  this.period <- stages[which(stages$stage==this.stage),'system']
  EEdominance$period[i] <- this.period
  
}

EEdominance$period <- factor(EEdominance$period, levels=unique(stages[4:94,'system']))
colnames(EEdominance)[12] <- 'feeding mode'
EEdominance$`feeding mode` <- factor(EEdominance$`feeding mode`, 
                                             levels=c('suspension feeder',
                                                      'deposit feeder',
                                                      'mixed deposit/suspension feeder',
                                                      'grazer'))
feeding.cols <-   c('#30638e', #suspension feeder
                     '#e07a5f', #deposit feeder
                     '#f2cc8f', #mixed
                     '#81b29a') #grazer

community_dominance <- ggplot(data=EEdominance) +
  geom_errorbar(aes(x=mid_ma, ymin=EEoccs_prop-EEoccs_sd, ymax=EEoccs_prop+EEoccs_sd, color=`feeding mode`)) + 
  geom_line(aes(y=EEoccs_prop, x=mid_ma), color='black', linewidth=0.5) +
  geom_point(aes(y=EEoccs_prop, x=mid_ma, fill=`feeding mode`), color='black', pch=23, size=3.5) +
  scale_color_manual(values=feeding.cols) +
  scale_fill_manual(values=feeding.cols) +
  ylim(c(0,1.01)) +
  scale_x_reverse(limits=c(541,0)) +
  ylab('Proportion of taxa that are ecosystem engineers') +
  ggtitle('Bioturbating taxa within their environments') +
  coord_geo(pos='bottom', dat='periods', size='auto', abbrv=FALSE, height=unit(1,'line')) +
  theme_classic() +
  theme(panel.border=element_rect(color='black', fill=NA, size=1),
        plot.title=element_text(hjust=0.5, face='bold'),
        axis.title.x=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8))
community_dominance 


formation_dominance <- ggplot(data=EEdominance) +
  geom_errorbar(aes(x=mid_ma, ymin=prop_EE_formations-forms_sd, ymax=prop_EE_formations+forms_sd, color=period)) + 
  geom_line(aes(y=prop_EE_formations, x=mid_ma), color='black', linewidth=0.5) +
  geom_point(aes(y=prop_EE_formations, x=mid_ma, fill=period), color='black', pch=21, size=3.5) +
  scale_color_manual(values=period.cols) +
  scale_fill_manual(values=period.cols) +
  ylim(c(0,1.01)) +
  scale_x_reverse(limits=c(541,0), 'Time (Mya)') +
  ylab('Proportion of formations containing ecosystem engineers') +
  ggtitle('Bioturbated environments') +
  coord_geo(pos='bottom', dat='periods', size='auto', abbrv=FALSE, height=unit(1,'line')) +
  theme_classic() +
  theme(panel.border=element_rect(color='black', fill=NA, size=1),
        plot.title=element_text(hjust=0.5, face='bold'),
        legend.position='none')
formation_dominance

bioturbator_dominance <- ggarrange(community_dominance, formation_dominance, ncol=1)
ggsave(filename='Output/Figures/Revision1/Figure1_Bioturbator_Dominance.pdf', bioturbator_dominance,
       height=8, width=10)
