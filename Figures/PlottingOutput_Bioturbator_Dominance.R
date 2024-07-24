#Plotting output of ecosystem engineer dominance

library(divDyn)
data("stages", package="divDyn")
period.cols <- unique(stages[4:94,]$systemCol)

library(ggplot2)
library(egg)

paleo_stages <- stages$stage[4:51]
paleo_mids <- stages$mid[4:51]

#=== load data ===#
load('Output/bioturbator_dominance.RData')

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
  geom_point(aes(y=EEoccs_prop, x=mid_ma, fill=`feeding mode`), color='black', pch=23, size=2.5) +
  scale_color_manual(values=feeding.cols) +
  scale_fill_manual(values=feeding.cols) +
  ylim(c(0,1.01)) +
  scale_x_reverse(limits=c(541,0), 'Time (Mya)') +
  ylab('Proportion of fossil occurrences') +
  coord_geo(pos='bottom', dat='periods', size=4, abbrv=TRUE, height=unit(1,'line')) +
  theme_classic() +
  annotate(geom="text", label="B", x=525, y=0.95) +
  theme(panel.border=element_rect(color='black', fill=NA, size=1),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        legend.text=element_text(size=8),
        legend.title=element_blank(),
        legend.position='bottom')
community_dominance 


formation_dominance <- ggplot(data=EEdominance) +
  geom_errorbar(aes(x=mid_ma, ymin=prop_EE_formations-forms_sd, ymax=prop_EE_formations+forms_sd, color=period)) + 
  geom_line(aes(y=prop_EE_formations, x=mid_ma), color='black', linewidth=0.5) +
  geom_point(aes(y=prop_EE_formations, x=mid_ma, fill=period), color='black', pch=21, size=2.5) +
  scale_color_manual(values=period.cols) +
  scale_fill_manual(values=period.cols) +
  ylim(c(0,1.01)) +
  scale_x_reverse(limits=c(541,0), 'Time (Mya)') +
  ylab('Proportion of formations') +
  annotate(geom="text", label="A", x=525, y=0.95) +
  coord_geo(pos='bottom', dat='periods', size=4, abbrv=TRUE, height=unit(1,'line')) +
  theme_classic() +
  theme(panel.border=element_rect(color='black', fill=NA, size=1),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        legend.position='none')
formation_dominance

bioturbator_dominance <- ggarrange(formation_dominance, community_dominance, ncol=1)

#====== constrained within environmnents =====#
load('Output/within_environments/bioturbator_dominance_shallow.RData')
load('Output/within_environments/bioturbator_dominance_deep.RData')
load('Output/within_environments/bioturbator_dominance_siliciclastics.RData')
load('Output/within_environments/bioturbator_dominance_carbonates.RData')
load('Output/within_environments/bioturbator_dominance_shallowsiliciclastics.RData')
load('Output/within_environments/bioturbator_dominance_shallowcarbonates.RData')
load('Output/within_environments/bioturbator_dominance_deepsiliciclastics.RData')
load('Output/within_environments/bioturbator_dominance_deepcarbonates.RData')
load('Output/bioturbator_dominance.RData')
infaunadom_df$environment <- 'all environments'
infaunadom_df$period <- rep(NA, nrow(infaunadom_df))
for(i in 1:nrow(infaunadom_df)){
  
  this.stage <- infaunadom_df$stage[i]
  this.period <- stages[which(stages$stage==this.stage),'system']
  infaunadom_df$period[i] <- this.period
  
}

environment_compare <- rbind(infaunadom_df, infaunadom_shallow_df, infaundadom_deep_df,
                             infaunadom_siliciclastic_df, infaunadom_shallow_siliciclastic_df, infaunadom_deep_siliciclastic_df,
                             infaunadom_carbs_df, infaunadom_shallow_carb_df, infaunadom_deep_carb_df)
environment_compare$environment <- factor(environment_compare$environment, levels=
                                            c('all environments', 'shallow', 'deep',
                                              'siliciclastic',
                                              'shallow siliciclastic', 'deep siliciclastic',
                                              'carbonates',
                                              'shallow carbonates', 'deep carbonates'))
env.cols <- c('#303030',
              '#B8D7D3', '#327970',
              '#EA698B','#E899AE', '#6F2F40',
              '#422CA0','#9181DC', '#1F1549')


facet_environments <- ggplot(environment_compare,
                             aes(x=mid_ma, y=prop_EE_formations, fill=environment)) +
  geom_errorbar(aes(x=mid_ma, ymin=prop_EE_formations-forms_sd, ymax=prop_EE_formations+forms_sd, col=environment), width=3) +
  geom_line(aes(color=environment)) +
  geom_point(shape=21, size=2) +
  facet_wrap(~environment) +
  annotate(geom='line',
           x=infaunadom_df$mid_ma, y=infaunadom_df$prop_EE_formations,
           linewidth=1, col='#303030') +
  scale_color_manual(values=env.cols) +
  scale_fill_manual(values=env.cols) +
  ylim(c(0,1.01)) +
  scale_x_reverse(limits=c(541,0), 'Time (Mya)') +
  ylab('Proportion of formations containing ecosystem engineers') +
  #ggtitle('Bioturbated environments') +
  coord_geo(pos='bottom', dat='periods', size=2.5, abbrv=TRUE, height=unit(1,'line')) +
  theme_classic() +
  theme(panel.border=element_rect(color='black', fill=NA, size=1),
        plot.title=element_text(hjust=0.5, face='bold'),
        legend.position='none'
  )
facet_environments

#====== within environment dominance =====#
facet_within <- ggplot(environment_compare,
                       aes(x=mid_ma, y=EEoccs_prop, fill=environment)) +
  geom_errorbar(aes(x=mid_ma, ymin=EEoccs_prop-EEoccs_sd, ymax=EEoccs_prop+EEoccs_sd, col=environment), width=3) +
  geom_line(aes(color=environment)) +
  geom_point(shape=21, size=2) +
  facet_wrap(~environment) +
  annotate(geom='line',
           x=infaunadom_df$mid_ma, y=infaunadom_df$EEoccs_prop,
           linewidth=1, col='#303030') +
  scale_color_manual(values=env.cols) +
  scale_fill_manual(values=env.cols) +
  ylim(c(0,1.01)) +
  scale_x_reverse(limits=c(541,0), 'Time (Mya)') +
  ylab('Proportion of taxa that are ecosystem engineers') +
  coord_geo(pos='bottom', dat='periods', size=2.5, abbrv=TRUE, height=unit(1,'line')) +
  theme_classic() +
  theme(panel.border=element_rect(color='black', fill=NA, size=1),
        plot.title=element_text(hjust=0.5, face='bold'),
        legend.position='none'
  )
facet_within
