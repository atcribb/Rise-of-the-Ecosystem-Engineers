#Author: Dr. Alison T Cribb, University of Southampton

library(divDyn)
data("stages", package="divDyn")
period.cols <- unique(stages[4:94,]$systemCol)

library(ggplot2)
library(egg)

paleo_stages <- stages$stage[4:51]
paleo_mids <- stages$mid[4:51]

#=== load data ===#
load('Output/reef_dominance.RData')

#add periods
EEdominance <- reefdom_df
for(i in 1:nrow(EEdominance)){
  
  this.stage <- EEdominance$stage[i]
  this.period <- stages[which(stages$stage==this.stage),'system']
  EEdominance$period[i] <- this.period
  
}

EEdominance$period <- factor(EEdominance$period, levels=unique(stages[4:94,'system']))
colnames(EEdominance)[11] <- 'reef builder'
EEdominance$`reef builder`[which(EEdominance$`reef builder`=='Glass_sponges')] <- 'Glass sponges'
EEdominance$`reef builder`[which(EEdominance$`reef builder`=='Archaeocyaths')] <- 'Archaeocyathids'
EEdominance$`reef builder`[which(EEdominance$`reef builder`=='Stromatoporoids')] <- 'Stromatoporoids'
EEdominance$`reef builder`[which(EEdominance$`reef builder`=='Hydrozoans')] <- 'Hydrozoans'
EEdominance$`reef builder`[which(EEdominance$`reef builder`=='Rudist_bivalves')] <- 'Rudist bivalves'
EEdominance$`reef builder`[which(EEdominance$`reef builder`=='Tube_worms')] <- 'Tube worms'
EEdominance$`reef builder`[which(EEdominance$`reef builder`=='Rugose_corals')] <- 'Rugose corals'
EEdominance$`reef builder`[which(EEdominance$`reef builder`=='Tabulate_corals')] <- 'Tabulate corals'
EEdominance$`reef builder`[which(EEdominance$`reef builder`=='Stony_corals')] <- 'Stony corals'
EEdominance$`reef builder`[which(EEdominance$`reef builder`=='Chaetetids')] <- 'Chaetetids'
table(EEdominance$`reef builder`)


EEdominance$`reef builder` <- factor(EEdominance$`reef builder`,
                                     levels=c('Archaeocyathids',
                                              'Glass sponges',
                                              'Stromatoporoids',
                                              'Rudist bivalves',
                                              'Hydrozoans', 
                                              'Tube worms',
                                              'Rugose corals',
                                              'Tabulate corals',
                                              'Stony corals'))
reef.colors <- c('#6e1423', #archaeocyathids
                 '#b21e35', #glass sponges 
                 '#e01e37', #stromatoporoids
                 '#b084cc', #rudist bivalves
                 'goldenrod2', #hydrozoa
                 '#548c2f', #tube worms
                 '#01497c', #rugose
                 'skyblue3', #tabualte
                 '#caf0f8') #scleractinia 

community_dominance <- ggplot(data=EEdominance) +
  geom_errorbar(aes(x=mid_ma, ymin=EEoccs_prop-EEoccs_sd, ymax=EEoccs_prop+EEoccs_sd, color=`reef builder`)) + 
  geom_line(aes(y=EEoccs_prop, x=mid_ma), color='black', linewidth=0.5) +
  geom_point(aes(y=EEoccs_prop, x=mid_ma, fill=`reef builder`), color='black', pch=23, size=2.5) +
  scale_color_manual(values=reef.colors) +
  scale_fill_manual(values=reef.colors) +
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

reef_dominance <- ggarrange(formation_dominance, community_dominance, ncol=1)

#==== constrained within environments =====#
load('Output/within_environments/reef_dominance_shallow.RData')
load('Output/within_environments/reef_dominance_deep.RData')
load('Output/within_environments/reef_dominance_siliciclastics.RData')
load('Output/within_environments/reef_dominance_carbonates.RData')
load('Output/within_environments/reef_dominance_shallowsiliciclastics.RData')
load('Output/within_environments/reef_dominance_shallowcarbonates.RData')
load('Output/within_environments/reef_dominance_deepsiliciclastics.RData')
load('Output/within_environments/reef_dominance_deepcarbonates.RData')
load('Output/reef_dominance.RData')
reefdom_df$environment <- 'all environments'
reefdom_df$period <- rep(NA, nrow(reefdom_df))
for(i in 1:nrow(reefdom_df)){
  
  this.stage <- reefdom_df$stage[i]
  this.period <- stages[which(stages$stage==this.stage),'system']
  reefdom_df$period[i] <- this.period
  
}

environment_compare <- rbind(reefdom_df, reefdom_shallow_df, reefdom_deep_df,
                             reefdom_siliciclastic_df, reefdom_carbs_df,
                             reefdom_shallow_siliciclastic_df, reefdom_shallow_carb_df,
                             reefdom_deep_siliciclastic_df, reefdom_deep_carb_df)
environment_compare$environment <- factor(environment_compare$environment, levels=
                                            c('all environments', 'shallow', 'deep',
                                              'siliciclastic',
                                              'shallow siliciclastics', 'deep siliciclastics',
                                              'carbonates',
                                              'shallow carbonates', 'deep carbonates'))
env.cols <- c('#303030',
              '#B8D7D3', '#327970',
              '#EA698B','#E899AE', '#6F2F40',
              '#422CA0','#9181DC', '#1F1549')

#====formation dominance =====#
facet_environments <- ggplot(environment_compare,
                             aes(x=mid_ma, y=prop_EE_formations, fill=environment)) +
  geom_errorbar(aes(x=mid_ma, ymin=prop_EE_formations-forms_sd, ymax=prop_EE_formations+forms_sd, col=environment), width=3) +
  geom_line(aes(color=environment)) +
  geom_point(shape=21, size=2) +
  #annotate(geom='line', x=reefdom_df$mid_ma, y=reefdom_df$prop_EE_formations, col='#303030#', lwd=1) +
  facet_wrap(~environment) +
  annotate(geom='line',
           x=reefdom_df$mid_ma, y=reefdom_df$prop_EE_formations,
           linewidth=1, col='#303030') +
  scale_color_manual(values=env.cols) +
  scale_fill_manual(values=env.cols) +
  ylim(c(0,1.01)) +
  scale_x_reverse(limits=c(541,0), 'Time (Mya)') +
  ylab('Proportion of formations containing ecosystem engineers') +
  ggtitle('Reef environments') +
  coord_geo(pos='bottom', dat='periods', size=2.5, abbrv=TRUE, height=unit(1,'line')) +
  theme_classic() +
  theme(panel.border=element_rect(color='black', fill=NA, size=1),
        plot.title=element_text(hjust=0.5, face='bold'),
        legend.position='none'
  )
facet_environments

#===== within environment dominance ====#
facet_within <- ggplot(environment_compare,
                       aes(x=mid_ma, y=EEoccs_prop, fill=environment)) +
  geom_errorbar(aes(x=mid_ma, ymin=EEoccs_prop-EEoccs_sd, ymax=EEoccs_prop+EEoccs_sd, col=environment), width=3) +
  geom_line(aes(color=environment)) +
  geom_point(shape=21, size=2) +
  facet_wrap(~environment) +
  annotate(geom='line',
           x=reefdom_df$mid_ma, y=reefdom_df$EEoccs_prop,
           linewidth=1, col='#303030') +
  scale_color_manual(values=env.cols) +
  scale_fill_manual(values=env.cols) +
  ylim(c(0,1.01)) +
  scale_x_reverse(limits=c(541,0), 'Time (Mya)') +
  ylab('Proportion of taxa that are ecosystem engineers') +
  ggtitle('Dominance of reef-builders within their palaeoenvironments') +
  coord_geo(pos='bottom', dat='periods', size=2.5, abbrv=TRUE, height=unit(1,'line')) +
  theme_classic() +
  theme(panel.border=element_rect(color='black', fill=NA, size=1),
        plot.title=element_text(hjust=0.5, face='bold'),
        legend.position='none'
  )
facet_within

#===== proportion of reef environments (Supplementary Figure 3) =====#
load('Data/Phanerozoic_clean_final.RData')
data("keys", package="divDyn")
all_data$lith <- categorize(all_data$lithology1, keys$lith)
all_data$bath <- categorize(all_data$environment, keys$bath)
all_data$reef <- categorize(all_data$environment, keys$reef)
all_data$reef[all_data$lith=="clastic" & all_data$environment=="marine indet."] <- "nonreef"

phanero_stages <- stages$stage[4:95]
phanero_mids <- stages$mid[4:95]
phanero_dur <- stages$dur[4:95]
variables <- c('stage', 'mid_ma', 'dur', 'n_reefs', 'prop_reefs', 'n_nonreefs', 'prop_nonreefs', 'n_unknown', 'prop_unknown')
reef_trends <- as.data.frame(matrix(NA, nrow=length(phanero_stages), ncol=length(variables)))
colnames(reef_trends) <- variables
reef_trends$stage <- phanero_stages
reef_trends$mid_ma <- phanero_mids
reef_trends$dur <- phanero_dur
reef_trends

for(i in 1:nrow(reef_trends)){
  
  this.stage <- reef_trends$stage[i]
  this.stage.data <- subset(all_data, stage==this.stage)
  reef_data <- subset(this.stage.data, reef=='reef')
  nonreef_data <- subset(this.stage.data, reef=='non-reef')
  unknown_data <- subset(this.stage.data, reef=='unknown')
  
  reef_formations <- unique(reef_data$formation)
  nonreef_formations <- unique(nonreef_data$formation)
  unknown_formations <- unique(unknown_data$formation)
  
  tot_formations <- length(reef_formations)+length(nonreef_formations)+length(unknown_formations)
  
  reef_trends$n_reefs[i] <- length(reef_formations)
  reef_trends$prop_reefs[i] <- length(reef_formations)/tot_formations
  
  reef_trends$n_nonreefs[i] <- length(nonreef_formations)
  reef_trends$prop_nonreefs[i] <- length(nonreef_formations)/tot_formations
  
  reef_trends$n_unknown[i] <- length(unknown_formations)
  reef_trends$prop_unknown[i] <- length(unknown_formations)/tot_formations 
  
  
}

View(reef_trends)

reef.cols <- c('#A5A5A5', '#9FB2E7', '#2E3FBF')

reef_trends_prop <- pivot_longer(reef_trends, cols=c(5,7,9),
                                 names_to='environment', values_to='prop')
reef_trends_prop$environment <- factor(reef_trends_prop$environment,
                                       levels=c("prop_unknown", "prop_nonreefs", "prop_reefs"))

prop_reefs <- ggplot(reef_trends_prop) +
  geom_bar(aes(x=mid_ma, y=prop, fill=environment, width=dur), stat='identity', position='stack') +
  scale_fill_manual(values=reef.cols, name="environment:", labels=c("unknown", "non-reef", "reef")) +
  scale_y_continuous('Relative abundance of environment') +
  scale_x_reverse(limits=c(541,0), 'Time (mya)') +
  annotate(geom="text", label="B", x=525, y=0.95) +
  coord_geo(pos='bottom', dat='periods', size=4, abbrv=TRUE, height=unit(1,'line')) +
  theme_classic() +
  theme(
    plot.title = element_text(face='bold'),
    panel.border=element_rect(fill=NA, colour='black', linewidth=1),
    legend.position='bottom',
    legend.title=element_text(size=10),
    legend.text=element_text(size=10)
  )

reef_trends_n <- pivot_longer(reef_trends, cols=c(4,6,8),
                              names_to='environment', values_to='n')
reef_trends_n$environment <- factor(reef_trends_n$environment,
                                    levels=c("n_unknown", "n_nonreefs", "n_reefs"))

n_reefs <- ggplot(reef_trends_n) +
  geom_bar(aes(x=mid_ma, y=n, fill=environment, width=dur), stat='identity', position='stack') +
  scale_fill_manual(values=reef.cols, name="environment", labels=c("unknown", "non-reef", "reef")) +
  scale_y_continuous('Number of formations') +
  scale_x_reverse(limits=c(541,0), 'Time (mya)') +
  annotate(geom="text", label="A", x=525, y=470) +
  coord_geo(pos='bottom', dat='periods', size=4, abbrv=TRUE, height=unit(1,'line')) +
  theme_classic() +
  theme(
    plot.title = element_text(face='bold'),
    panel.border=element_rect(fill=NA, colour='black', linewidth=1),
    legend.position='none'
  )

