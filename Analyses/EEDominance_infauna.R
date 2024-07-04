#Author: Dr. Alison T. Cribb, University of Southampton 
#Created: 4 Deceomber 2023
#Last edited: 4 July 2024

#Summary: Dominance of bioturbatored environments/environments with infauna, or "how common are infauna-engineered environments?"
#         First: Proportion of environments (formations) that contain infauna, per stage
#         Second: In infauna engineneered formations, what proportion of occurrences are infauna?
#         Third: What is the dominant infauna class and infauna feeding behaviour in each stage?

library(divDyn)
data(stages)

#===================================================================#
#===================================================================#
#setwd("~/Desktop/Manucripts/Palass_ecosystemengineering")

#=== load data ===#
#load('Data/Phanerozoic_clean_final.RData')
load('Phanerozoic_clean_final.RData')
all_data <- subset(all_data, !is.na(formation))
phanero_stages <- stages$stage[4:95]
phanero_mids <- stages$mid[4:95]

#=== find fossil assemblages with and without ecosystem engineers ===#
table(all_data$life_habit) #use life_habit to find infauna 
infaunal_lifehabits <- c('deep infaunal', 'infaunal', 'semi-infaunal', 'semi-infaunal, solitary', 'shallow-infaunal',
                         'infaunal, depth=deep', 'semi-infaunal, gregarious', 'semi-infaunal, solitary',
                         'shallow infaunal')
infauna_data <- subset(all_data, life_habit %in% infaunal_lifehabits)
infauna_EE_genera <- unique(infauna_data$genus)
infauna_formations <- unique(infauna_data$formation)

#=== and also add epifaunal sediment bulldozers  ===#
epifaunal_data <- subset(all_data, life_habit=='epifaunal')
epifaunal_data_mobile <- subset(epifaunal_data, motility=='actively mobile') #conservatively, just the actively mobile epifauna
EE_epifauna_diets <- c('detritivore, grazer', 'grazer', 'grazer, deposit feeder') #conservative guesses for what's eating the top surface of the seafloor
epifauna_EE_data <- subset(epifaunal_data_mobile, diet %in% EE_epifauna_diets)
epifauna_EE_genera <- unique(epifauna_EE_data$genus)
epifauna_EE_formations <- unique(epifauna_EE_data$formation)

#=== combine for final list of benthic->infauna ecosystem engineer genera and formations ===# 
ecoeng_genera <- unique(c(infauna_EE_genera, epifauna_EE_genera))
ecoeng_formations <- unique(c(infauna_formations, epifauna_EE_formations))
infauna_data <- subset(all_data, genus %in% ecoeng_genera)
table(infauna_data$phylum) #some of these we can tell shouldn't be included
keep_phyla <- c('Annelida', 'Arthropoda', 'Brachiopoda', 'Echinodermata', 'Mollusca')
infauna_data <- subset(infauna_data, phylum %in% keep_phyla)
 
#=== assign broader feeding categories based on infauna_data$diet ===#
table(infauna_data$diet)
suspension_feeder_cats <- c('chemosymbiotic, suspension feeder',
                            'detritivore, suspension feeder',
                            'suspension feeder, chemosymbiotic',
                            'suspension feeder',
                            'suspension feeder, detritivore')
deposit_feeder_cats <- c('deposit feeder',
                         'deposit feeder, chemosymbiotic',
                         'deposit feeder, detritivore')
grazer_feeder_cats <- c('detritivore', 'detritivore, grazer', 'grazer, deposit feeder', 'grazer')
predator_feeder_cats <- c('carnivore', 'omnivore')
mixed_feeder_cats <- c('deposit feeder, suspension feeder')
other_cats <- c(NA, 'chemosymbiotic', 'photosymbiotic')

infauna_data$feeding <- rep(NA, nrow(infauna_data))
infauna_data$feeding[which(infauna_data$diet %in% suspension_feeder_cats)] <- 'suspension feeder'
infauna_data$feeding[which(infauna_data$diet %in% deposit_feeder_cats)] <- 'deposit feeder'
infauna_data$feeding[which(infauna_data$diet %in% mixed_feeder_cats)] <- 'mixed deposit/suspension feeder'
infauna_data$feeding[which(infauna_data$diet %in% grazer_feeder_cats)] <- 'grazer'
infauna_data$feeding[which(infauna_data$diet %in% predator_feeder_cats)] <- 'predator'
infauna_data$feeding[which(infauna_data$diet %in% other_cats)] <- 'other'
table(infauna_data$feeding)

#=== set up output ===#
variables <- c('stage', 'mid_ma', 
               'n_tot_formations', 'n_EE_formations', 'prop_EE_formations', 'forms_sd', 
               'n_occs_tot', 'n_occs_EEs', 'EEoccs_prop', 'EEoccs_sd', 
               'main_class', 'main_feeding')
infaunadom_df <- as.data.frame(matrix(nrow=length(phanero_stages), ncol=length(variables)))
colnames(infaunadom_df) <- variables
infaunadom_df$stage <- phanero_stages
infaunadom_df$mid_ma <- phanero_mids
infaunadom_df

iter <- 1000
n.quota <- 750
n.quota.infauna <- 250

for(i in 1:nrow(infaunadom_df)){
  
  this.stage <- phanero_stages[i]
  this.stage.data <- subset(all_data, stage==this.stage)
  
  #dataframes to save results for each bootstrap to get average result and sd
  n_tot_formations_temp <- rep(NA, iter) #total formations captured in n.quota random samples
  n_EE_formations_temp <- rep(NA, iter) #reef EE formations captured in n.quota random samples 
  n_formation_prop_temp <- rep(NA, iter) #proportion of reef formations captured in n.quota random samples 
  
  n_occs_tot_temp <- rep(NA, iter) #total fossil occurrences in engineered formations
  n_occs_EEs <- rep(NA, iter) #total ecosystem engineers in engineered formations
  EEoccs_prop_temp <- rep(NA, iter) #proportion of taxa in engineered formations that are ecosystem engineers
  
  max.samples <- nrow(this.stage.data) #get number of occurrences in this stage for subsampling

  #and subset stage data for just the infauna
  this.stage.infauna.data <- subset(infauna_data, stage==this.stage)
  most_infauna_class_temp <- rep(NA, iter)
  most_infauna_feeding_temp <- rep(NA, iter)
  max.infauna.samples <- nrow(this.stage.infauna.data)
  
  print(paste('on stage:', this.stage))
  
  for(j in 1:iter){
    
    #get random rows to pull data from
    row_idxs <- sample(max.samples, n.quota, replace=TRUE) #bootstrap = with replacement
    subbed.data <- this.stage.data[row_idxs,] #pull out subsampled data based on those random rows
    
    #==== proportion of formations ====#
    #number of formations we just pulled
    subbed.all.formations <- unique(subbed.data$formation)
    n_tot_formations_temp[j] <- length(subbed.all.formations) #number of unique formations 
    
    #number of just ecosystem engineering formations we just pulled ('engineered environments')
    #subbed.ecoeng.data <- subset(subbed.data, genus %in% infauna_EE_genera) #get the subbed data that has ecosystem engineers
    subbed.ecoeng.data <- subset(subbed.data, genus %in% ecoeng_genera) #get the subbed data that has ecosystem engineers
    subbed.ecoeng.formations <- unique(subbed.ecoeng.data$formation) #get what formations these taxa are in --> what formations are they engineering?
    n_EE_formations_temp[j] <- length(subbed.ecoeng.formations)
    #and get the proportion of ecosystem engineered environments
    n_formation_prop_temp[j] <- length(subbed.ecoeng.formations)/length(subbed.all.formations)
    
    #==== proportion of taxa that are ecosystem engineers ====#
    prop.ecoengs.temp <- rep(NA, length(subbed.ecoeng.formations))
    ecoengs.tot.occs <- rep(NA, length(subbed.ecoeng.formations))
    occs.tot <- rep(NA, length(subbed.ecoeng.formations))
    for(k in 1:length(subbed.ecoeng.formations)){
      this.formation.data <- subset(subbed.data, formation==subbed.ecoeng.formations[k]) #for each formation
      this.formation.ecoengs.data <- subset(this.formation.data, genus %in% ecoeng_genera) #identify ecosysteme engineers 
      this.formation.ecoengs <- nrow(this.formation.ecoengs.data) #how many are there?
      this.formation.tot <- nrow(this.formation.data) #how many total occurrences?
      this.formation.prop <- this.formation.ecoengs/this.formation.tot #proportion of occurrences that are ecosystem engineers?
      prop.ecoengs.temp[k] <- this.formation.prop #save 
      occs.tot[k] <- this.formation.tot
      ecoengs.tot.occs[k] <- this.formation.ecoengs
    }
    
    #engineered.data <- subset(subbed.data, formation %in% infauna_formations) #all data in an identified engineered formation
    # engineered.data <- subset(subbed.data, formation %in% ecoeng_formations) #all data in an identified engineered formation
    # engineered.tot.occs <- length(engineered.data$genus)
    # #engineered.engineers <- subset(engineered.data, genus %in% infauna_EE_genera) #subset out just the ecosystem engineers
    # engineered.engineers <- subset(engineered.data, genus %in% ecoeng_genera) #subset out just the ecosystem engineers
    # engineered.engineers.occs <- length(engineered.engineers$genus)
    # prop.engineers <- engineered.engineers.occs/engineered.tot.occs #and the proportion 
    
    n_occs_tot_temp[j] <- mean(occs.tot, na.rm=TRUE) #average across formations
    n_occs_EEs[j] <- mean(ecoengs.tot.occs, na.rm=TRUE) #average across formations 
    EEoccs_prop_temp[j] <- mean(prop.ecoengs.temp, na.rm=TRUE) #average across formations
    
    #==== most common infauna groups and feeding types ====$
    if(nrow(this.stage.infauna.data>0)){
      infauna.row.idxs <- sample(max.infauna.samples, n.quota.infauna, replace=TRUE)
      subbed.infauna.data <- this.stage.infauna.data[infauna.row.idxs,]
      most_infauna_class <- names(which.max(table(subbed.infauna.data$class)))
      most_infauna_class_temp[j] <- most_infauna_class
      most_infauna_feeding <- names(which.max(table(subbed.infauna.data$feeding)))
      most_infauna_feeding_temp[j] <- most_infauna_feeding
    }
    
  }
    
  #collect final means and statistics and save in infaunadom_df
  #formation dominance 
  infaunadom_df$n_tot_formations[i] <- mean(n_tot_formations_temp, na.rm=TRUE)
  infaunadom_df$n_EE_formations[i] <- mean(n_EE_formations_temp, na.rm=TRUE)
  infaunadom_df$prop_EE_formations[i] <- mean(n_formation_prop_temp, na.rm=TRUE)
  infaunadom_df$forms_sd[i] <- sd(n_formation_prop_temp, na.rm=TRUE)
  
  #taxa dominance
  infaunadom_df$n_occs_tot[i] <- mean(n_occs_tot_temp, na.rm=TRUE)
  infaunadom_df$n_occs_EEs[i] <- mean(n_occs_EEs, na.rm=TRUE)
  infaunadom_df$EEoccs_prop[i] <- mean(EEoccs_prop_temp, na.rm=TRUE)
  infaunadom_df$EEoccs_sd[i] <- sd(EEoccs_prop_temp, na.rm=TRUE)
  
  #main infaunan groups and feeders
  if(nrow(this.stage.infauna.data>0)){
    infaunadom_df$main_class[i] <- names(which.max(table(most_infauna_class_temp)))
    infaunadom_df$main_feeding[i] <- names(which.max(table(most_infauna_feeding_temp)))
  }
  
}


View(infaunadom_df)

save(infaunadom_df, file='infauna_dominance_04-07-2024.RData')


