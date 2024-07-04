
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


#=== feeding ===#
feeding_modes <- sort(unique(infauna_data$feeding))

variables <- c('stage', 'mid_ma', feeding_modes)
infauna_feeding_relabund <- as.data.frame(matrix(NA, nrow=length(phanero_stages), ncol=length(variables)))
colnames(infauna_feeding_relabund) <- variables
infauna_feeding_relabund$stage <- phanero_stages
infauna_feeding_relabund$mid_ma <- phanero_mids
infauna_feeding_relabund

iter <- 1000
n.quota <- 750
n.quota.infauna <- 250

for(i in 1:nrow(infauna_feeding_relabund)){
  
  this.stage <- infauna_feeding_relabund$stage[i]
  this.stage.data <- subset(infauna_data, stage==this.stage)
  max.samples <- nrow(this.stage.data)
  
  #set up temporary dataframe for subsampling
  temp.results <- as.data.frame(matrix(NA, nrow=iter, ncol=length(feeding_modes)))
  colnames(temp.results) <- feeding_modes
  
  
  print(paste('on stage:', this.stage))
  
  for(j in 1:iter){
    
    #get random samples
    row_idxs <- sample(max.samples, n.quota.infauna, replace=TRUE) 
    subbed.data <- this.stage.data[row_idxs,] #pull out subsampled data based on those random rows
    
    tot.infauna <- nrow(subbed.data)
    
    for(k in 1:length(feeding_modes)){
      
      this.infauna_feeding <- feeding_modes[k]
      this.infauna_feeding.data <- subset(subbed.data, feeding==this.infauna_feeding)
      n.this.infauna_feeding <- nrow(this.infauna_feeding.data)
      prop.this.infauna_feeding <- n.this.infauna_feeding/tot.infauna
      
      temp.results[j,this.infauna_feeding] <- prop.this.infauna_feeding
      
    }
    
    mean.stage.results <- colMeans(temp.results, na.rm=TRUE)
    infauna_feeding_relabund[i,3:ncol(infauna_feeding_relabund)] <- mean.stage.results
    
  }
  
}

View(infauna_feeding_relabund)
save(infauna_feeding_relabund, file='infauna_feeding_relativeabundance_04-17-2024.RData')