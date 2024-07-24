#Author: Dr. Alison T. Cribb, University of Southampton 

library(divDyn)
data("stages", package="divDyn")

#===================================================================#
#===================================================================#
#=== load data ===#
load('Data/Phanerozoic_clean_final.RData')
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


#===== relative abundance of feeding and taxonomic grops =====#
#taxonomic groups
variables <- c('stage', 'mid_ma', unique(infauna_data$phylum))
infauna_taxa_relabund <- as.data.frame(matrix(NA, nrow=length(phanero_stages), ncol=length(variables)))
colnames(infauna_taxa_relabund) <- variables
infauna_taxa_relabund$stage <- phanero_stages
infauna_taxa_relabund$mid_ma <- phanero_mids
infauna_taxa_relabund

iter <- 1000
n.quota <- 750
n.quota.infauna <- 250

for(i in 1:nrow(infauna_taxa_relabund)){
  
  this.stage <- infauna_taxa_relabund$stage[i]
  this.stage.data <- subset(infauna_data, stage==this.stage)
  max.samples <- nrow(this.stage.data)
  
  #set up temporary dataframe for subsampling
  temp.results <- as.data.frame(matrix(NA, nrow=iter, ncol=length(unique(infauna_data$phylum))))
  colnames(temp.results) <- unique(infauna_data$phylum)
  
  
  print(paste('on stage:', this.stage))
  
  for(j in 1:iter){
    
    #get random samples
    row_idxs <- sample(max.samples, n.quota.infauna, replace=TRUE) 
    subbed.data <- this.stage.data[row_idxs,] #pull out subsampled data based on those random rows
    
    tot.infauna <- nrow(subbed.data)
    
    for(k in 1:length(unique(infauna_data$phylum))){
      
      this.infauna_group <- unique(infauna_data$phylum)[k]
      this.infauna_group.data <- subset(subbed.data, phylum==this.infauna_group)
      n.this.infauna_group <- nrow(this.infauna_group.data)
      prop.this.infauna_group <- n.this.infauna_group/tot.infauna
      
      temp.results[j,this.infauna_group] <- prop.this.infauna_group
      
    }
    
    mean.stage.results <- colMeans(temp.results, na.rm=TRUE)
    infauna_taxa_relabund[i,3:ncol(infauna_taxa_relabund)] <- mean.stage.results
    
  }
  
}

save(infauna_taxa_relabund, file='bioturbator_taxa_relativeabundance.RData')







