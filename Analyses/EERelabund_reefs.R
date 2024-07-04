#Author: Dr. Alison T. Cribb, University of Southampton 
#Created: 7 September 2023
#Last edited: 4 July 2024

#Summary: Dominance of reef environments, or "how common are reef-engineered environments?"
#         First: Proportion of environments that contain reefs, per stage
#         Second: In reef engineneered formations, what proportion of occurrences are reef-builders?
#         Third: What is the dominant reef builder taxa in each stage?

library(divDyn)
data(stages)

#===================================================================#
#===================================================================#
#setwd("~/Desktop/Manucripts/Palass_ecosystemengineering")

#=== load data ===#
#load('Data/Phanerozoic_clean_final.RData')
load('Phanerozoic_clean_final.RData')
all_data <- subset(all_data, !(is.na(formation)))
#load('Data/Reef_Ecosystem_Engineers_final.RData')
load('Reef_Ecosystem_Engineers_final.RData')
all_reef_builders <- subset(all_reef_builders, !(is.na(formation)))
phanero_stages <- stages$stage[4:95]
phanero_mids <- stages$mid[4:95]

#=== get reef formations ===#
#save reef formations
reef_formations <- unique(all_reef_builders$formation)
#save reef genera 
reef_EE_genera <- unique(all_reef_builders$genus)

#=== set up output ==#
variables <- c('stage', 'mid_ma', unique(all_reef_builders$REE_classification))
reef_relabund <- as.data.frame(matrix(NA, nrow=length(phanero_stages), ncol=length(variables)))
colnames(reef_relabund) <- variables
reef_relabund$stage <- phanero_stages
reef_relabund$mid_ma <- phanero_mids
reef_relabund

iter <- 1000
n.quota <- 750 
n.quota.reefs <- 250 

for(i in 1:nrow(reef_relabund)){
  
  this.stage <- reef_relabund$stage[i]
  this.stage.data <- subset(all_reef_builders, stage==this.stage)
  max.samples <- nrow(this.stage.data)
  
  #set up temporary dataframe for subsampling
  temp.results <- as.data.frame(matrix(NA, nrow=iter, ncol=length(unique(all_reef_builders$REE_classification))))
  colnames(temp.results) <- unique(all_reef_builders$REE_classification)
  
  
  print(paste('on stage:', this.stage))
  
  for(j in 1:iter){
    
    #get 1500 random samples
    row_idxs <- sample(max.samples, n.quota.reefs, replace=TRUE) 
    subbed.data <- this.stage.data[row_idxs,] #pull out subsampled data based on those random rows
    
    tot.reefs <- nrow(subbed.data)
    
    for(k in 1:length(unique(all_reef_builders$REE_classification))){
      
      this.reef_builder <- unique(all_reef_builders$REE_classification)[k]
      this.reef_builder.data <- subset(subbed.data, REE_classification==this.reef_builder)
      n.this.reef_builder <- nrow(this.reef_builder.data)
      prop.this.reef <- n.this.reef_builder/tot.reefs
      
      temp.results[j,this.reef_builder] <- prop.this.reef
      
    }
    
    mean.stage.results <- colMeans(temp.results, na.rm=TRUE)
    reef_relabund[i,3:ncol(reef_relabund)] <- mean.stage.results
    
  }
  
}

View(reef_relabund)
save(reef_relabund, file='reef_relativeabundance_04-07-2024.RData')
