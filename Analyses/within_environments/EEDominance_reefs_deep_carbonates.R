#Author: Dr. Alison T. Cribb, University of Southampton 

library(divDyn)
data("stages", package="divDyn")
data("keys", package="divDyn")


#===================================================================#
#===================================================================#

#=== load data ===#
load('Data/Phanerozoic_clean_final.RData')
all_data <- subset(all_data, !(is.na(formation)))
load('Data/Reef_Ecosystem_Engineers_final.RData')
all_reef_builders <- subset(all_reef_builders, !(is.na(formation)))
phanero_stages <- stages$stage[4:95]
phanero_mids <- stages$mid[4:95]

#assign environmental variables
all_data$lith <- categorize(all_data$lithology1, keys$lith)
all_data$bath <- categorize(all_data$environment, keys$bath)
all_reef_builders$lith <- categorize(all_reef_builders$lithology1, keys$lith)
all_reef_builders$bath <- categorize(all_reef_builders$environment, keys$bath)

#just the shallow data
all_deep_carb_data <- subset(all_data, bath=='deep' & lith=='carbonate')
deep_carb_reefs <- subset(all_reef_builders, bath=='deep' & lith=='carbonate')


#=== get reef formations ===#
#save reef formations
reef_formations <- unique(deep_carb_reefs$formation)
#save reef genera 
reef_EE_genera <- unique(deep_carb_reefs$genus)

#=== set up output ==#
variables <- c('stage', 'mid_ma', 
               'n_tot_formations', 'n_EE_formations', 'prop_EE_formations', 'forms_sd', 
               'n_occs_tot', 'n_occs_EEs', 'EEoccs_prop', 'EEoccs_sd', 'main_builder')
reefdom_df <- as.data.frame(matrix(nrow=length(phanero_stages), ncol=length(variables)))
colnames(reefdom_df) <- variables
reefdom_df$stage <- phanero_stages
reefdom_df$mid_ma <- phanero_mids
head(reefdom_df)

iter <- 1000
n.quota <- 750 
n.quota.reefs <- 250 

#=== analysis - how common are reef engineered environments? ===#
for(i in 1:nrow(reefdom_df)){
  
  this.stage <- reefdom_df$stage[i]
  this.stage.data <- subset(all_deep_carb_data, stage==this.stage)
  
  #dataframes to save results for each bootstrap to get average result and sd
  n_tot_formations_temp <- rep(NA, iter) #total formations captured in n.quota random samples
  n_EE_formations_temp <- rep(NA, iter) #reef EE formations captured in n.quota random samples 
  n_formation_prop_temp <- rep(NA, iter) #proportion of reef formations captured in n.quota random samples 
  
  n_occs_tot_temp <- rep(NA, iter) #total fossil occurrences in engineered formations
  n_occs_EEs <- rep(NA, iter) #total ecosystem engineers in engineered formations
  EEoccs_prop_temp <- rep(NA, iter) #proportion of taxa in engineered formations that are ecosystem engineers
  max.samples <- nrow(this.stage.data) #get number of occurrences in this stage for subsampling
  
  #also need this stage data for just the reef builders 
  this.stage.reef.data <- subset(deep_carb_reefs, stage==this.stage)
  this.stage.reef.formations <- length(unique(this.stage.reef.data$formation))
  most_reefs_temp <- rep(NA,iter) #temp vector to save most dominant reef builder 
  max.reef.samples <- nrow(this.stage.reef.data)
  
  print(paste('on stage:', this.stage))
  
  if(nrow(this.stage.reef.data>1)){ #only proceed if there are enough reef formations 
    for(j in 1:iter){
      
      #get random rows to pull data from
      row_idxs <- sample(max.samples, n.quota, replace=TRUE) #bootstrap = with replacement
      subbed.data <- this.stage.data[row_idxs,] #pull out subsampled data based on those random rows
      
      #==== proportion of formations ====#
      #number of formations we just pulled
      subbed.all.formations <- unique(subbed.data$formation)
      n_tot_formations_temp[j] <- length(subbed.all.formations) #number of unique formations 
      
      #number of just ecosystem engineering formations we just pulled ('engineered environments')
      subbed.ecoeng.data <- subset(subbed.data, genus %in% reef_EE_genera) #get the subbed data that has ecosystem engineers
      subbed.ecoeng.formations <- unique(subbed.ecoeng.data$formation) #get what formations these taxa are in --> what formations are they engineering?
      n_EE_formations_temp[j] <- length(subbed.ecoeng.formations)
      
      #and get the proportion of ecosystem engineered environments
      n_formation_prop_temp[j] <- length(subbed.ecoeng.formations)/length(subbed.all.formations)
      
      #==== proportion of taxa that are ecosystem engineers ====#
      prop.ecoengs.temp <- rep(NA, length(subbed.ecoeng.formations))
      ecoengs.tot.occs.temp <- rep(NA, length(subbed.ecoeng.formations))
      occs.tot.temp <- rep(NA, length(subbed.ecoeng.formations))
      for(k in 1:length(subbed.ecoeng.formations)){
        this.formation.data <- subset(subbed.data, formation==subbed.ecoeng.formations[k]) #for each formation
        this.formation.ecoengs.data <- subset(this.formation.data, genus %in% reef_EE_genera) #identify ecosystem engineers 
        this.formation.ecoengs <- nrow(this.formation.ecoengs.data) #how many are there?
        this.formation.tot <- nrow(this.formation.data) #how many total occurrences?
        this.formation.prop <- this.formation.ecoengs/this.formation.tot #proportion of occurrences that are ecosystem engineers?
        prop.ecoengs.temp[k] <- this.formation.prop #save 
        occs.tot.temp[k] <- this.formation.tot
        ecoengs.tot.occs.temp[k] <- this.formation.ecoengs
      }
      
      # engineered.data <- subset(subbed.data, formation %in% reef_formations) #all data in an identified reef formation
      # engineered.tot.occs <- length(engineered.data$genus)
      # engineered.engineers <- subset(engineered.data, genus %in% reef_EE_genera) #subset out just the ecosystem engineers
      # engineered.engineers.occs <- length(engineered.engineers$genus)
      # prop.engineers <- engineered.engineers.occs/engineered.tot.occs #and the proportion 
      
      n_occs_tot_temp[j] <- mean(occs.tot.temp, na.rm=TRUE)
      n_occs_EEs[j] <- mean(ecoengs.tot.occs.temp, na.rm=TRUE)
      EEoccs_prop_temp[j] <- mean(prop.ecoengs.temp, na.rm=TRUE)
      
      #==== most common reef-building group ====#
      if(nrow(this.stage.reef.data>0)){ #can only do this and only want to do this if there are any reefs
        reef.row.idxs <- sample(max.reef.samples, n.quota.reefs, replace=TRUE)
        subbed.reef.data <- this.stage.reef.data[reef.row.idxs,]
        most_reefs <- names(which.max(table(subbed.reef.data$REE_classification)))
        most_reefs_temp[j] <- most_reefs
      }
    }
  } 
  
  #collect final means and statistics and save in reefdom_df
  #formation dominance 
  reefdom_df$n_tot_formations[i] <- mean(n_tot_formations_temp, na.rm=TRUE)
  reefdom_df$n_EE_formations[i] <- mean(n_EE_formations_temp, na.rm=TRUE)
  reefdom_df$prop_EE_formations[i] <- mean(n_formation_prop_temp, na.rm=TRUE)
  reefdom_df$forms_sd[i] <- sd(n_formation_prop_temp, na.rm=TRUE)
  
  #taxa dominance
  reefdom_df$n_occs_tot[i] <- mean(n_occs_tot_temp, na.rm=TRUE)
  reefdom_df$n_occs_EEs[i] <- mean(n_occs_EEs, na.rm=TRUE)
  reefdom_df$EEoccs_prop[i] <- mean(EEoccs_prop_temp, na.rm=TRUE)
  reefdom_df$EEoccs_sd[i] <- sd(EEoccs_prop_temp, na.rm=TRUE)
  
  #biggest reef builder
  if(nrow(this.stage.reef.data>0)){
    reefdom_df$main_builder[i] <- names(which.max(table(most_reefs_temp)))
  }
}


reefdom_deep_carb_df <- reefdom_df
save(reefdom_deep_carb_df, file='reef_dominance_deepcarbonates.RData')

