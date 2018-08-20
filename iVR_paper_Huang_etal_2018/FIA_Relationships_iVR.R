
###This is the script to back-calculate dbh and height from species, age and biomass.
##So first it's just a table of plot CN, spp, age, biomass, dbh and height
## Then it graphs the relation of spp, age and biomass for dbh.  Separately for height.
## Then it calculates the curve of the relationhip.

library(plyr)
library(dplyr)
options(scipen=999) #This removes scientific notation in printing. Plot CNs are very long


#Here's the tree data.  I used data from WI and MI.  Decided MN was too different from WI.
dir<-("I:/Research/Shares/lucash_lab/Lucash/VIFF/FIA_data/")
setwd(dir)

tree_data_all<-read.csv("I:/Research/Samba/lucash_lab/Lucash/VIFF/FIA_data/FIA_age_added_matrix_combined_110717.csv")
tree_data_all[is.na(tree_data_all)]<-0

TPA_GROW_ADJ<-mapvalues(tree_data_all$TPAGROW_UNADJ,from=c(0.000000), to=c (6.018046))
tree_data_TPAadj<-cbind(tree_data_all, TPA_GROW_ADJ)

#This loop selects just the 44 species of interest from all the species in the FIA plots. Ignores other species.
spp_file<-read.csv("I:/Research/Shares/lucash_lab/Lucash/VIFF/FIA_data/CalculatingTreeAge/Site_index_coefficients_VIFF.csv")

PLT_CN_Column<-tree_data_TPAadj[,"PLT_CN"] #isolated the PLT_CN column in the WI_TREE dataset. 
unique_PLT_CN<-unique(PLT_CN_Column)  #isolated the unique values for PLT_CN in the isolated PLT_CN column. 

tree_data_merged<-merge(tree_data_TPAadj, spp_file[,1:4], by.x="SPCD", by.y="spp_num")
rm(tree_data_all, tree_data_TPAadj)

crown_file<-read.csv("I:/Research/Shares/lucash_lab/Lucash/VIFF/3D_Viz_Tool/CrownWidth_coefficients_VIFF.csv")
tree_data_merged_crown<-merge(tree_data_merged, crown_file[,4:8], by.x="SPCD", by.y="spp_num")

#################### Graphing original FIA data

dbh_FIA<-tree_data_all[,"DIA"]
hist(dbh_FIA)
height_FIA<-tree_data_all[,"HT"]
hist(height_FIA)


###################
pb <- txtProgressBar(min=0,max=length(unique_PLT_CN), initial=0, style=3)
cnt <- 1

Species_age_plot <- NULL  #creates an empty matrix
for (i in unique_PLT_CN){  #loop through every unique PLT_CN or CN
  SubsetEach_Plot <- subset(tree_data_merged_crown, tree_data_merged_crown[,"PLT_CN"]==i) #you subset each plot
  #example of how to get sample size
  #number_trees_plot<-nrow(Each_Plot)
  for (j in length(unique(SubsetEach_Plot[,"Final_spp_code"]))){  #loop through every species in each plot
    each_spp_code<-SubsetEach_Plot[j,"Final_spp_code"]
    Each_Species <- subset(SubsetEach_Plot, SubsetEach_Plot[,"Final_spp_code"] == each_spp_code)#subset each plot for each species
    Each_Species_CW_b0 <- unique(Each_Species[,"b0"])
    Each_Species_CW_b1 <- unique(Each_Species[,"b1"])
    Each_Species_cW_b2 <- unique(Each_Species[,"b2"])
    
    for (k in unique(Each_Species[,"calc_age_rounded"])){ ##Loop for individual species  #loop through every (practice variable(TREECLCD)) in each plot
     #each_age<-(Each_Species[k, "calc_age_rounded"])
      SubsetEach_Age <- subset(Each_Species, Each_Species[,"calc_age_rounded"] == k)  #subset each plot for rounded age
      cohort_biomass <- as.numeric(SubsetEach_Age[,"CARBON_AG"]) #converting to numeric 
      cohort_biomass_sum <- sum(cohort_biomass) #converting to numeric 
      cohort_age <- as.numeric(SubsetEach_Age[,"calc_age"]) #converting to numeric 
      cohort_biomass_avg <- mean(cohort_age) #converting to numeric 
      tpa_adjust_factor <- as.numeric(SubsetEach_Age[,"TPA_GROW_ADJ"]) #converting to numeric 
      tpa_adjust_factor_mean <- mean(tpa_adjust_factor) #converting to numeric 
      cohort_biomass_gm2 <- (cohort_biomass_sum * 2 * tpa_adjust_factor_mean * 453.592)/4046.86 #convert from C to biomass, lbs to grams, divide by acres to m2 conversion
      cohort_dbh<- as.numeric(SubsetEach_Age[,"DIA"]) #converting to numeric 
      dbh_mean <- mean(cohort_dbh) #dbh
      cohort_crownWidth <-  (Each_Species_CW_b0 + (Each_Species_CW_b1 *  dbh_mean) + (Each_Species_cW_b2 * (dbh_mean^2)))#
      cohort_ht<- as.numeric(SubsetEach_Age[,"HT"]) #converting to numeric 
      height_mean <- mean(cohort_ht) #height
      number_trees <- nrow(Each_Species)
      
      
    spp_cohort <- cbind(i, each_spp_code, k, cohort_biomass_avg, cohort_biomass_gm2,dbh_mean, height_mean, number_trees, cohort_crownWidth)#bind together the plot number, the species number and the biomass
    Species_age_plot <- rbind(Species_age_plot, spp_cohort)
    
    }
   
  }
  #increment progress bar
  setTxtProgressBar(pb, cnt)
  cnt <- cnt + 1 
  
}
colnames(Species_age_plot)<-c("PLT_CN", "SpeciesCode", "CohortAge", "MeanAge", "Biomass_gm2","DBH", "Height", "NumberTrees", "CrownWidth")
print("done")
print(head(Species_age_plot))
print(tail(Species_age_plot))

Species_age_plot_merge<-merge(Species_age_plot, spp_file[,1:4], by.x="SpeciesCode", by.y="spp_num")
Species_age_plot_merge_NonZero<-subset(Species_age_plot_merge, Species_age_plot_merge$Biomass_gm2>0)


Species_age_plot_sorted<-Species_age_plot_merge_NonZero[order(Species_age_plot_merge_NonZero[,"FinalName"], Species_age_plot_merge_NonZero[,"CohortAge"], Species_age_plot_merge_NonZero[,"PLT_CN"] ),]
write.csv(Species_age_plot_sorted,("I:/Research/Samba/lucash_lab/Lucash/VIFF/FIA_data/iVR_Linkage/FIA_plot_data_iVR_11.18.csv"))


########################################################
#Switch over to making graphs of age for each species.

Species_age_plot_sorted<-read.csv("I:/Research/Samba/lucash_lab/Lucash/VIFF/FIA_data/iVR_Linkage/FIA_plot_data_iVR_11.18.csv")
Species_age_plot_sorted[is.na(Species_age_plot_sorted)]<-0

output.dir<-("I:/Research/Shares/lucash_lab/Lucash/VIFF/FIA_data/iVR_Linkage/OutputGraphs/")
setwd(output.dir)


TPA_adjustment_Factor<-6.018046

calc_age<-Species_age_plot_sorted[,"CohortAge"]
spec<-Species_age_plot_sorted[,"FinalName"]
dbh<-Species_age_plot_sorted[,"DBH"]
height<-Species_age_plot_sorted[,"Height"]
biomass<-Species_age_plot_sorted[,"Biomass_gm2"]
numbertrees_adjusted<-(Species_age_plot_sorted[,"NumberTrees"]* TPA_adjustment_Factor)/0.404686  #use TPA adjust factor, convert from acres to ha
numbertrees_adjusted_acres<-(Species_age_plot_sorted[,"NumberTrees"]* TPA_adjustment_Factor)
crown_width<-Species_age_plot_sorted[,"CrownWidth"]
basal_area<-(dbh^2)*0.005454*TPA_adjustment_Factor
hist(basal_area)

Species_age_plot_sorted<-cbind(Species_age_plot_sorted, numbertrees_adjusted)

#To make sure trees per acre is reasonable 
#https://woodlandinfo.org/sites/woodlandinfo.org/files/pdf/UWEX/G3362.pdf

#final Data
hist(dbh)
hist(height)
hist(biomass)
hist(numbertrees_adjusted)
hist(numbertrees_adjusted_acres)
hist(crown_width)
hist(log(biomass))

plot(height~calc_age)
plot(dbh~calc_age)
plot(biomass~calc_age)
plot(height~dbh)
plot(crown_width~dbh)
plot(crown_width~calc_age)

max(numbertrees_adjusted)
max(height)
max(crown_width)


#########################################

#This is the loop to make graphs of Crown width for all the species
species_names<-as.factor(unique(Species_age_plot_sorted[,"FinalName"]))

#for (m in unique(Species_age_plot_sorted[,"FinalName"])){
  for (m in 1:length(species_names)){
    each_spp<-(species_names[m])
  spp_select <- subset(Species_age_plot_sorted, Species_age_plot_sorted$FinalName==each_spp)
  age_spp<-spp_select[,"CohortAge"]
  crown_spp<-spp_select[,"CrownWidth"]
  output_plot_dbh<-plot(age_spp, crown_spp, xlab="Age", ylab="Crown Width (ft)", main=paste("Graph of ", each_spp, sep=''))
  plot_name<- paste(output.dir, each_spp, "_CrownWidth.jpg", sep="")
  print(plot_name)
  png(output_plot_dbh, filename = plot_name, width=480, height = 480, units="px", bg="white")

}  
print("done")

#This is the loop to make graphs and output for all the species
for (m in 1:length(species_names)){
  each_spp<-(species_names[m])
  spp_select <- subset(Species_age_plot_sorted, Species_age_plot_sorted$FinalName==each_spp)
  age_spp<-spp_select[,"CohortAge"]
  height_spp<-spp_select[,"Height"]
  plot2_name<- paste(output.dir, each_spp, "_HT.jpg", sep="")
  output_plot_ht<-plot(age_spp, height_spp, xlab="Age", ylab="Height (ft)", main=paste("Graph of ", each_spp, sep=''))
  png(output_plot_ht, filename = plot2_name, width=480, height = 480, units="px", bg="white")

}  
print("done")

graphics.off()
#This is the loop to make graphs of Number of Trees and output for all the species
for (m in 1:length(species_names)){
  each_spp<-(species_names[m])
  spp_select <- subset(Species_age_plot_sorted, Species_age_plot_sorted$FinalName==each_spp)
  age_spp<-spp_select[,"CohortAge"]
  crown_spp<-spp_select[,"CrownWidth"]
  NumberTrees_spp<-spp_select[,"NumberTrees"]
  output_plot_trees<-plot(age_spp, NumberTrees_spp, xlab="Age", ylab="Number of Trees", main=paste("Graph of ", each_spp, sep=''))
  plot3_name<- paste(output.dir, each_spp, "_NumberTrees.jpg", sep="")
  png(output_plot_trees, filename = plot3_name, width=480, height = 480, units="px", bg="white")
}  
print("done")

#*******************

species_names<-(unique(Species_age_plot_sorted[,"FinalName"]))
#Regression plot of Crown Width
for (m in 1:length(species_names)){
  each_spp<-(species_names[m])
  spp_select <- subset(Species_age_plot_sorted, Species_age_plot_sorted$FinalName==each_spp)
  age_spp<-spp_select[,"CohortAge"]
  biomass_spp<-spp_select[,"Biomass_gm2"]
  crown_spp<-spp_select[,"CrownWidth"]
  fit<- lm(crown_spp~age_spp+ Biomass_gm2, data=spp_select)
  cf <- round(coef(fit), 6) 
  eq <- paste0("crown width = ", cf[1], ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " age ", ifelse(sign(cf[3])==1, " + ", " - "), abs(cf[3]), " biomass")
  #summary(fit)
  plot3_name<- paste(output.dir, each_spp, "_CrownWidth_Regressions.jpg", sep="")
  output_CW<-plot(crown_spp, fitted(fit), xlab="CrownWidth Calculated", ylab="CrownWidth Fitted", main=paste("Graph of ", each_spp, sep=''), mtext(eq, 3, line=-2))
  jpeg(output_CW, filename = plot3_name, width=480, height = 480, units="px", bg="white")
}

#Regression plot of Number of Treesfor (m in 1:length(species_names)){
for (m in 1:length(species_names)){
  each_spp<-(species_names[m])
  spp_select <- subset(Species_age_plot_sorted, Species_age_plot_sorted$FinalName==each_spp)
  age_spp<-spp_select[,"CohortAge"]
  numbertrees_spp<-spp_select[,"numbertrees_adjusted"]
  biomass_spp<-spp_select[,"Biomass_gm2"]
  fit<- lm(numbertrees_spp~age_spp+ Biomass_gm2, data=spp_select)
  cf <- round(coef(fit), 6) 
  eq <- paste0("NumberTrees = ", cf[1], ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " age ", ifelse(sign(cf[3])==1, " + ", " - "), abs(cf[3]), " biomass")
  summary(fit)
  output_CW<-plot(numbertrees_spp, fitted(fit), xlab="NumberTrees Calculated", ylab="NumberTrees Fitted", main=paste("Graph of ", each_spp, sep=''), mtext(eq, 3, line=-2))
  png(output_CW, filename = paste(output.dir, paste(each_spp, "_TreeNumber_Regressions.jpg", sep="")), width=480, height = 480, units="px", bg="white")
}


#Regression plot of Height of Trees
for (m in 1:length(species_names)){
  each_spp<-(species_names[m])
  spp_select <- subset(Species_age_plot_sorted, Species_age_plot_sorted$FinalName==each_spp)
  age_spp<-spp_select[,"CohortAge"]
  height_spp<-spp_select[,"Height"]
  biomass_spp<-spp_select[,"Biomass_gm2"]
  fit<- lm(height_spp~age_spp+ Biomass_gm2, data=spp_select)
  cf <- round(coef(fit), 6) 
  eq <- paste0("Height = ", cf[1], ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " age ", ifelse(sign(cf[3])==1, " + ", " - "), abs(cf[3]), " biomass")
  summary(fit)
  output_CW<-plot(height_spp, fitted(fit), xlab="Height Calculated", ylab="Height Fitted", main=paste("Graph of ", each_spp, sep=''), mtext(eq, 3, line=-2))
  plot3_name<- paste(output.dir, each_spp, "_Height_Regressions.jpg", sep="")
  png(output_CW, filename = plot3_name, width=480, height = 480, units="px", bg="white")
}
