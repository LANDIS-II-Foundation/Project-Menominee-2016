
#Choose either CC or historic.

ModelOutput<-read.csv("I:/Research/Samba/lucash_lab/Lucash/VIFF/3D_Viz_Tool/ModelOutput_csvFiles/community-input-file-CC-50.csv")
map_codes<-read.csv("I:/Research/Samba/lucash_lab/Lucash/VIFF/3D_Viz_Tool/ModelOutput_csvFiles/CC_bitty.csv")

ModelOutput<-read.csv("I:/Research/Samba/lucash_lab/Lucash/VIFF/3D_Viz_Tool/ModelOutput_csvFiles/community-input-file-50.csv")
map_codes<-read.csv("I:/Research/Samba/lucash_lab/Lucash/VIFF/3D_Viz_Tool/ModelOutput_csvFiles/Historic_bitty.csv")

map_codes_used<-map_codes[,2]
SelectMapCodes <- ModelOutput[(ModelOutput$MapCode %in% map_codes_used),]

RegressionEquations<-read.csv("I:/Research/Samba/lucash_lab/Lucash/VIFF/3D_Viz_Tool/Regression_Equations.csv")

#roundUp <- function(x) 10*ceiling(x/10)   #Need to round the age to a multiple of 10.

################################################################
pb <- txtProgressBar(min=0,max=nrow(ModelOutput), initial=0, style=3)
cnt <- 1


one_equation<-RegressionEquations[1,]

Age<-SelectMapCodes[,"CohortAge"]#Diameter (inches)
Biomass<-SelectMapCodes[,"CohortBiomass"]#Height (ft)

number_trees_unrounded<-(one_equation[,2]+(one_equation[,3]*Biomass)+(one_equation[,4]*Age))/0.404686  # number of trees in FIA 1 acre plot converted to 1 ha
number_trees<-round(number_trees_unrounded)
dbh<-one_equation[,5]+(one_equation[,6]*Biomass)+(one_equation[,7]*Age)
height<-one_equation[,8]+(one_equation[,9]*Biomass)+(one_equation[,10]*Age)
spatial_contagion<-one_equation[,11]+(one_equation[,12]*Age)
crown_width<-one_equation[,14]+(one_equation[,15]*Age)

hist(height)

final_matrix<-cbind(SelectMapCodes, number_trees, dbh, height, spatial_contagion, crown_width)
write.csv(final_matrix, "I:/Research/Samba/lucash_lab/Lucash/VIFF/3D_Viz_Tool/ModelOutput_csvFiles/community-input-file-Historic_IttyBitty-iVR_11.17.17.csv")


############################### 

matrix_trees_equations<-merge(SelectMapCodes, RegressionEquations, by.x="SpeciesName", by.y="Species")

number_trees_unrounded<-(matrix_trees_equations[,"Trees_a1"]+(matrix_trees_equations[,"Trees_a2"]*matrix_trees_equations[,"CohortAge"])+(matrix_trees_equations[,"Trees_a3"]*(matrix_trees_equations[,"CohortBiomass"])))
number_trees<-round(number_trees_unrounded)
height_unrounded<-number_trees_unrounded<-(matrix_trees_equations[,"Height_a1"]+(matrix_trees_equations[,"Height_a2"]*matrix_trees_equations[,"CohortAge"])+(matrix_trees_equations[,"Trees_a3"]*(matrix_trees_equations[,"CohortBiomass"])))
height<-round(height_unrounded)
crown_width_unrounded<-number_trees_unrounded<-(matrix_trees_equations[,"Crown_a1"]+(matrix_trees_equations[,"Crown_a2"]*matrix_trees_equations[,"CohortAge"])+(matrix_trees_equations[,"Crown_a3"]*(matrix_trees_equations[,"CohortBiomass"])))
crown_width<-round(crown_width_unrounded)
spatial_contagion<-number_trees_unrounded<-(matrix_trees_equations[,"Spatial_a1"]+(matrix_trees_equations[,"Spatial_a2"]*matrix_trees_equations[,"CohortAge"]))

species_matrix<-cbind(matrix_trees_equations, number_trees, height, crown_width, spatial_contagion)
write.csv(species_matrix, "I:/Research/Samba/lucash_lab/Lucash/VIFF/3D_Viz_Tool/ModelOutput_csvFiles/community-input-file-ClimateChange_IttyBitty-iVR_02.28.18.csv")
