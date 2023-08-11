source("scripts/functions.R")

##T-tests for biomass and biomass partitioning------
biomass <- read.csv("raw_data/biomassharvest_romaine.csv")
  biomass$totalbiomass <- with(biomass, shoots_g + roots_g)
  biomass$rs_ratio <- with(biomass, roots_g/shoots_g)
  biomass$treatment <- as.factor(biomass$treatment)

biomass_agg <- doBy::summaryBy(.~treatment, data=biomass, FUN = mean2, keep.names = TRUE)


biomassaqua <- biomass[biomass$treatment == "A",]
biomasssoil <- biomass[biomass$treatment == "S",]

biomass_ttest <- t.test(biomassaqua$totalbiomass, biomasssoil$totalbiomass, 
                     alternative = "two.sided") #yes, P <0.0001 aqua >

roots_ttest <- t.test(biomassaqua$roots_g, biomasssoil$roots_g, 
                           alternative = "two.sided") #yes, p<0.0001 aqua >

above_ttest <- t.test(biomassaqua$shoots_g, biomasssoil$shoots_g, 
                           alternative = "two.sided") #yes, p<0.0001 aqua >

ratio_ttest <- t.test(biomassaqua$rs_ratio, biomasssoil$rs_ratio, 
                           alternative = "two.sided") #not different

##root to shoot not different

mean(brocaqua$rs_ratio)
mean(brocsoil$rs_ratio)

##T-test for specific leaf area

sla <- read.csv("raw_data/specificleafarea_romaine.csv")
  sla$drymass_ss <- with(sla, mass_mg * number_of_punches)
  sla$leafarea_ss <- with(sla, punch_area * number_of_punches)
  sla$sla_ <- with(sla, leafarea_ss/drymass_ss)
  sla$treatment <- as.factor(biomass$treatment)
  
slaaqua <- sla[sla$treatment == "A",]
slassoil <- sla[sla$treatment == "S",]
  
sla_ttest <- t.test(slaaqua$sla, slassoil$sla, alternative = "two.sided") #yes, P <0.0001 aqua >
mean(slaaqua$sla)
mean(slassoil$sla)

## T-test for chlorophyll

chloro <- read.csv("raw_data/chlorophyll_romaine.csv")
  chloro$treatment <- as.factor(chloro$treatment)
  chloro$chlAB <- with(chloro, chlA/chlB)
  chloro$chl_total <- with(chloro, chlA+chlB)

chlaqua <- chloro[chloro$treatment == "A",]
chlsoil <- chloro[chloro$treatment == "C",]
  
chla_ttest <- t.test(chlaqua$chlA, chlsoil$chlA, alternative = "two.sided")
#yes, P <0.0001 aqua >
  mean(chlaqua$chlA)
  mean(chlsoil$chlA)
  
chlb_ttest <- t.test(chlaqua$chlB, chlsoil$chlB, alternative = "two.sided")
#yes, P <0.0001 aqua >
  mean(chlaqua$chlB)
  mean(chlsoil$chlB)
  