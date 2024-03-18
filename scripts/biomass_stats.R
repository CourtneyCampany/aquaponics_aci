source("scripts/functions.R")

biomass <- read.csv("raw_data/biomass_aci.csv")
  biomass$totalbiomass <- with(biomass, shoots_mg + roots_mg)
  biomass$rs_ratio <- with(biomass, roots_mg/shoots_mg)

biomass_agg <- doBy::summaryBy(shoots_mg + roots_mg + totalbiomass + rs_ratio ~ treatment + species, 
                               data =biomass, FUN=c(mean2, sd2, se), keep.names=TRUE)


#lettuce stats----------
lettuceaqua <- biomass[biomass$species == "lettuce" & biomass$treatment == "a",]
lettucesoil <- biomass[biomass$species == "lettuce" & biomass$treatment == "s",]

lettuce_ttest <- t.test(lettuceaqua$totalbiomass, lettucesoil$totalbiomass, 
                        alternative = "two.sided")

lettuce_ttest_roots <- t.test(lettuceaqua$roots_mg, lettucesoil$roots_mg, 
                              alternative = "two.sided")

lettuce_ttest_above <- t.test(lettuceaqua$shoots_mg, lettucesoil$shoots_mg, 
                              alternative = "two.sided")

lettuce_ttest_ratio <- t.test(lettuceaqua$rs_ratio, lettucesoil$rs_ratio, 
                              alternative = "two.sided")

##all biomass variables are different, but root to shoot ratio does not change


#basil statss ----------
basilaqua <- biomass[biomass$species == "basil" & biomass$treatment == "a",]
basilsoil <- biomass[biomass$species == "basil" & biomass$treatment == "s",]

basil_ttest <- t.test(basilaqua$totalbiomass, basilsoil$totalbiomass, 
                    alternative = "two.sided")

basil_ttest_roots <- t.test(basilaqua$roots_mg, basilsoil$roots_mg, 
                          alternative = "two.sided")

basil_ttest_above <- t.test(basilaqua$shoots_mg, basilsoil$shoots_mg, 
                          alternative = "two.sided")

basil_ttest_ratio <- t.test(basilaqua$rs_ratio, basilsoil$rs_ratio, 
                          alternative = "two.sided")

##total, above and R:D differ, roots do not


#wheat stats and graphs ----------
wheataqua <- biomass[biomass$species == "wheat" & biomass$treatment == "a",]
wheatsoil <- biomass[biomass$species == "wheat" & biomass$treatment == "s",]

wheat_ttest <- t.test(wheataqua$totalbiomass, wheatsoil$totalbiomass, 
                     alternative = "two.sided")

wheat_ttest_roots <- t.test(wheataqua$roots_mg, wheatsoil$roots_mg, 
                           alternative = "two.sided")

wheat_ttest_above <- t.test(wheataqua$shoots_mg, wheatsoil$shoots_mg, 
                           alternative = "two.sided")

wheat_ttest_ratio <- t.test(wheataqua$rs_ratio, wheatsoil$rs_ratio, 
                           alternative = "two.sided")
#all biomass variables and R:S differ

