source("scripts/plot_objects.R")

chloro <- read.csv("raw_data/chlorophyll_master.csv")
  chloro$treatment <- as.factor(chloro$treatment)
  chloro$chla <- 12.7*chloro$abs_664-2.79*chloro$abs_647
  chloro$chlb <- 20.7*chloro$abs_647+8.08*chloro$abs_664
  chloro$chlAB <- with(chloro, chla/chlb)
  chloro$chl_total <- with(chloro, chla+chlb)
  # chloro$chlAmass <- with(chloro, chlA/weight_mg)
  # chloro$chlBmass <- with(chloro, chlB/weight_mg)
  # chloro$chlAarea <- with(chloro, chlA/(punches * punch_diam_mm2))
  # chloro$chlBarea <- with(chloro, chlB/(punches * punch_diam_mm2))

#lettuce stats----------
lettuceaqua <- chloro[chloro$species == "lettuce" & chloro$treatment == "aqua",]
lettucesoil <- chloro[chloro$species == "lettuce" & chloro$treatment == "soil",]

lettuce_ttest_chl <- t.test(lettuceaqua$chl_total, lettucesoil$chl_total, 
                           alternative = "two.sided")
##aqua and soil differ (p < 0.001)

lettuce_ttest_chla <- t.test(lettuceaqua$chla, lettucesoil$chla, 
                            alternative = "two.sided")
##aqua and soil differ (p < 0.001)

lettuce_ttest_chlb <- t.test(lettuceaqua$chlb, lettucesoil$chlb, 
                            alternative = "two.sided")
##aqua and soil differ (p < 0.001)

#basil statss ----------
basilaqua <- chloro[chloro$species == "basil" & chloro$treatment == "aqua",]
basilsoil <- chloro[chloro$species == "basil" & chloro$treatment == "soil",]

basil_ttest_chl <- t.test(basilaqua$chl_total, basilsoil$chl_total, 
                         alternative = "two.sided")
##aqua and soil differ (p < 0.001)

basil_ttest_chla <- t.test(basilaqua$chla, basilsoil$chla, 
                          alternative = "two.sided")
##aqua and soil differ (p < 0.001

basil_ttest_chlb <- t.test(basilaqua$chlb, basilsoil$chlb, 
                          alternative = "two.sided")
##aqua and soil differ (p < 0.001


#wheat stats and graphs ----------
wheataqua <- chloro[chloro$species == "wheatgrass" & chloro$treatment == "aqua",]
wheatsoil <- chloro[chloro$species == "wheatgrass" & chloro$treatment == "soil",]

wheat_ttest_chl<- t.test(wheataqua$chl_total, wheatsoil$chl_total, 
                            alternative = "two.sided")

wheat_ttest_chla<- t.test(wheataqua$chla, wheatsoil$chla, 
                         alternative = "two.sided")

wheat_ttest_chlb<- t.test(wheataqua$chlb, wheatsoil$chlb, 
                         alternative = "two.sided")