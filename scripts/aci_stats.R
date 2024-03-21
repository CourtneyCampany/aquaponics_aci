source("scripts/functions.R")
wheat_aci <- read.csv("aci_parameters/wheat/wheat_aci_params.csv")

#wheat stats and graphs ----------
wheataqua <- aci[aci$treatment == "aqua",]
wheatsoil <- aci[aci$treatment == "soil",]

wheat_ttest_vcmax <- t.test(wheataqua$V_cmax, wheatsoil$V_cmax, 
                      alternative = "two.sided")

mean(wheataqua$V_cmax) #75.1
mean(wheatsoil$V_cmax) #63

wheat_ttest_jmax <- t.test(wheataqua$J_max, wheatsoil$J_max, 
                            alternative = "two.sided")

mean(wheataqua$J_max) #177.3
mean(wheatsoil$J_max) #140.1

#all aci variables differ

#lettuce stats----------
lettuceaci <- read.csv("aci_parameters/lettuce/lettuce_aci_params.csv")

lettuceaqua <- lettuceaci[lettuceaci$treatment == "aqua",]
lettucesoil <- lettuceaci[lettuceaci$treatment == "soil",]

lettuce_ttest_vcmax <- t.test(lettuceaqua$V_cmax, lettucesoil$V_cmax, 
                            alternative = "two.sided")

#vcmax differs p<0.001
mean(lettuceaqua$V_cmax) #51.1
mean(lettucesoil$V_cmax) #29.9

lettuce_ttest_jmax <- t.test(lettuceaqua$J_max, lettucesoil$J_max, 
                           alternative = "two.sided")

#jmax differs p<0.001

mean(lettuceaqua$J_max) #109.7
mean(lettucesoil$J_max) #67.8

##all aci variables are different, but root to shoot ratio does not change


#basil stats ----------

basil_aci <- read.csv("aci_parameters/basil/basil_aci_params.csv")

#basil stats and graphs ----------
basilaqua <- basil_aci[basil_aci$treatment == "aqua",]
basilsoil <- basil_aci[basil_aci$treatment == "soil",]


basil_ttest_vcmax <- t.test(basilaqua$V_cmax, basilsoil$V_cmax, 
                            alternative = "two.sided")

#vcmax differs p<0.001
mean(basilaqua$V_cmax) #16.9
mean(basilsoil$V_cmax) #7.9

basil_ttest_jmax <- t.test(basilaqua$J_max, basilsoil$J_max, 
                           alternative = "two.sided")

#jmax differs p<0.001

mean(basilaqua$J_max) #39.5
mean(basilsoil$J_max) #16.7





