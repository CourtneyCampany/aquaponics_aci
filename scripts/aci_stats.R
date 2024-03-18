source("scripts/functions.R")
aci <- read.csv("aci_parameters/wheat/wheat_aci_params.csv")

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

#all aci variables and R:S differ

#lettuce stats----------
lettuceaqua <- aci[aci$species == "lettuce" & aci$treatment == "a",]
lettucesoil <- aci[aci$species == "lettuce" & aci$treatment == "s",]

lettuce_ttest <- t.test(lettuceaqua$totalaci, lettucesoil$totalaci, 
                        alternative = "two.sided")

lettuce_ttest_roots <- t.test(lettuceaqua$roots_mg, lettucesoil$roots_mg, 
                              alternative = "two.sided")

lettuce_ttest_above <- t.test(lettuceaqua$shoots_mg, lettucesoil$shoots_mg, 
                              alternative = "two.sided")

lettuce_ttest_ratio <- t.test(lettuceaqua$rs_ratio, lettucesoil$rs_ratio, 
                              alternative = "two.sided")

##all aci variables are different, but root to shoot ratio does not change


#basil statss ----------
basilaqua <- aci[aci$species == "basil" & aci$treatment == "a",]
basilsoil <- aci[aci$species == "basil" & aci$treatment == "s",]

basil_ttest <- t.test(basilaqua$totalaci, basilsoil$totalaci, 
                    alternative = "two.sided")

basil_ttest_roots <- t.test(basilaqua$roots_mg, basilsoil$roots_mg, 
                          alternative = "two.sided")

basil_ttest_above <- t.test(basilaqua$shoots_mg, basilsoil$shoots_mg, 
                          alternative = "two.sided")

basil_ttest_ratio <- t.test(basilaqua$rs_ratio, basilsoil$rs_ratio, 
                          alternative = "two.sided")

##total, above and R:D differ, roots do not




