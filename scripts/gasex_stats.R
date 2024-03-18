source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$ITE <- with(gasex, A/(E*1000))
  gasex$uniqueid <- paste(gasex$species, gasex$treatment, sep="-")
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$species <- as.factor(gasex$species)

#lettuce stats----------
  lettuceaqua <- gasex[gasex$species == "lettuce" & gasex$treatment == "aqua",]
  lettucesoil <- gasex[gasex$species == "lettuce" & gasex$treatment == "soil",]
  
  lettuce_ttest_photo <- t.test(lettuceaqua$A, lettucesoil$A, 
                          alternative = "two.sided")
  
  lettuce_ttest_gsw <- t.test(lettuceaqua$gsw, lettucesoil$gsw, 
                                alternative = "two.sided")
  
  lettuce_ttest_ITE <- t.test(lettuceaqua$ITE, lettucesoil$ITE, 
                                alternative = "two.sided")

##Photo and gsw differ, not ITE
  
  
#basil statss ----------
basilaqua <- gasex[gasex$species == "basil" & gasex$treatment == "aqua",]
basilsoil <- gasex[gasex$species == "basil" & gasex$treatment == "soil",]
  
  basil_ttest_photo <- t.test(basilaqua$A, basilsoil$A, 
                        alternative = "two.sided")
  
  basil_ttest_gsw <- t.test(basilaqua$gsw, basilsoil$gsw, 
                              alternative = "two.sided")
  
  basil_ttest_ITE <- t.test(basilaqua$ITE, basilsoil$ITE, 
                              alternative = "two.sided")

  
##photo and ITE differ, not gsw
  
  
#wheat stats and graphs ----------
wheataqua <- gasex[gasex$species == "wheat" & gasex$treatment == "aqua",]
wheatsoil <- gasex[gasex$species == "wheat" & gasex$treatment == "soil",]
  
  wheat_ttest_photo <- t.test(wheataqua$A, wheatsoil$A, 
                        alternative = "two.sided")
  
  wheat_ttest_gsw <- t.test(wheataqua$gsw, wheatsoil$gsw, 
                              alternative = "two.sided")
  
  wheat_ttest_ITE <- t.test(wheataqua$ITE, wheatsoil$ITE, 
                              alternative = "two.sided")

#photo differs, gsw = marginal, ITE = same
  
  