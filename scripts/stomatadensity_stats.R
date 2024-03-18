source("scripts/functions.R")

sd <- read.csv("raw_data/stomataldensity_master.csv")
sd$date <- as.Date(sd$date, format = "%m/%d/%Y")
sd$stomatacount <- with(sd, (c1+c2+c3)/3)
#calculate density from FOV diameter
sd$sd_mm2 <- with(sd, (stomatacount/(3.14 * (fov_mm/2)^2)))
sd$uniqueid <- paste(sd$species, sd$treatment, sep="-")
sd$treatment <- as.factor(sd$treatment)
sd$species <- as.factor(sd$species)

#lettuce stats----------
lettuceaqua <- sd[sd$species == "lettuce" & sd$treatment == "aqua",]
lettucesoil <- sd[sd$species == "lettuce" & sd$treatment == "soil",]

lettuce_ttest_sd <- t.test(lettuceaqua$sd_mm2, lettucesoil$sd_mm2, 
                              alternative = "two.sided")
##aqua and soil differ (p < 0.001)

#basil statss ----------
basilaqua <- sd[sd$species == "basil" & sd$treatment == "aqua",]
basilsoil <- sd[sd$species == "basil" & sd$treatment == "soil",]

basil_ttest_sd <- t.test(basilaqua$sd_mm2, basilsoil$sd_mm2, 
                            alternative = "two.sided")

##aqua and soil differ (p < 0.001)

#wheat stats and graphs ----------
wheataqua <- sd[sd$species == "wheat" & sd$treatment == "aqua",]
wheatsoil <- sd[sd$species == "wheat" & sd$treatment == "soil",]

wheat_ttest_photo <- t.test(wheataqua$sd_mm2, wheatsoil$sd_mm2, 
                            alternative = "two.sided")