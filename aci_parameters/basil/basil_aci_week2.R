library(racir)

#read in week 1 data for romaine - includes 2 empty chamber runs, and 4 aci curves for each species
week2_data <- read.csv("aci_parameters/basil/basil_aci_master.csv")
#temp correct to calvin, needed for curve fits later 
week2_data$T_leaf <- week2_data$Tleaf + 273

basil_week2 <- week2_data[week2_data$species == "basil" & week2_data$week == 2, ]


#split into two dataframes, soil & aqua with correct empty chamber 
aqua <- basil_week2[basil_week2$treatment == "aqua",]
  aqua$uniqueid <- as.factor(with(aqua, paste(treatment, replicate, sep="-")))
soil <- basil_week2[basil_week2$treatment == "soil",,]
  soil$uniqueid <- as.factor(with(soil, paste(treatment, replicate, sep="-")))

#extract empty chamber run for each treatment
caltest_empty <- basil_week2[basil_week2$treatment == 'empty',]

##test and pick cutoffs for each treatments empty chamber
racircalcheck(data = caltest_empty, mincut=78,maxcut = 1200)


#extract and correct each curve from each treatment

#### AQUAPONICS TREATMENT
unique(aqua$uniqueid)
#corrected curve using aqua empty fix
aqua11_corr <- racircal(data = aqua[aqua$uniqueid =="aqua-11",], caldata = caltest_empty,
                       mincut = 78, maxcut = 1200)

aqua5_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-5",], caldata = caltest_empty,
                       mincut = 78, maxcut = 1200)

aqua13_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-13",], caldata = caltest_empty,
                        mincut = 78, maxcut = 1200)

aqua15_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-15",], caldata = caltest_empty,
                        mincut = 78, maxcut = 1200)

aqua17_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-17",], caldata = caltest_empty,
                        mincut = 78, maxcut = 1200)

#merge all corrected dataframes
#put all data frames into list
aqua_list <- list(aqua11_corr, aqua5_corr, aqua13_corr, aqua15_corr, aqua17_corr)

#merge all data frames in list
aqua_corr <- Reduce(function(x, y) merge(x, y, all=TRUE), aqua_list)

#### SOIL TREATMENT
#corrected curve using soil empty fix
unique(soil$uniqueid)

soil11_corr <- racircal(data = soil[soil$uniqueid == "soil-11",], caldata = caltest_empty,
                        mincut = 78, maxcut = 1200)

soil14_corr <- racircal(data = soil[soil$uniqueid == "soil-14",], caldata = caltest_empty,
                        mincut = 78, maxcut = 1200)

soil15_corr <- racircal(data = soil[soil$uniqueid == "soil-15",], caldata = caltest_empty,
                        mincut = 78, maxcut = 1200)

soil18_corr <- racircal(data = soil[soil$uniqueid == "soil-18",], caldata = caltest_empty,
                        mincut = 78, maxcut = 1200)


#merge all corrected dataframes
#put all data frames into list
soil_list <- list(soil11_corr, soil14_corr, soil15_corr, soil18_corr)

#merge all data frames in list
soil_corr <- Reduce(function(x, y) merge(x, y, all=TRUE), soil_list)

##load package for plant physiology data
library(dplyr)
library(photosynthesis)
library(purrr)

##batch aci fits for basial aqua week 2---------
aqua_corr2 <- aqua_corr %>% 
  rename(A_net = "Acor", C_i = "Cicor", PPFD = "Qin")

#remove empty chamber data
aqua_corr3 <- droplevels(aqua_corr2)

fits_aqua = aqua_corr3 |>
  split(~ uniqueid) |>
  map(fit_aci_response,  .progress=TRUE)

aquafits_week2 <- compile_data(fits_aqua,
                               output_type = "dataframe",
                               list_element = 1)

write.csv(aquafits_week2, file="aci_parameters/basil/basil_aquafits_week2.csv", row.names=FALSE)


##batch aci fits for soil week 1-------
soil_corr2 <- soil_corr %>% 
  rename(A_net = "Acor", C_i = "Cicor", PPFD = "Qin")

#remove empty chamber data
soil_corr3 <- droplevels(soil_corr2)

fits_soil = soil_corr3 |>
  split(~ uniqueid) |>
  map(fit_aci_response,  .progress=TRUE)

soilfits_week2 <- compile_data(fits_soil,
                               output_type = "dataframe",
                               list_element = 1)

write.csv(soilfits_week2, file="aci_parameters/basil/basil_soilfits_week2.csv", row.names=FALSE)
