library(racir)

#read in week 1 data for romaine - includes 2 empty chamber runs, and 4 aci curves for each species
week4_data <- read.csv("aci_parameters/basil/basil_aci_master.csv")
#temp correct to calvin, needed for curve fits later 
week4_data$T_leaf <- week4_data$Tleaf + 273

basil_week4 <- week4_data[week4_data$species == "basil" & week4_data$week == 4, ]

#split into two data frames, soil & aqua 
aqua <- basil_week4[basil_week4$treatment == "aqua",]
  aqua$uniqueid <- as.factor(with(aqua, paste(treatment, replicate, sep="-")))
soil <- basil_week4[basil_week4$treatment == "soil",,]
  soil$uniqueid <- as.factor(with(soil, paste(treatment, replicate, sep="-")))

#extract empty chamber run for both treatments
caltest_empty <- basil_week4[basil_week4$treatment == 'empty',]

##test and pick cutoffs for each treatments empty chamber
racircalcheck(data = caltest_empty, mincut=295,maxcut = 1200)

##another empty with less trim
caltest_empty2 <- week4_data[week4_data$treatment == 'empty' & week4_data$week == 3,]

#extract and correct each curve from each treatment

#### AQUAPONICS TREATMENT
unique(aqua$uniqueid)
#corrected curve using aqua empty fix
aqua7_corr <- racircal(data = aqua[aqua$uniqueid =="aqua-7",], caldata = caltest_empty,
                       mincut = 295, maxcut = 1200)

aqua5_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-5",], caldata = caltest_empty,
                        mincut = 295, maxcut = 1200)

aqua14_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-14",], caldata = caltest_empty2,
                       mincut = 105, maxcut = 1200)

aqua18_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-18",], caldata = caltest_empty,
                        mincut = 295, maxcut = 1200)

aqua19_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-19",], caldata = caltest_empty2,
                        mincut = 105, maxcut = 1200)

#merge all corrected dataframes
#put all data frames into list
aqua_list <- list(aqua7_corr, aqua5_corr, aqua14_corr, aqua18_corr, aqua19_corr)

#merge all data frames in list
aqua_corr <- Reduce(function(x, y) merge(x, y, all=TRUE), aqua_list)

#### SOIL TREATMENT
#corrected curve using soil empty fix
unique(soil$uniqueid)

soil11_corr <- racircal(data = soil[soil$uniqueid == "soil-11",], caldata = caltest_empty2,
                        mincut = 105, maxcut = 1200)

soil15_corr <- racircal(data = soil[soil$uniqueid == "soil-15",], caldata = caltest_empty2,
                        mincut = 105, maxcut = 1200)

soil19_corr <- racircal(data = soil[soil$uniqueid == "soil-19",], caldata = caltest_empty2,
                        mincut = 105, maxcut = 1200)

soil8_corr <- racircal(data = soil[soil$uniqueid == "soil-8",], caldata = caltest_empty,
                       mincut = 295, maxcut = 1200)

soil20_corr <- racircal(data = soil[soil$uniqueid == "soil-20",], caldata = caltest_empty2,
                       mincut = 105, maxcut = 1200)

#merge all corrected dataframes
#put all data frames into list
soil_list <- list(soil11_corr, soil15_corr, soil19_corr, soil8_corr, soil20_corr)

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

aquafits_week4 <- compile_data(fits_aqua,
                               output_type = "dataframe",
                               list_element = 1)

write.csv(aquafits_week4, file="aci_parameters/basil/basil_aquafits_week4.csv", row.names=FALSE)


##batch aci fits for soil week 1-------
soil_corr2 <- soil_corr %>% 
  rename(A_net = "Acor", C_i = "Cicor", PPFD = "Qin")

#remove empty chamber data
soil_corr3 <- droplevels(soil_corr2)

fits_soil = soil_corr3 |>
  split(~ uniqueid) |>
  map(fit_aci_response,  .progress=TRUE)

soilfits_week4 <- compile_data(fits_soil,
                               output_type = "dataframe",
                               list_element = 1)

write.csv(soilfits_week4, file="aci_parameters/basil/basil_soilfits_week4.csv", row.names=FALSE)
