library(racir)

##I coded 'replicate' for each aqua or soil as 'empty'. Separate accordingly below

##raw data needs to be checked to align correct empty chamber across multiple files

#read in week 1 data for romaine - includes 2 empty chamber runs, and 4 aci curves for each species
week1_data <- read.csv("aci_parameters/basil/basil_aci_master.csv")
  #temp correct to calvin, needed for curve fits later 
  week1_data$T_leaf <- week1_data$Tleaf + 273
  
basil_week1 <- week1_data[week1_data$species == "basil" & week1_data$week == 1, ]


#split into two dataframes, soil & aqua with correct empty chamber 
aqua <- basil_week1[basil_week1$treatment == "aqua",]
  aqua$uniqueid <- as.factor(with(aqua, paste(treatment, replicate, sep="-")))
soil <- basil_week1[basil_week1$treatment == "soil",,]
  soil$uniqueid <- as.factor(with(soil, paste(treatment, replicate, sep="-")))
  
#extract empty chamber run for each treatment
caltest_aqua <- aqua[aqua$replicate == 'empty',]
caltest_soil <- soil[soil$replicate == 'empty',]

##test and pick cutoffs for each treatments empty chamber
racircalcheck(data = caltest_aqua, mincut=10,maxcut = 1200)
racircalcheck(data = caltest_soil, mincut=10,maxcut = 1500)

#extract and correct each curve from each treatment


#### AQUAPONICS TREATMENT
unique(aqua$uniqueid)
#corrected curve using aqua empty fix
aqua3_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-3",], caltest_aqua, 
                       mincut=10,maxcut = 1200)

aqua7_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-7",], caldata = caltest_aqua,
                       mincut=10,maxcut = 1200)

##gets soil empty chamber correction
aqua11_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-11",], caldata = caltest_soil,
                        mincut=10,maxcut = 1380)

##gets soil empty chamber correction
aqua20_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-20",], caldata = caltest_soil,
                        mincut=10,maxcut = 1150)

##gets soil empty chamber correction
aqua9_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-9",], caldata = caltest_soil,
                       mincut = 10, maxcut = 1500)

##gets soil empty chamber correction
aqua13_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-13",], caldata = caltest_aqua,
                        mincut = 10, maxcut = 1500)

#merge all corrected dataframes
#put all data frames into list
aqua_list <- list(aqua3_corr, aqua7_corr, aqua11_corr, aqua20_corr, aqua9_corr, aqua13_corr)

#merge all data frames in list
aqua_corr <- Reduce(function(x, y) merge(x, y, all=TRUE), aqua_list)

#### SOIL TREATMENT
#corrected curve using soil empty fix
unique(soil$uniqueid)

soil18_corr <- racircal(data = soil[soil$uniqueid == "soil-18",], caldata = caltest_aqua,
                       mincut = 10, maxcut = 1500)

soil9_corr <- racircal(data = soil[soil$uniqueid == "soil-9",], caldata = caltest_aqua,
                       mincut = 10, maxcut = 1500)

soil6_corr <- racircal(data = soil[soil$uniqueid == "soil-6",], caldata = caltest_aqua,
                        mincut = 10, maxcut = 1500)

soil11_corr <- racircal(data = soil[soil$uniqueid == "soil-11",], caldata = caltest_aqua,
                        mincut = 10, maxcut = 1500)

# soil3_corr <- racircal(data = soil[soil$uniqueid == "soil-3",], caldata = caltest_aqua,
#                         mincut = 250, maxcut = 1500)

soil16_corr <- racircal(data = soil[soil$uniqueid == "soil-16",], caldata = caltest_aqua,
                       mincut = 10, maxcut = 1500)

#merge all corrected dataframes
#put all data frames into list
soil_list <- list(soil18_corr, soil9_corr, soil6_corr, soil11_corr,soil16_corr)

#merge all data frames in list
soil_corr <- Reduce(function(x, y) merge(x, y, all=TRUE), soil_list)

##load package for plant physiology data
library(dplyr)
library(photosynthesis)
library(purrr)

##batch aci fits for aqua week 1---------
aqua_corr2 <- aqua_corr %>% 
              rename(A_net = "Acor", C_i = "Cicor", PPFD = "Qin")

#remove empty chamber data
aqua_corr3 <- droplevels(aqua_corr2)

fits_aqua = aqua_corr3 |>
  split(~ uniqueid) |>
  map(fit_aci_response,  .progress=TRUE)

aquafits_week1 <- compile_data(fits_aqua,
                        output_type = "dataframe",
                         list_element = 1)

write.csv(aquafits_week1, file="aci_parameters/basil/basil_aquafits_week1.csv", row.names=FALSE)


##batch aci fits for soil week 1-------
soil_corr2 <- soil_corr %>% 
  rename(A_net = "Acor", C_i = "Cicor", PPFD = "Qin")

#remove empty chamber data
soil_corr3 <- droplevels(soil_corr2)

fits_soil = soil_corr3 |>
  split(~ uniqueid) |>
  map(fit_aci_response,  .progress=TRUE)

soilfits_week1 <- compile_data(fits_soil,
                              output_type = "dataframe",
                              list_element = 1)

write.csv(soilfits_week1, file="aci_parameters/soilfits_week1.csv", row.names=FALSE)

