library(racir)

#read in week 1 data for romaine - includes 2 empty chamber runs, and 4 aci curves for each species
week4_data <- read.csv("raw_data/aci_raw_07032023.csv")
#temp correct to calvin, needed for curve fits later 
week4_data$T_leaf <- week4_data$Tleaf + 273

#split into two dataframes, soil & aqua with correct empty chamber 
# dataframe[row arguments, column arguments]
empty <- week4_data[week4_data$treatment=='empty',]
aqua <- week4_data[week4_data$treatment=='aqua',]
soil <- week4_data[week4_data$treatment=='soil',]

#create unique id for each plant (treatment + replicate)
aqua$uniqueid <- as.factor(with(aqua, paste(treatment, replicate, sep="-")))
soil$uniqueid <- as.factor(with(soil, paste(treatment, replicate, sep="-")))

##test and pick cutoffs for each treatments empty chamber
racircalcheck(data = empty)
racircalcheck(data = empty, mincut= 220, maxcut = 1205)

unique(aqua$uniqueid)
unique(soil$uniqueid)
#test
testa <- racircal(data = aqua[aqua$uniqueid == "aqua-1",], caldata = empty,
                       mincut = 220, maxcut = 1205)
tests <- racircal(data = soil[soil$uniqueid == "soil-3",], caldata = empty,
                  mincut = 220, maxcut = 1205)

#aqua20  are messed up (drop), soil 3 may be also

#Batch calibration with aqua
aqua_raw_list <- split(aqua, f = aqua$uniqueid)  
aquacurvenames <- names(aqua_raw_list)
#Batch calibration with normal algorithm
aqua_corr_list <- racircalbatch(caldata = empty, data = aqua_raw_list,
                        mincut = 220, maxcut = 1205, title = aquacurvenames)

#merge all data frames in list
aqua_corr <- Reduce(function(x, y) merge(x, y, all=TRUE), aqua_corr_list)

#Batch calibration with soil
soil_raw_list <- split(soil, f = soil$uniqueid)  
soilcurvenames <- names(soil_raw_list)
#Batch calibration with normal algorithm
soil_corr_list <- racircalbatch(caldata = empty, data = soil_raw_list,
                           mincut = 220, maxcut = 1205, title = soilcurvenames)

#merge all data frames in list
soil_corr <- Reduce(function(x, y) merge(x, y, all=TRUE), soil_corr_list)

##load package for plant physiology data
library(dplyr)
library(photosynthesis)
library(purrr)

##batch aci fits for aqua week 4---------
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

write.csv(aquafits_week4, file="aci_parameters/aquafits_week4.csv", row.names=FALSE)


##batch aci fits for soil week 4-------
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

write.csv(soilfits_week4, file="aci_parameters/soilfits_week4.csv", row.names=FALSE)


