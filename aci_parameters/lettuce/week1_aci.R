library(racir)

#read in week 1 data for romaine - includes 2 empty chamber runs, and 4 aci curves for each species
week1_data <- read.csv("raw_data/aci_raw_14022023.csv")
  #temp correct to calvin, needed for curve fits later 
  week1_data$T_leaf <- week1_data$Tleaf + 273

#split into two dataframes, soil & aqua with correct empty chamber 
# dataframe[row arguments, column arguments]
aqua <- week1_data[1:1459,]
#create unique id for each plant (treatment + replicate)
  aqua$uniqueid <- as.factor(with(aqua, paste(treatment, replicate, sep="-")))
soil <- week1_data[1460:2951,]
  soil$uniqueid <- as.factor(with(soil, paste(treatment, replicate, sep="-")))
  
#extract empty chamber run for each treatment
caltest_aqua <- aqua[aqua$treatment == 'empty',]
caltest_soil <- soil[soil$treatment == 'empty',]

##test and pick cutoffs for each treatments empty chamber
racircalcheck(data = caltest_aqua, mincut=180,maxcut = 1205)
racircalcheck(data = caltest_soil, mincut=55,maxcut = 1205)

#extract and correct each curve from each treatment


#### AQUAPONICS TREATMENT
unique(aqua$uniqueid)
#corrected curve using aqua empty fix
aqua3_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-3",], caldata = caltest_aqua,
                        mincut = 180, maxcut = 1205)

aqua8_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-8",], caldata = caltest_aqua,
                       mincut = 180, maxcut = 1205)

aqua13_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-13",], caldata = caltest_aqua,
                       mincut = 180, maxcut = 1205)

aqua17_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-17",], caldata = caltest_aqua,
                       mincut = 180, maxcut = 1205)

aqua19_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-19",], caldata = caltest_aqua,
                       mincut = 180, maxcut = 1205)

#merge all corrected dataframes
#put all data frames into list
aqua_list <- list(aqua3_corr, aqua8_corr, aqua13_corr, aqua17_corr, aqua19_corr)

#merge all data frames in list
aqua_corr <- Reduce(function(x, y) merge(x, y, all=TRUE), aqua_list)

#### SOIL TREATMENT
#corrected curve using soil empty fix
unique(soil$uniqueid)

soil5_corr <- racircal(data = soil[soil$uniqueid == "soil-5",], caldata = caltest_aqua,
                       mincut = 55, maxcut = 1205)

soil3_corr <- racircal(data = soil[soil$uniqueid == "soil-3",], caldata = caltest_aqua,
                       mincut = 55, maxcut = 1205)

soil9_corr <- racircal(data = soil[soil$uniqueid == "soil-9",], caldata = caltest_aqua,
                        mincut = 55, maxcut = 1205)

soil10_corr <- racircal(data = soil[soil$uniqueid == "soil-10",], caldata = caltest_aqua,
                        mincut = 200, maxcut = 1205)

soil20_corr <- racircal(data = soil[soil$uniqueid == "soil-20",], caldata = caltest_aqua,
                        mincut = 55, maxcut = 1205)

##soil 5 is probably bad

#merge all corrected dataframes
#put all data frames into list
soil_list <- list(soil5_corr, soil3_corr, soil9_corr, soil10_corr, soil20_corr)

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

write.csv(aquafits_week1, file="aci_parameters/aquafits_week1.csv", row.names=FALSE)


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

