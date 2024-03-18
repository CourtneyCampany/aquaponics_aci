library(racir)

#read in week 1 data for romaine - includes 2 empty chamber runs, and 4 aci curves for each species
week3_soil <- read.csv("raw_data/aci_raw_28022023_soil.csv")
#temp correct to calvin, needed for curve fits later 
week3_soil$T_leaf <- week3_soil$Tleaf + 273

#split into two dataframes, soil with correct empty chamber 
# dataframe[row arguments, column arguments]
empty <- week3_soil[week3_soil$treatment=='empty',]
soil <- week3_soil[week3_soil$treatment=='soil',]

#create unique id for each plant (treatment + replicate)
soil$uniqueid <- as.factor(with(soil, paste(treatment, replicate, sep="-")))

##test and pick cutoffs for each treatments empty chamber
racircalcheck(data = empty)
racircalcheck(data = empty, mincut= 55, maxcut = 1200)

unique(soil$uniqueid)
#test
test <- racircal(data = soil[soil$uniqueid == "soil-16",], caldata = empty,
                       mincut = 55, maxcut = 1205)


#Batch calibration with soil
soil_raw_list <- split(soil, f = soil$uniqueid)  
soilcurvenames <- names(soil_raw_list)
#Batch calibration with normal algorithm
soil_corr_list <- racircalbatch(caldata = empty, data = soil_raw_list,
                           mincut = 55, maxcut = 1205, title = soilcurvenames)

#merge all data frames in list
soil_corr <- Reduce(function(x, y) merge(x, y, all=TRUE), soil_corr_list)

##load package for plant physiology data
library(dplyr)
library(photosynthesis)
library(purrr)

##batch aci fits for soil week 2-------
soil_corr2 <- soil_corr %>% 
  rename(A_net = "Acor", C_i = "Cicor", PPFD = "Qin")

#remove empty chamber data
soil_corr3 <- droplevels(soil_corr2)

fits_soil = soil_corr3 |>
  split(~ uniqueid) |>
  map(fit_aci_response,  .progress=TRUE)

soilfits_week3 <- compile_data(fits_soil,
                               output_type = "dataframe",
                               list_element = 1)

write.csv(soilfits_week3, file="aci_parameters/soilfits_week3.csv", row.names=FALSE)





