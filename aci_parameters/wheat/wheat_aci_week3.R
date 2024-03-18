library(racir)

#read in week 1 data for romaine - includes 2 empty chamber runs, and 4 aci curves for each species
wheat <- read.csv("aci_parameters/wheat/aci_wheat.csv")
#temp correct to calvin, needed for curve fits later 
wheat$T_leaf <- wheat$Tleaf + 273
wheat$uniqueid <- as.factor(with(wheat, paste(treatment, replicate, sep="-")))

wheat_week3 <- wheat[wheat$species == "wheat" & wheat$week == 3, ]

empty1 <- read.csv("aci_parameters/wheat/empty1.csv")
empty2 <- read.csv("aci_parameters/wheat/empty2.csv")

#split into two dataframes, soil & aqua with correct empty chamber 
aqua <- wheat_week3[wheat_week3$treatment == "aqua",]
soil <- wheat_week3[wheat_week3$treatment == "soil",]

##test and pick cutoffs for each treatments empty chamber
# racircalcheck(data = empty1) mincut=100,maxcut = 1200)
racircalcheck(data = empty2, mincut=70,maxcut = 1200)


#### AQUAPONICS TREATMENT by week
unique(aqua$uniqueid)
#corrected curve using aqua empty fix
aqua19_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-19",], caldata = empty2, 
                        mincut=70,maxcut = 1200)

aqua17_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-17",], caldata = empty2,
                        mincut=70,maxcut = 1200)

aqua15_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-15",], caldata = empty2,
                        mincut=70,maxcut = 1200)

aqua9_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-9",], caldata = empty2,
                        mincut=70,maxcut = 1200)


aqua7_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-7",], caldata = empty2,
                       mincut = 70, maxcut = 1200)

#merge all corrected dataframes
#put all data frames into list
aqua_list_week1 <- list(aqua19_corr, aqua17_corr, aqua15_corr, aqua9_corr, aqua7_corr)

#merge all data frames in list
aqua_corr_week1 <- Reduce(function(x, y) merge(x, y, all=TRUE), aqua_list_week1)

#### SOIL TREATMENT
#corrected curve using soil empty fix
unique(soil$uniqueid)

soil18_corr <- racircal(data = soil[soil$uniqueid == "soil-18",], caldata = empty2,
                       mincut = 105, maxcut = 1500)

soil17_corr <- racircal(data = soil[soil$uniqueid == "soil-17",], caldata = empty2,
                        mincut = 70, maxcut = 1500)

soil9_corr<- racircal(data = soil[soil$uniqueid == "soil-9",], caldata = empty2,
                      mincut = 70, maxcut = 1500)

soil2_corr <- racircal(data = soil[soil$uniqueid == "soil-2",], caldata = empty2,
                        mincut = 70, maxcut = 1500)

soil7_corr<- racircal(data = soil[soil$uniqueid == "soil-7",], caldata = empty2,
                       mincut = 70, maxcut = 1500)

#merge all corrected dataframes
#put all data frames into list
soil_list_week1 <- list(soil18_corr, soil17_corr, soil9_corr, soil2_corr,soil7_corr)

#merge all data frames in list
soil_corr_week1 <- Reduce(function(x, y) merge(x, y, all=TRUE), soil_list_week1)

##load package for plant physiology data
library(dplyr)
library(photosynthesis)
library(purrr)

##batch aci fits for aqua week 1---------
aqua_corr2 <- aqua_corr_week1 %>% 
  rename(A_net = "Acor", C_i = "Cicor", PPFD = "Qin")

#remove empty chamber data
aqua_corr3 <- droplevels(aqua_corr2)

fits_aqua = aqua_corr3 |>
  split(~ uniqueid) |>
  map(fit_aci_response,  .progress=TRUE)

aquafits_week3 <- compile_data(fits_aqua,
                               output_type = "dataframe",
                               list_element = 1)

write.csv(aquafits_week3, file="aci_parameters/wheat/wheat_aquafits_week3.csv", row.names=FALSE)


##batch aci fits for soil week 1-------
soil_corr2 <- soil_corr_week1 %>% 
  rename(A_net = "Acor", C_i = "Cicor", PPFD = "Qin")

#remove empty chamber data
soil_corr3 <- droplevels(soil_corr2)

fits_soil = soil_corr3 |>
  split(~ uniqueid) |>
  map(fit_aci_response,  .progress=TRUE)

soilfits_week3 <- compile_data(fits_soil,
                               output_type = "dataframe",
                               list_element = 1)

write.csv(soilfits_week3, file="aci_parameters/wheat/wheat_soilfits_week3.csv", row.names=FALSE)




