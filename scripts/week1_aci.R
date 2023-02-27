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
racircalcheck(data = caltest_aqua, mincut=200,maxcut = 1205)
racircalcheck(data = caltest_soil, mincut=55,maxcut = 1205)

#extract and correct each curve from each treatment


#### AQUAPONICS TREATMENT
unique(aqua$uniqueid)
#corrected curve using aqua empty fix
aqua3_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-3",], caldata = caltest_aqua,
                        mincut = 200, maxcut = 1205)

aqua8_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-8",], caldata = caltest_aqua,
                       mincut = 200, maxcut = 1205)

aqua13_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-13",], caldata = caltest_aqua,
                       mincut = 200, maxcut = 1205)

aqua17_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-17",], caldata = caltest_aqua,
                       mincut = 200, maxcut = 1205)

aqua19_corr <- racircal(data = aqua[aqua$uniqueid == "aqua-19",], caldata = caltest_aqua,
                       mincut = 200, maxcut = 1205)

#merge all corrected dataframes
#put all data frames into list
aqua_list <- list(aqua3_corr, aqua8_corr, aqua13_corr, aqua17_corr, aqua19_corr)

#merge all data frames in list
aqua_corr <- Reduce(function(x, y) merge(x, y, all=TRUE), aqua_list)

#### SOIL TREATMENT
#corrected curve using soil empty fix
unique(soil$uniqueid)

soil5_corr <- racircal(data = soil[soil$uniqueid == "soil-5",], caldata = caltest_aqua,
                       mincut = 200, maxcut = 1205)

soil3_corr <- racircal(data = soil[soil$uniqueid == "soil-3",], caldata = caltest_aqua,
                       mincut = 200, maxcut = 1205)

soil9_corr <- racircal(data = soil[soil$uniqueid == "soil-9",], caldata = caltest_aqua,
                        mincut = 200, maxcut = 1205)

soil10_corr <- racircal(data = soil[soil$uniqueid == "soil-10",], caldata = caltest_aqua,
                        mincut = 200, maxcut = 1205)

soil20_corr <- racircal(data = soil[soil$uniqueid == "soil-20",], caldata = caltest_aqua,
                        mincut = 200, maxcut = 1205)

#merge all corrected dataframes
#put all data frames into list
soil_list <- list(soil5_corr, soil3_corr, soil9_corr, soil10_corr, soil20_corr)

#merge all data frames in list
soil_corr <- Reduce(function(x, y) merge(x, y, all=TRUE), soil_list)

##load package for plant physiology data
library(photosynthesis)

##batch aci fits for aqua week 1---------
aqua_ids <- c("aqua-3", "aqua-8", "aqua-13", "aqua-17", "aqua-19")

aquafits <- fit_many(data=aqua_corr, varnames=list(A_net = "Acor",T_leaf = "T_leaf",
                                                   C_i = "Cicor", PPFD = "Qin", 
                                                   g_mc = "g_mc"), func=fit_aci_response,
                                                  group="uniqueid")

aquafits_pars <- compile_data(aquafits,
                          output_type = "dataframe",
                          list_element = 1)
aquafits_pars$id <- aqua_ids

write.csv(aquafits_pars, file="aci_parameters/aquafits_week1.csv", row.names=FALSE)


##batch aci fits for soil week 1-------
soil_ids <- c("soil-5", "soil-3", "soil-9", "soil-10", "soil-20")

soilfits <- fit_many(data=soil_corr, varnames=list(A_net = "Acor",T_leaf = "T_leaf",
                                                   C_i = "Cicor", PPFD = "Qin", 
                                                   g_mc = "g_mc"), func=fit_aci_response,
                                                   group="uniqueid")

soilfits_pars <- compile_data(soilfits,
                              output_type = "dataframe",
                              list_element = 1)
soilfits_pars$id <- soil_ids

write.csv(soilfits_pars, file="aci_parameters/soilfits_week1.csv", row.names=FALSE)

###not used below (yet)--------
#run aci curve, note the varnames function to pick the correct (and corrected) variables
#takes about a few min to run
fit_aci_response(aqua3_corr, varnames=list(A_net = "Acor",T_leaf = "T_leaf",
                                            C_i = "Cicor", PPFD = "Qin", g_mc = "g_mc"))

fit_aci_response(soil5_corr, varnames=list(A_net = "Acor",T_leaf = "T_leaf",
                                           C_i = "Cicor", PPFD = "Qin", g_mc = "g_mc"))


##batch calibration for aqua
#Create a list of files
files <- c(system.file("extdata", "poplar_1", package = "racir"),
           system.file("extdata", "poplar_2", package = "racir"))
data <- vector("list", length(files))
for(i in seq_along(files)){
  data[[i]] <- read_6800(files[i])
  names(data)[i] <- files[i]
}

caldata <- read_6800(system.file("extdata", "cal", package = "racir"))

#Batch calibration with normal algorithm
output <- racircalbatch(caldata = caldata, data = data,
                        mincut = 350, maxcut = 780, title = files)
                                            