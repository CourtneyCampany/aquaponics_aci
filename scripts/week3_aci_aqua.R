library(racir)

#read in week 1 data for romaine - includes 2 empty chamber runs, and 4 aci curves for each species
week3_aqua <- read.csv("raw_data/aci_raw_28022023_aqua.csv")
#temp correct to calvin, needed for curve fits later 
week3_aqua$T_leaf <- week3_aqua$Tleaf + 273

#split into two dataframes, soil & aqua with correct empty chamber 
# dataframe[row arguments, column arguments]
empty <- week3_aqua[week3_aqua$treatment=='empty',]
aqua <- week3_aqua[week3_aqua$treatment=='aqua',]

#create unique id for each plant (treatment + replicate)
aqua$uniqueid <- as.factor(with(aqua, paste(treatment, replicate, sep="-")))

##test and pick cutoffs for each treatments empty chamber
racircalcheck(data = empty)
racircalcheck(data = empty, mincut= 70, maxcut = 1200)

unique(aqua$uniqueid)
#test
test <- racircal(data = aqua[aqua$uniqueid == "aqua-14",], caldata = empty,
                       mincut = 70, maxcut = 1200)

#aqua3, 10 (a few ci corr that are so low they ruin curve, delete somehow)

#Batch calibration with aqua
aqua_raw_list <- split(aqua, f = aqua$uniqueid)  
aquacurvenames <- names(aqua_raw_list)
#Batch calibration with normal algorithm
aqua_corr_list <- racircalbatch(caldata = empty, data = aqua_raw_list,
                        mincut = 70, maxcut = 1200, title = aquacurvenames)

#merge all data frames in list
aqua_corr <- Reduce(function(x, y) merge(x, y, all=TRUE), aqua_corr_list)


##load package for plant physiology data
library(photosynthesis)

##batch aci fits for aqua week 2---------
aqua_ids <- orderc("aqua-3", "aqua-6", "aqua-12", "aqua-14", "aqua-19")

aquafits <- fit_many(data=aqua_corr, varnames=list(A_net = "Acor",T_leaf = "T_leaf",
                                                   C_i = "Cicor", PPFD = "Qin", 
                                                   g_mc = "g_mc"), func=fit_aci_response,
                     group="uniqueid")

aquafits_pars <- compile_data(aquafits,
                              output_type = "dataframe",
                              list_element = 1)
aquafits_pars$id <- aqua_ids

write.csv(aquafits_pars, file="aci_parameters/aquafits_week2.csv", row.names=FALSE)


##batch aci fits for soil week 2-------
soil_ids <- c("soil-2", "soil-4", "soil-7", "soil-9", "soil-19")

soilfits <- fit_many(data=soil_corr, varnames=list(A_net = "Acor",T_leaf = "T_leaf",
                                                   C_i = "Cicor", PPFD = "Qin", 
                                                   g_mc = "g_mc"), func=fit_aci_response,
                     group="uniqueid")

soilfits_pars <- compile_data(soilfits,
                              output_type = "dataframe",
                              list_element = 1)
soilfits_pars$id <- soil_ids

write.csv(soilfits_pars, file="aci_parameters/soilfits_week2.csv", row.names=FALSE)



