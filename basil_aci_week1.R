library(racir)

#read in week 1 data for romaine - includes 2 empty chamber runs, and 4 aci curves for each species
week1_data <- read.csv("raw_data/basil_aci_week1_racir.csv")
#temp correct to calvin, needed for curve fits later 
week1_data$T_leaf <- week1_data$Tleaf + 273

#split into two dataframes, soil & aqua with correct empty chamber 
# dataframe[row arguments, column arguments]
empty <- week1_data[week1_data$treatment=='empty',]
aqua <- week1_data[week1_data$treatment=='aqua',]
soil <- week1_data[week1_data$treatment=='container',]

#create unique id for each plant (treatment + replicate)
aqua$uniqueid <- as.factor(with(aqua, paste(treatment, replicate, sep="-")))
soil$uniqueid <- as.factor(with(soil, paste(treatment, replicate, sep="-")))

racircalcheck(data = empty)
racircalcheck(data = empty, mincut= 70, maxcut = 1200)

racircalcheck(data = soil, mincut= 55, maxcut=1205)

unique(aqua$uniqueid)
#soil - only one
soil18 <- racircal(data = soil[soil$uniqueid == "container-18",], caldata = empty,
                 mincut = 70, maxcut = 1200)
##yes

#aqua checks
racircalcheck(data = aqua[aqua$uniqueid == "aqua-3",])

aqua3 <- racircal(data = aqua[aqua$uniqueid == "aqua-3",], caldata=empty,
                       mincut = 70, maxcut = 1200)

racircalcheck(data = aqua[aqua$uniqueid == "aqua-7",])

aqua7 <- racircal(data = aqua[aqua$uniqueid == "aqua-7",], caldata=empty,
                  mincut = 70, maxcut = 1200)

#Batch calibration with aqua
aqua_raw_list <- split(aqua, f = aqua$uniqueid)  
aquacurvenames <- names(aqua_raw_list)
#Batch calibration with normal algorithm
aqua_corr_list <- racircalbatch(caldata = empty, data = aqua_raw_list,
                                mincut = 70, maxcut = 1200, title = aquacurvenames)

#merge all data frames in list
aqua_corr <- Reduce(function(x, y) merge(x, y, all=TRUE), aqua_corr_list)