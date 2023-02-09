library(racir)
data <- read_6800(system.file("extdata", "cal", package = "racir"))

#Read in the file
data <- read_6800(system.file("extdata", "cal", package = "racir"))
#Run calibration check
racircalcheck(data = data)


##check cutoffs
racircalcheck(data = data,
              mincut = 350,
              maxcut = 780)


caltest <- read.csv("raw_data/test_empty.csv")
racircalcheck(data = caltest)

#trim empty chamber
racircalcheck(data = caltest,
              mincut = 85,
              maxcut = 1000)

#test plant #1
plant1test <- read.csv("raw_data/plant1_test.csv")

#correct using previous trim
plant1_corr <- racircal(data = plant1test, caldata = caltest,
                           mincut = 85, maxcut = 1000)


# data_corrected <- racircal_advanced(data = plant1test, caldata = caltest,
#                                     mincut = 85, maxcut = 1000,
#                                     digits = -2)

##load packages for plant phs data (using photosynthesis - new pacakge)
library(photosynthesis)
library(plantecophys)

#correct temp to calvin
plant1_corr$T_leaf <- plant1_corr$Tleaf + 273

#run aci curve, note the varnames function to pick the correct (and corrected) variables
#takes about ~1 min to run
fit_aci_response(plant1_corr, varnames=list(A_net = "Acor",T_leaf = "T_leaf",
                                            C_i = "Cicor", PPFD = "Qin", g_mc =
                                              "g_mc"))
