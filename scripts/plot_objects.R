# plot objects

wp1 <- wesanderson::wes_palette("Darjeeling2") 
wp2 <- wesanderson::wes_palette("Cavalcanti1")

trtcols <- c(wp1[2], wp2[5])
trtcols2 <- scales::alpha(trtcols, .6)
plantcols <- c( "darkgoldenrod4","forestgreen")
boxcols <- c("white", "grey45")
boxcols2 <- scales::alpha(boxcols, .6)

pchs=c(15,16,17) #broc, pak, salanova

specieslabs <- c("Basil", "Lettuce" ,"Wheat Grass")

ratio_lab <- "Root to Shoot ratio"
biomasslab <- "Total Biomass (g)"
boxlabs <- c("Aquaponics", "Containerized")
satlab <- expression(italic(A)[sat]~~(mu*mol~m^-2~s^-1))
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
photolab <- expression(italic(A[n])~~(mu*mol~m^-2~s^-1))
itelab <- expression(ITE~~(mmol~CO[2]~mol~H[2]*O^-1))
denslab <- expression(paste("Stomatal Density  (# m", m^"-2",")"), sep="")
denslab2 <- expression(bold(paste("Stomatal Density  (# m", m^"-2",")"), sep=""))
sizelab <- expression(paste("Stomatal Size  ", mu, m^-2, sep=""))
treatmentlab <- c("Aqua", "Soil", "Aqua", "Soil", "Aqua", "Soil")
slalab<- expression(SLA~~(mm^2~g^-1))
chlalab<- expression(Chlorophyll~A~~(g~L^-1))
chlblab <- expression(Chlorophyll~B~~(g~L^-1))
chltotallab <- expression(Chlorophyll~A+B~~(g~L^-1))
vcmaxlab <- expression(Vc[max]~~(mu*mol~m^-2~s^-1))
jmaxlab <- expression(J[max]~~(mu*mol~m^-2~s^-1))

treelab <- c( "Shoots", "Roots")

boxlabs <- c("Aquaponics", "Soil")
boxlabs2 <- rep(boxlabs, 3)
boxlabs3 <- c("Aqua", "Soil")
boxlabs4 <- rep(boxlabs3, 3)

partlab <- c("shoots", "roots")

