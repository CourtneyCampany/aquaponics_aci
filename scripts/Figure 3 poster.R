source("scripts/functions.R")
source("scripts/plot_objects.R")

wheat_aci <- read.csv("aci_parameters/wheat/wheat_aci_params.csv")
wheat_aci$species <- "wheat"

lettuce_aci <- read.csv("aci_parameters/lettuce/lettuce_aci_params.csv")
lettuce_aci$species <- "lettuce"

basil_aci <- read.csv("aci_parameters/basil/basil_aci_params.csv")
basil_aci$species <- "basil"

aci_list <- list(wheat_aci, lettuce_aci, basil_aci)
aci_params <- Reduce(function(x, y) merge(x, y, all=TRUE), aci_list)
  aci_params$uniqueid <- paste(aci_params$species, aci_params$treatment, sep="-")
  aci_params$treatment <- as.factor(aci_params$treatment)
  aci_params$species <- as.factor(aci_params$species)
  
  
##chlorophyll data
source("scripts/plot_objects.R")
  
chloro <- read.csv("raw_data/chlorophyll_master.csv")
  chloro$treatment <- as.factor(chloro$treatment)
  chloro$chla <- 12.7*chloro$abs_664-2.79*chloro$abs_647
  chloro$chlb <- 20.7*chloro$abs_647+8.08*chloro$abs_664
  chloro$chlAB <- with(chloro, chla/chlb)
  chloro$chl_total <- with(chloro, chla+chlb)
  chloro$uniqueid <- paste(chloro$species, chloro$treatment, sep="-")

### 2 panel physiology graph --------
jpeg(filename = "output/jmaxchloro.jpeg",
     width = 8.5, height = 10, units = "in", res= 500)
par(mfrow=c(2,1),mgp=c(2.5,.75,0), cex.lab=1.5,  cex.axis = 1.5)

#jmax conductance
par(mar=c(0,5,1,1))
boxplot(J_max ~ uniqueid, data=aci_params, xaxt='n',varwidth=TRUE,
        ylab=jmaxlab,border=trtcols,ylim=c(0,250),xlab="",outline=FALSE,
        boxlwd=3.5, whisklwd=3.5,staplelwd=3.5)
# stripchart(J_max ~ uniqueid, data = aci_params,
#            vertical = TRUE, method = "jitter",cex=1.5,
#            pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

text(1.5, 100, "*",cex=2.5, font=2)
text(3.5, 160, "*",cex=2.5, font=2)
text(5.5, 248, "*",cex=2.5, font=2)
text(.55, 250, "A", cex=1.51, font=2)
axis(1, at=1:6, labels=FALSE)

par(mar=c(5,5,0,1))
boxplot(chloro$chl_total ~ uniqueid, data=chloro, xaxt='n',varwidth=TRUE,
        ylab=chltotallab,border=trtcols,ylim=c(0,50),xlab="", outline=FALSE,
        boxlwd=3.5, whisklwd=3.5,staplelwd=3.5)
axis(1, boxlabs2, at=1:6)
# stripchart(chloro$chl_total ~ uniqueid, data = chloro,
#            vertical = TRUE, method = "jitter",cex=1.5,
#            pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.5)
text(1.5, 41, "*",cex=2.5, font=2)
text(3.5, 28, "*",cex=2.5, font=2)
text(5.5, 35, "*",cex=2.5, font=2)
text(0.5, 50, "B", cex=1.51, font=2)

dev.off()
