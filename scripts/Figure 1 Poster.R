source("scripts/functions.R")
source("scripts/plot_objects.R")

biomass <- read.csv("raw_data/biomass_aci.csv")
  biomass$totalbiomass <- with(biomass, shoots_mg + roots_mg)
  biomass$uniqueid <- paste(biomass$species, biomass$treatment, sep="-")
  biomass$treatment <- as.factor(biomass$treatment)
  biomass$species <- as.factor(biomass$species)

#for poster, edible biomass
jpeg(filename = "output/ediblebiomass2.jpeg",
     width = 8.5, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), cex.lab=1.5, cex.axis=1.5,  
    omi=c(.5,0,0.1,0.1))

par(mar=c(5,5,1,1))
boxplot(biomass$shoots ~ uniqueid, data=biomass, xaxt='n',varwidth=TRUE,
        ylab="",border=trtcols,ylim=c(0,46),xlab="", outline=FALSE,
        boxlwd=3.5, whisklwd=3.5,staplelwd=3.5)
axis(2, "Edible Shoot Biomass  (g)" , at=25, tick=FALSE, line=2)
axis(1, boxlabs2[1:6], at=1:6)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.5)
# stripchart(biomass$shoots ~ uniqueid, data = biomass,
#            vertical = TRUE, method = "jitter",cex=1.25,
#            pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

text(1.5, 11, "*",cex=2.5, font=2)
text(3.5, 13, "*",cex=2.5, font=2)
text(5.5, 45, "*",cex=2.5, font=2)

dev.off()
