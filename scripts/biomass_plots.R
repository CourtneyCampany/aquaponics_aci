source("scripts/functions.R")
source("scripts/plot_objects.R")

biomass <- read.csv("raw_data/biomass_aci.csv")
  biomass$totalbiomass <- with(biomass, shoots_mg + roots_mg)
  biomass$rs_ratio <- with(biomass, roots_mg/shoots_mg)
  biomass$uniqueid <- paste(biomass$species, biomass$treatment, sep="-")
  biomass$treatment <- as.factor(biomass$treatment)
  biomass$species <- as.factor(biomass$species)

#for poster, combined into 2 panel graph
jpeg(filename = "output/biomass.jpeg",
     width = 8, height = 10, units = "in", res= 500)

par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25, mfrow=c(2,1),  
    omi=c(.5,0,0.1,0.1))

#totalbiomass by species------
par(mar=c(0,5,1,1))
boxplot(biomass$shoots ~ uniqueid, data=biomass, xaxt='n',varwidth=TRUE,
        ylab="",border=trtcols,ylim=c(0,51),xlab="", outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
# axis(1, boxlabs2, at=1:6)
axis(2, "Edible Shoot Biomass  (g)" , at=25, tick=FALSE, line=2)
stripchart(biomass$shoots ~ uniqueid, data = biomass,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

text(1.5, 11, "*",cex=2.5, font=2)
text(3.5, 13, "*",cex=2.5, font=2)
text(5.5, 45, "*",cex=2.5, font=2)
text(.55, 50, "A", cex=1.51, font=2)

# RS ratio
par(mar=c(2,5,0,1))
boxplot(biomass$rs_ratio ~ uniqueid, data=biomass, xaxt='n',varwidth=TRUE,
        ylab="",border=trtcols,ylim=c(0,1.1),xlab="", outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
axis(1, boxlabs2, at=1:6)
axis(2, "Root:Shoot Ratio  (%)" , at=.5, tick=FALSE, line=2)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
stripchart(biomass$rs_ratio ~ uniqueid, data = biomass,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

text(1.5, .8, "*",cex=2.5, font=2)
# text(3.5, .6, "*",cex=2.5, font=2)
text(5.5, 1, "*",cex=2.5, font=2)
text(.55, 1.05, "B", cex=1.51, font=2)

dev.off()


