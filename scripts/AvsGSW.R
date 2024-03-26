#bivariate relationships between traits


library(emmeans)
library(car)
library(lme4)
library(MuMIn)

source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$ITE <- with(gasex, A/(E*1000))
  gasex$uniqueid <- paste(gasex$species, gasex$treatment, sep="-")
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$species <- as.factor(gasex$species)

###models ------
ags_mod <- lmer(A ~ gsw * treatment + (1|species), data=gasex)
ags_mod_simp_aqua <- lm(A ~ gsw, data=gasex[gasex$treatment=="aqua",])
ags_mod_simp_soil <- lm(A ~ gsw, data=gasex[gasex$treatment=="soil",])

##ploting-------
jpeg(filename = "output/photocond.jpeg",
     width = 8.5, height = 6, units = "in", res= 300)

par(mgp=c(2.5,.75,0), cex.lab=1.5, cex.axis=1.5,  
    omi=c(.5,0,0.1,0.1))
#photo vs gs
par(mar=c(5,5,1,1))
plot(A ~ gsw, data=gasex, ylab=photolab, xlab=condlab, ylim=c(0,30),
     xlim=c(0,1), type='n')
predline(ags_mod_simp_aqua, col=trtcols[1],lwd=2, lty=2)
predline(ags_mod_simp_soil, col=trtcols[2],lwd=2, lty=2)
# points(asat ~ gs, data=sela2, col=trtcols[habitat], pch=16, cex=1.5)
points(A ~ gsw, data=gasex, bg=trtcols2[treatment], pch=pchs[species], cex=1.5)
legend("topright",pt.bg=trtcols,pch=21,legend=c("Aqua", "Soil"),inset=.01,  bty='n',
       cex=1.5)
legend("bottomright",pt.bg="grey",pch=pchs,legend=specieslabs,inset=.01,  bty='n',
       cex=1.5)

dev.off()

