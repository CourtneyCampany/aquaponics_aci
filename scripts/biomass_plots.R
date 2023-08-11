source("scripts/plot_objects.R")

biomass <- read.csv("raw_data/biomassharvest_romaine.csv")
  biomass$totalbiomass <- with(biomass, shoots_g + roots_g)
  biomass$rs_ratio <- with(biomass, roots_g/shoots_g)
  biomass$treatment <- as.factor(biomass$treatment)


#biomass partitioning
mass_perc <- data.frame(treatment=as.factor(biomass$treatment), 
                        shoots_perc = with(biomass, shoots_g/totalbiomass),
                        roots_perc = with(biomass, roots_g/totalbiomass))

mass_perc_agg <- doBy::summaryBy(.~treatment, data=mass_perc, FUN = mean2)
mass_perc2 <- mass_perc_agg[,2:3]
i=c(2,1)


#for paper, combined into 2 panel graph
jpeg(filename = "output/biomassfigure.jpeg",
     width = 12, height = 6, units = "in", res= 500)
# windows(12,6)
par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25, mfrow=c(1,2),  
    omi=c(.5,0,0.1,0.1))

#totalbiomass by species------
# jpeg(filename = "output/totalbiomass.jpeg",
#        width = 8, height = 6, units = "in", res= 500)
par(mar=c(5,5,1,0))
boxplot(biomass$totalbiomass ~ treatment, data=biomass, xaxt='n',varwidth=TRUE,
        ylab="",border=trtcols,ylim=c(0,13),xlab="", outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
axis(1, boxlabs2, at=1:6)
axis(2, biomasslab , at=7.5, tick=FALSE, line=2)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
stripchart(biomass$totalbiomass ~ treatment, data = biomass,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 

text(1.5, 12, "*",cex=2.5, font=2)
# text(.55, 14.6, "(A)", cex=1.51)
# dev.off()

#biomasspartioning
# jpeg(filename = "output/shootroot.jpeg",
#      width = 8, height = 6, units = "in", res= 500)
par(mar = c(5, 5, 1, 7.3), xpd = TRUE)
barplot(t(as.matrix(mass_perc2))[i,], names.arg=boxlabs2, col=c("chocolate3", "forestgreen"),
        width=2, xlab= "", 
        ylab="Biomass Partitioning (%)", ylim=c(0, 1), xaxt = 'n')
box()
axis(1, boxlabs, at=c(1.4, 3.8))
# mtext(specieslabs, 1, line=2.25, at=c(2.6, 7.4, 12.2),  cex=1.25)
legend("topright", inset = c(-0.315, 0), fill = c("forestgreen", "chocolate3"), 
       legend=partlab, cex=1)
# text(x=1.3, .94, "(B)", cex=1.51)
dev.off()