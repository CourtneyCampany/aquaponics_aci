source("scripts/functions.R")
source("scripts/plot_objects.R")

gasex <- read.csv("raw_data/gasexchange_master.csv")
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$ITE <- with(gasex, A/(E*1000))
  gasex$uniqueid <- paste(gasex$species, gasex$treatment, sep="-")
  gasex$treatment <- as.factor(gasex$treatment)
  gasex$species <- as.factor(gasex$species)

### 2 panel physiology graph --------
jpeg(filename = "output/gasexchange.jpeg",
     width = 8, height = 10, units = "in", res= 500)
par(mfrow=c(2,1),mgp=c(2.5,.75,0), cex.lab=1.25,  cex.axis = 1.25)

#photosynthesis
par(mar=c(0,5,1,1))
boxplot(A ~ uniqueid, data=gasex, xaxt='n',varwidth=TRUE,
        ylab=photolab,border=trtcols,ylim=c(0,25),xlab="", outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
# axis(2, photolab , at=25, tick=FALSE, line=2)
stripchart(A ~ uniqueid, data = gasex,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

axis(1, at=1:2, labels=FALSE)

text(1.5, 11, "*",cex=2.5, font=2)
text(3.5, 23, "*",cex=2.5, font=2)
text(5.5, 25, "*",cex=2.5, font=2)
text(.55, 25, "A", cex=1.51, font=2)


#stomatal conductance
par(mar=c(5,5,0,1))
boxplot(gsw ~ uniqueid, data=gasex, xaxt='n',varwidth=TRUE,
        ylab=condlab,border=trtcols,ylim=c(0,1.0),xlab="",outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
stripchart(gsw ~ uniqueid, data = gasex,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
axis(1, boxlabs2, at=1:6)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)

# text(1.5, .3, "*",cex=2.5, font=2)
text(3.5, .9, "*",cex=2.5, font=2)
#text(5.5, .65, "*",cex=2.5, font=2)
text(.55, 1, "B", cex=1.51, font=2)

dev.off()
