source("scripts/plot_objects.R")

chloro <- read.csv("raw_data/chlorophyll_romaine.csv")
  chloro$treatment <- as.factor(chloro$treatment)
  chloro$chlAB <- with(chloro, chlA/chlB)
  chloro$chl_total <- with(chloro, chlA+chlB)
  # chloro$chlAmass <- with(chloro, chlA/weight_mg)
  # chloro$chlBmass <- with(chloro, chlB/weight_mg)
  # chloro$chlAarea <- with(chloro, chlA/(punches * punch_diam_mm2))
  # chloro$chlBarea <- with(chloro, chlB/(punches * punch_diam_mm2))
  
#CHL A boxplot------
jpeg(filename = "output/chlA.jpeg",
       width = 8, height = 6, units = "in", res= 500)
par(mar=c(5,5,1,0))
boxplot(chloro$chlA ~ treatment, data=chloro, xaxt='n',varwidth=TRUE,
          ylab="",border=trtcols,ylim=c(0,10),xlab="", outline=FALSE,
          boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
axis(1, boxlabs2, at=1:6)
axis(2, chlalab , at=5, tick=FALSE, line=2)
stripchart(chloro$chlA ~ treatment, data = chloro,
             vertical = TRUE, method = "jitter",cex=1.25,
             pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
  
# text(1.5, 1.5, "*",cex=2.5, font=2)
# text(.55, 14.6, "(A)", cex=1.51)
dev.off()

#CHL B boxplot------
jpeg(filename = "output/chlB.jpeg",
     width = 8, height = 6, units = "in", res= 500)
par(mar=c(5,5,1,0))
boxplot(chloro$chlB ~ treatment, data=chloro, xaxt='n',varwidth=TRUE,
        ylab="",border=trtcols,ylim=c(0,15),xlab="", outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
axis(1, boxlabs2, at=1:6)
axis(2, chlblab , at=7.55, tick=FALSE, line=2)
stripchart(chloro$chlB ~ treatment, data = chloro,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 

# text(1.5, 1.5, "*",cex=2.5, font=2)
# text(.55, 14.6, "(A)", cex=1.51)
dev.off()