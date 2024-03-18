source("scripts/plot_objects.R")

chloro <- read.csv("raw_data/chlorophyll_master.csv")
  chloro$treatment <- as.factor(chloro$treatment)
  chloro$chla <- 12.7*chloro$abs_664-2.79*chloro$abs_647
  chloro$chlb <- 20.7*chloro$abs_647+8.08*chloro$abs_664
  chloro$chlAB <- with(chloro, chla/chlb)
  chloro$chl_total <- with(chloro, chla+chlb)
  chloro$uniqueid <- paste(chloro$species, chloro$treatment, sep="-")
  # chloro$chlAmass <- with(chloro, chlA/weight_mg)
  # chloro$chlBmass <- with(chloro, chlB/weight_mg)
  # chloro$chlAarea <- with(chloro, chlA/(punches * punch_diam_mm2))
  # chloro$chlBarea <- with(chloro, chlB/(punches * punch_diam_mm2))

jpeg(filename = "output/chlorophylltotal.jpeg",
       width = 6, height = 6, units = "in", res= 500)  
  
par(mar=c(5,5,1,1))
boxplot(chloro$chl_total ~ uniqueid, data=chloro,varwidth=TRUE,xaxt = 'n',
          ylab=chltotallab,border=trtcols,ylim=c(0,41),xlab="", outline=FALSE,
          boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
  axis(1, boxlabs2[1:4], at=1:4)
  mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
  stripchart(chloro$chl_total ~ uniqueid, data = chloro,
             vertical = TRUE, method = "jitter",cex=1.25,
             pch = 16,  col= trtcols2, xaxt='n', add=TRUE)
  
  text(1.5, 11, "*",cex=2.5, font=2)
  text(3.5, 13, "*",cex=2.5, font=2)
  text(5.5, 45, "*",cex=2.5, font=2)
  
dev.off()  
  
  
    
jpeg(filename = "output/chlAB.jpeg",
     width = 8, height = 10, units = "in", res= 500)

par(mfrow=c(2,1),mgp=c(2.5,.75,0), cex.lab=1.25,  cex.axis = 1.25)

par(mar=c(0,5,1,1))
  boxplot(chloro$chlA ~ treatment, data=chloro, xaxt='n',varwidth=TRUE,
          ylab=chlalab,border=trtcols,ylim=c(0,15),xlab="", outline=FALSE,
          boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
  axis(1, at=1:2, labels=FALSE)
  stripchart(chloro$chlA ~ treatment, data = chloro,
             vertical = TRUE, method = "jitter",cex=1.5,
             pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 

text(1.5, 10, "*",cex=2.5, font=2)
text(0.5, 14.5, "A", cex=1.51, font=2)


par(mar=c(5,5,0,1))
  boxplot(chloro$chlB ~ treatment, data=chloro, xaxt='n',varwidth=TRUE,
          ylab=chlblab,border=trtcols,ylim=c(0,15),xlab="", outline=FALSE,
          boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
  axis(1, boxlabs, at=1:2)
  stripchart(chloro$chlB ~ treatment, data = chloro,
             vertical = TRUE, method = "jitter",cex=1.5,
             pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
  # mtext(boxlabs, 1, line=2.25, at=1:2, cex=1.25)
  
  text(1.5, 14, "*",cex=2.5, font=2)
  text(0.5, 14.5, "B", cex=1.51, font=2)
  dev.off()
  
  
# #CHL A boxplot------
# jpeg(filename = "output/chlA.jpeg",
#        width = 8, height = 6, units = "in", res= 500)
# par(mar=c(5,5,1,0))
# boxplot(chloro$chlA ~ treatment, data=chloro, xaxt='n',varwidth=TRUE,
#           ylab="",border=trtcols,ylim=c(0,10),xlab="", outline=FALSE,
#           boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
# axis(1, boxlabs2, at=1:6)
# axis(2, chlalab , at=5, tick=FALSE, line=2)
# stripchart(chloro$chlA ~ treatment, data = chloro,
#              vertical = TRUE, method = "jitter",cex=1.25,
#              pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
#   
# # text(1.5, 1.5, "*",cex=2.5, font=2)
# # text(.55, 14.6, "(A)", cex=1.51)
# dev.off()
# 
# #CHL B boxplot------
# jpeg(filename = "output/chlB.jpeg",
#      width = 8, height = 6, units = "in", res= 500)
# par(mar=c(5,5,1,0))
# boxplot(chloro$chlB ~ treatment, data=chloro, xaxt='n',varwidth=TRUE,
#         ylab="",border=trtcols,ylim=c(0,15),xlab="", outline=FALSE,
#         boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
# axis(1, boxlabs2, at=1:6)
# axis(2, chlblab , at=7.55, tick=FALSE, line=2)
# stripchart(chloro$chlB ~ treatment, data = chloro,
#            vertical = TRUE, method = "jitter",cex=1.25,
#            pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
# 
# # text(1.5, 1.5, "*",cex=2.5, font=2)
# # text(.55, 14.6, "(A)", cex=1.51)
# dev.off()