source("scripts/functions.R")
source("scripts/plot_objects.R")

sd <- read.csv("raw_data/stomataldensity_master.csv")
  sd$date <- as.Date(sd$date, format = "%m/%d/%Y")
  sd$stomatacount <- with(sd, (c1+c2+c3)/3)
  #calculate density from FOV diameter
  sd$sd_mm2 <- with(sd, (stomatacount/(3.14 * (fov_mm/2)^2)))
  sd$uniqueid <- paste(sd$species, sd$treatment, sep="-")
  sd$treatment <- as.factor(sd$treatment)
  sd$species <- as.factor(sd$species)
  

jpeg(filename = "output/stomataldensity.jpeg",
     width = 8, height = 6, units = "in", res= 500)

par(mgp=c(2.5,.75,0), cex.lab=1.25,cex.axis=1.25, omi=c(.5,0,0.1,0.1))

par(mar=c(5,5,1,1))
boxplot(sd$sd_mm2 ~ uniqueid, data=sd,varwidth=TRUE,xaxt = 'n',
        ylab=denslab2,border=trtcols,ylim=c(0,430),xlab="", outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
axis(1, boxlabs2[1:6], at=1:6)
# axis(2, denslab2 , at=215, tick=FALSE, line=2)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
stripchart(sd$sd_mm2 ~ uniqueid, data = sd,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

text(1.5, 425, "*",cex=2.5, font=2)
text(3.5, 190, "*",cex=2.5, font=2)
text(5.5, 100, "*",cex=2.5, font=2)

dev.off()
