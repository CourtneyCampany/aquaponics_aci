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
     width = 6, height = 6, units = "in", res= 500)

par(mar=c(5,5,1,1))
boxplot(sd$sd_mm2 ~ uniqueid, data=sd,varwidth=TRUE,xaxt = 'n',
        ylab=denslab2,border=trtcols,ylim=c(0,400),xlab="", outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
axis(1, boxlabs2[1:4], at=1:4)
mtext(specieslabs, 1, line=2.25, at=c(1.5, 3.5, 5.5), cex=1.25)
stripchart(sd$sd_mm2 ~ uniqueid, data = sd,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE)

text(1.5, 11, "*",cex=2.5, font=2)
text(3.5, 13, "*",cex=2.5, font=2)
text(5.5, 45, "*",cex=2.5, font=2)

dev.off()
