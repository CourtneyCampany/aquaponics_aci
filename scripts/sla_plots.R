source("scripts/plot_objects.R")

#read in leaf data
sla <- read.csv("raw_data/specificleafarea_romaine.csv")
  sla$leafarea_ss <- with(sla, punch_area * number_of_punches)
  sla$sla <- with(sla, leafarea_ss/mass_mg)
sla$treatment <- as.factor(sla$treatment)


#SLA boxplot------
jpeg(filename = "output/sla.jpeg",
       width = 8, height = 6, units = "in", res= 500)
par(mar=c(5,5,1,1))
boxplot(sla ~ treatment, data=sla, xaxt='n',
        ylab="",border=trtcols,ylim=c(0.25,1.6),xlab="", outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
axis(1, boxlabs2, at=1:2)
axis(2, slalab , at=.75, tick=FALSE, line=2)
stripchart(sla$sla ~ treatment, data = sla,
           vertical = TRUE, method = "jitter",cex=1.25,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 

text(1.5, 1.5, "*",cex=2.5, font=2)
# text(.55, 14.6, "(A)", cex=1.51)
# dev.off()