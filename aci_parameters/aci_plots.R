source("scripts/plot_objects.R")

aci <- read.csv("aci_parameters/aci_params_simple.csv")
  aci$treatment <- as.factor(aci$treatment)

aqua <- aci[aci$treatment == "a",]
soil <- aci[aci$treatment == "s",]

t.test(aqua$V_cmax, soil$V_cmax) #p < 0.001
t.test(aqua$J_max , soil$J_max) #p < 0.001

mean(aqua$V_cmax)
mean(soil$V_cmax)
mean(aqua$J_max)
mean(soil$J_max)


### jmax vcmax 2 panel
jpeg(filename = "output/Figure4_aci.jpeg",
     width = 8, height = 10, units = "in", res= 500)
par(mfrow=c(2,1),mgp=c(2.5,.75,0), cex.lab=1.25,  cex.axis = 1.25)

par(mar=c(0,5,1,1))
boxplot(V_cmax ~ treatment, data=aci, xaxt='n',varwidth=TRUE,
        ylab=vcmaxlab, border=trtcols,ylim=c(0,80),xlab="", outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)

stripchart(V_cmax ~ treatment, data = aci,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
axis(1, at=1:2, labels=FALSE)
text(1.5, 75, "*",cex=2.5, font=2)
text(.5, 80, "A", cex=1.51, font=2)


par(mar=c(5,5,0,1))
boxplot(J_max ~ treatment, data=aci, xaxt='n',varwidth=TRUE,
        ylab=jmaxlab, border=trtcols,ylim=c(0,170),xlab="", outline=FALSE,
        boxlwd=2.5, whisklwd=2.5,staplelwd=2.5)
axis(1, at=1:2, labels=FALSE)
stripchart(J_max ~ treatment, data = aci,
           vertical = TRUE, method = "jitter",cex=1.5,
           pch = 16,  col= trtcols2, xaxt='n', add=TRUE) 
axis(1, boxlabs, at=1:2)
text(1.5, 150, "*",cex=2.5, font=2)
text(.5, 170, "B", cex=1.51, font=2)
dev.off()
