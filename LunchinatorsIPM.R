sim<-generateData()
sim$sizeNext<-sim2$sizenext ####not always necessary###
library(IPMpack)


gr1<-makeGrowthObj(dataf=sim, Formula = sizeNext~size)

sv1<-makeSurvObj(sim, Formula = surv~size)

Pmatrix<- makeIPMPmatrix(nBigMatrix = 50,
                         minSize = min(sim$size, na.rm = T), 
                         maxSize = max(sim$size, na.rm = T),
                         growObj=gr1, survObj=sv1)

LE<-meanLifeExpect(Pmatrix)
plot(Pmatrix@meshpoints, LE, ylab = "Life Expectancy", xlab= "Size")

fv1<- makeFecObj(sim, Formula = fec~size, Family = "gaussian", Transform = "log")
Fmatrix<- makeIPMFmatrix(nBigMatrix = 50, 
                         minSize = min(sim$size, na.rm = T), 
                         maxSize = max(sim$size, na.rm = T), 
                         fecObj = fv1)

IPMmatrix<-makeIPMmatrix(Pmatrix,Fmatrix)
print(IPMmatrix)
require(fields)

image.plot(IPMmatrix@meshpoints, IPMmatrix@meshpoints, t(Pmatrix), 
           xlab = "Size(t)", 
           ylab = "Size(t+1)",
           main = "Survival/Growth Kernel")
image.plot(IPMmatrix@meshpoints, IPMmatrix@meshpoints, t(Fmatrix), 
           xlab = "Size (t)", 
           ylab = "Size(t+1)",
           main = "Fecundity Kernel")
image.plot(IPMmatrix@meshpoints, IPMmatrix@meshpoints, t(IPMmatrix), 
           xlab = "Size(t)", 
           ylab = "Size(t+1)",
           main = "IPM Kernel")


elasticity<-elas(IPMmatrix)
image.plot(IPMmatrix@meshpoints, IPMmatrix@meshpoints, t(elasticity),
           xlab = "Size(t)", 
           ylab = "Size(t+1)",
           main = "Elasticities")
sensitivities<-sens(IPMmatrix)
image.plot(IPMmatrix@meshpoints, IPMmatrix@meshpoints, t(sensitivities),
           xlab = "Size(t)", 
           ylab = "Size(t+1)",
           main = "Sensitivities")

