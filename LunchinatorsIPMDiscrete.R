library(IPMpack)

plant<-generateData(type="discrete")
table(plant$stage)

####makes fecundity object, recognizes the discrete stages####
fv1 <- makeFecObj(plant, Formula = fec~size, Family="gaussian", meanOffspringSize = 19 , sdOffspringSize = 1.2,
                  offspringSplitter = data.frame(continuous = 0.5, 
                  dormant = 0.5), 
                  fecByDiscrete = data.frame(dormant = 0, seedAge1=0, seedOld=0))

Fmatrix <- makeIPMFmatrix(fecObj = fv1, nBigMatrix = 50, 
                          minSize = min(plant$size, na.rm = TRUE), 
                          maxSize = max(plant$size, na.rm = TRUE), 
                          correction = "constant")

####same as before to make growth and survival objects####
gr1 <- makeGrowthObj(dataf = plant, Formula = sizeNext~size)
sv1 <- makeSurvObj(plant, Formula = surv~size)

####Finds the transistions within the discrete stages and into the continuous stage####
discTrans<-makeDiscreteTrans(plant)

####Pmatrix is made the same####
Pmatrix <- makeIPMPmatrix(nBigMatrix = 50, 
                          minSize = min(plant$size, na.rm = TRUE), 
                          maxSize = max(plant$size, na.rm = TRUE), 
                          growObj = gr1, 
                          survObj = sv1, 
                          discreteTrans = discTrans, 
                          correction = "constant")

IPMmatrix<-makeIPMmatrix(Pmatrix,Fmatrix)

print(IPMmatrix)

image(x=IPMmatrix@meshpoints, y = IPMmatrix@meshpoints, IPMmatrix)
contour(IPMmatrix@meshpoints, IPMmatrix@meshpoints, t(IPMmatrix))
        
require(fields)

####These plot codes don't work cause the z dimensions aren't right####
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

####This plot code works better#####

x=IPMmatrix@meshpoints
y=IPMmatrix@meshpoints
z=array(t(Fmatrix),dim=c(length(y),length(x)))

image.plot(x, y, z, 
           xlab = "Size(t)", 
           ylab = "Size(t+1)",
           main = "Fec Kernel")

x=IPMmatrix@meshpoints
y=IPMmatrix@meshpoints
z=array(t(Pmatrix),dim=c(length(y),length(x)))

image.plot(x, y, z, 
           xlab = "Size(t)", 
           ylab = "Size(t+1)",
           main = "Growth/Surv Kernel")

x=IPMmatrix@meshpoints
y=IPMmatrix@meshpoints
z=array(t(IPMmatrix),dim=c(length(y),length(x)))

image.plot(x, y, z, 
           xlab = "Size(t)", 
           ylab = "Size(t+1)",
           main = "IPM Kernel")





