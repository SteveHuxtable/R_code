rm(list = ls())

library(ggplot2)
library(reshape2)

setwd("/Users/Huxtable/Documents/MicroPEM仪器保存/MicroPEM测试-0726/")

# read file
testMicroPEM <- read.table("MicroPEM_0726.csv", header = TRUE, sep = ",")

# create a new set used to compare, 1min average

transX44N <- vector(length = 1260/5)
transX60N <- vector(length = 1260/5)
transX84N <- vector(length = 1260/5)
transX93N <- vector(length = 1260/5)
transX108N <- vector(length = 1260/5)
transX0964N <- vector(length = 1260/5)
transX0978N <- vector(length = 1260/5)

j = 1
i = 1

while(i < 7558)
{
    transX44N[j] <- testMicroPEM$X44N[i:(i+29)]
    transX84N[j] <- testMicroPEM$X84N[i:(i+29)]
    transX93N[j] <- testMicroPEM$X93N[i:(i+29)]
    transX108N[j] <- testMicroPEM$X108N[i:(i+29)]
    transX0964N[j] <- testMicroPEM$X0964N[i:(i+29)]
    transX0978N[j] <- testMicroPEM$X0978N[i:(i+29)]
    transX60N[j] <- testMicroPEM$X60N[i:(i+29)]
    
    i = i + 30 
    j = j + 1
}

for(i in 1:251)
{
    transX108N[i] = transX108N[i] + 2
}

# noise reduction

for(i in 1:251)
{
    if(abs(transX44N[i+1]-transX44N[i]) > 5)
    {
        transX44N[i+1] = (transX44N[i]+transX44N[i+2])/2
    }
    if(transX44N[i+1] > 30)
    {
        transX44N[i+1] = transX44N[i+1] - 10
    }
}
for(i in 1:251)
{
    if(abs(transX60N[i+1]-transX60N[i]) > 5)
    {
        transX60N[i+1] = (transX60N[i]+transX60N[i+2])/2
    }
}
for(i in 1:251)
{
    if(abs(transX84N[i+1]-transX84N[i]) > 5)
    {
        transX84N[i+1] = (transX84N[i]+transX84N[i+2])/2
    }
}
for(i in 1:251)
{
    if(abs(transX93N[i+1]-transX93N[i]) > 5)
    {
        transX93N[i+1] = (transX93N[i]+transX93N[i+2])/2
    }
}
for(i in 1:251)
{
    if(abs(transX108N[i+1]-transX108N[i]) > 5)
    {
        transX108N[i+1] = (transX108N[i]+transX108N[i+2])/2
    }
}
for(i in 1:251)
{
    if(abs(transX0964N[i+1]-transX0964N[i]) > 5)
    {
        transX0964N[i+1] = (transX0964N[i]+transX0964N[i+2])/2
    }
}
for(i in 1:251)
{
    if(abs(transX0978N[i+1]-transX0978N[i]) > 5)
    {
        transX0978N[i+1] = (transX0978N[i]+transX0978N[i+2])/2
    }
}

###########################
# # end noise reduction  ##
###########################

testPara <- cbind(transX44N, transX60N, transX84N, transX93N, transX108N, transX0964N, transX0978N)

colnames(testPara) <- c("No.44N", "No.60N", "No.84N", "No.93N", "No.108N", "No.0964N", "No.0978N")


testParaPlot <- melt(testPara)

colnames(testParaPlot) <- c("Var1", "No.", "value")

testParaPlot$Var1 <- testParaPlot$Var1 * 5

ggplot(testParaPlot, aes(x = Var1, y = value)) + geom_line(aes(color = No.)) + xlab("Minutes from Turning on") + ylab("Fine Particles Concentration (ug/m³)")