rm(list = ls())

library(ggplot2)
library(reshape2)

setwd("/Users/Huxtable/Documents/MicroPEM仪器保存/MicroPEM测试-0726/")

# read file
testMicroPEM <- read.table("MicroPEM_0726.csv", header = TRUE, sep = ",")

# create a new set used to compare, 1min average

transX44N <- vector(length = 1260)
transX60N <- vector(length = 1260)
transX84N <- vector(length = 1260)
transX93N <- vector(length = 1260)
transX108N <- vector(length = 1260)
transX0964N <- vector(length = 1260)
transX0978N <- vector(length = 1260)

j = 1
i = 1

while(i < 7558)
{
    transX44N[j] <- testMicroPEM$X44N[i:(i+5)]
    transX84N[j] <- testMicroPEM$X84N[i:(i+5)]
    transX93N[j] <- testMicroPEM$X93N[i:(i+5)]
    transX108N[j] <- testMicroPEM$X108N[i:(i+5)]
    transX0964N[j] <- testMicroPEM$X0964N[i:(i+5)]
    transX0978N[j] <- testMicroPEM$X0978N[i:(i+5)]
    transX60N[j] <- testMicroPEM$X60N[i:(i+5)]
    
    i = i + 6 
    j = j + 1
}


# noise reduction

for(i in 1:1259)
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
for(i in 1:1259)
{
    if(abs(transX60N[i+1]-transX60N[i]) > 5)
    {
        transX60N[i+1] = (transX60N[i]+transX60N[i+2])/2
    }
}
for(i in 1:1259)
{
    if(abs(transX84N[i+1]-transX84N[i]) > 5)
    {
        transX84N[i+1] = (transX84N[i]+transX84N[i+2])/2
    }
    if(transX84N[i+1] > 30)
    {
        transX84N[i+1] = transX84N[i+1] - 10
    }
}
for(i in 1:1259)
{
    if(abs(transX93N[i+1]-transX93N[i]) > 5)
    {
        transX93N[i+1] = (transX93N[i]+transX93N[i+2])/2
    }
}
for(i in 1:1259)
{
    if(abs(transX108N[i+1]-transX108N[i]) > 5)
    {
        transX108N[i+1] = (transX108N[i]+transX108N[i+2])/2
    }
}
for(i in 1:1259)
{
    if(abs(transX0964N[i+1]-transX0964N[i]) > 5)
    {
        transX0964N[i+1] = (transX0964N[i]+transX0964N[i+2])/2
    }
}
for(i in 1:1259)
{
    if(abs(transX0978N[i+1]-transX0978N[i]) > 5)
    {
        transX0978N[i+1] = (transX0978N[i]+transX0978N[i+2])/2
    }
}

###########################
# # end noise reduction  ##
###########################

# data adjust

#transX108N = transX108N 
transX84N = transX84N - 1.9
transX44N = transX44N - 2
transX60N = transX60N + 0.8
transX108N = transX60N + 0.2

testPara <- cbind(transX44N, transX60N, transX84N, transX93N, transX108N, transX0964N, transX0978N)

colnames(testPara) <- c("No.44N", "No.60N", "No.84N", "No.93N", "No.108N", "No.0964N", "No.0978N")


# draw the first figure

testParaPlot <- melt(testPara)

colnames(testParaPlot) <- c("Var1", "No.", "value")

ggplot(testParaPlot, aes(x = Var1, y = value)) + geom_line(aes(color = No.)) + xlab("Minutes from Turning on") + ylab("Fine Particles Concentration (ug/m³)")


# calculate every hours' mean and std.error

transX44N_1hourMean <- vector(length = 21)
transX60N_1hourMean <- vector(length = 21)
transX84N_1hourMean <- vector(length = 21)
transX93N_1hourMean <- vector(length = 21)
transX108N_1hourMean <- vector(length = 21)
transX0964N_1hourMean <- vector(length = 21)
transX0978N_1hourMean <- vector(length = 21)

transX44N_1hourErr <- vector(length = 21)
transX60N_1hourErr <- vector(length = 21)
transX84N_1hourErr <- vector(length = 21)
transX93N_1hourErr <- vector(length = 21)
transX108N_1hourErr <- vector(length = 21)
transX0964N_1hourErr <- vector(length = 21)
transX0978N_1hourErr <- vector(length = 21)

testPara <- as.data.frame(testPara)

i = 1
j = 1
while(i < 1260)
{
    transX44N_1hourMean[j] <- mean(testPara$No.44N[i:(i+59)], na.rm = TRUE)
    transX60N_1hourMean[j] <- mean(testPara$No.60N[i:(i+59)], na.rm = TRUE)
    transX84N_1hourMean[j] <- mean(testPara$No.84N[i:(i+59)], na.rm = TRUE)
    transX93N_1hourMean[j] <- mean(testPara$No.93N[i:(i+59)], na.rm = TRUE)
    transX108N_1hourMean[j] <- mean(testPara$No.108N[i:(i+59)], na.rm = TRUE)
    transX0964N_1hourMean[j] <- mean(testPara$No.0964N[i:(i+59)], na.rm = TRUE)
    transX0978N_1hourMean[j] <- mean(testPara$No.0978N[i:(i+59)], na.rm = TRUE)
    
    transX44N_1hourErr[j] <- sd(testPara$No.44N[i:(i+59)], na.rm = TRUE)
    transX60N_1hourErr[j] <- sd(testPara$No.60N[i:(i+59)], na.rm = TRUE)
    transX84N_1hourErr[j] <- sd(testPara$No.84N[i:(i+59)], na.rm = TRUE)
    transX93N_1hourErr[j] <- sd(testPara$No.93N[i:(i+59)], na.rm = TRUE)
    transX108N_1hourErr[j] <- sd(testPara$No.108N[i:(i+59)], na.rm = TRUE)
    transX0964N_1hourErr[j] <- sd(testPara$No.0964N[i:(i+59)], na.rm = TRUE)
    transX0978N_1hourErr[j] <- sd(testPara$No.0978N[i:(i+59)], na.rm = TRUE)
    
    i = i + 60
    j = j + 1
}

transX44N_1hourMean <- round(transX44N_1hourMean, 3)
transX60N_1hourMean <- round(transX60N_1hourMean, 3)
transX84N_1hourMean <- round(transX84N_1hourMean, 3)
transX93N_1hourMean <- round(transX93N_1hourMean, 3)
transX108N_1hourMean <- round(transX108N_1hourMean, 3)
transX0964N_1hourMean <- round(transX0964N_1hourMean, 3)
transX0978N_1hourMean <- round(transX0978N_1hourMean, 3)

transX44N_1hourErr <- round(transX44N_1hourErr, 3)
transX60N_1hourErr <- round(transX60N_1hourErr, 3)
transX84N_1hourErr <- round(transX84N_1hourErr, 3)
transX93N_1hourErr <- round(transX93N_1hourErr, 3)
transX108N_1hourErr <- round(transX108N_1hourErr, 3)
transX0964N_1hourErr <- round(transX0964N_1hourErr, 3)
transX0978N_1hourErr <- round(transX0978N_1hourErr, 3)

# sumFigure <- cbind(transX44N_1hourMean, transX44N_1hourErr,
#                    transX60N_1hourMean, transX60N_1hourErr,
#                    transX84N_1hourMean, transX84N_1hourErr,
#                    transX93N_1hourMean, transX93N_1hourErr,
#                    transX108N_1hourMean, transX108N_1hourErr,
#                    transX0964N_1hourMean, transX0964N_1hourErr,
#                    transX0978N_1hourMean, transX0978N_1hourErr)

sumFigure <- as.data.frame(sumFigure)
#sumFigure2 <- as.data.frame(sumFigure2)


# all data merged, and calculate mean and std.err
X44N_Mean = mean(testPara$No.44N, na.rm = TRUE)
X60N_Mean = mean(testPara$No.60N, na.rm = TRUE)
X84N_Mean = mean(testPara$No.84N, na.rm = TRUE)
X93N_Mean = mean(testPara$No.93N, na.rm = TRUE)
X108N_Mean = mean(testPara$No.108N, na.rm = TRUE)
X0964N_Mean = mean(testPara$No.0964N, na.rm = TRUE)
X0978N_Mean = mean(testPara$No.0978N, na.rm = TRUE)

X44N_StdErr = sd(testPara$No.44N, na.rm = TRUE)
X60N_StdErr = sd(testPara$No.60N, na.rm = TRUE)
X84N_StdErr = sd(testPara$No.84N, na.rm = TRUE)
X93N_StdErr = sd(testPara$No.93N, na.rm = TRUE)
X108N_StdErr = sd(testPara$No.108N, na.rm = TRUE)
X0964N_StdErr = sd(testPara$No.0964N, na.rm = TRUE)
X0978N_StdErr = sd(testPara$No.0978N, na.rm = TRUE)

# deal with sumFigure2
sumFigureMean <- cbind(transX44N_1hourMean, 
                    transX60N_1hourMean, 
                    transX84N_1hourMean, 
                    transX93N_1hourMean, 
                    transX108N_1hourMean, 
                    transX0964N_1hourMean, 
                    transX0978N_1hourMean)

colnames(sumFigureMean) <- c("44N", "60N", "84N", "93N", "108N", "0964N", "0978N") 

sumFigureErr <- cbind(transX44N_1hourErr, 
                    transX60N_1hourErr, 
                    transX84N_1hourErr, 
                    transX93N_1hourErr, 
                    transX108N_1hourErr, 
                    transX0964N_1hourErr, 
                    transX0978N_1hourErr)

# draw figure using sumFigureMean and sumFigureErr
sumFigureMean <- melt(sumFigureMean)
sumFigureErr <- melt(sumFigureErr)

colnames(sumFigureMean) <- c("No.Hour", "Group.Mean", "Mean")
colnames(sumFigureErr) <- c("No.Hour", "Group.Std.Err", "Std.Err")

# merge sumFigureMean and sumFigureErr
sumFigure <- cbind(sumFigureMean, sumFigureErr$Std.Err)
colnames(sumFigure) <- c("No.Hour", "No.Group", "Mean", "Std.Error")

# draw the figure with standard error of the mean
ggplot(sumFigure, aes(x = No.Hour, y = Mean, colour = No.Group)) + 
    geom_errorbar(aes(ymin = Mean - Std.Error, ymax = Mean + Std.Error), width=.1) +
    geom_line() +
    geom_point() +
    xlab("Hours from Turning on") + 
    ylab("Mean Fine Particles Concentration (ug/m³)")








