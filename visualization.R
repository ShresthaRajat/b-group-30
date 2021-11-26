library(tidyverse)
df <- read.csv("2020-survey_results_public.csv")
df <- df[!is.na(df[[8]]), ]
df <- df[!is.na(df[[5]]), ]
Age1stCode <- as.numeric(df[[5]])
#print(length(Age1stCode))
ConvertedComp <- as.numeric(df[[8]])
cor(Age1stCode,ConvertedComp,use="pairwise.complete.obs")
pdf("visualization.pdf")
plot(jitter(Age1stCode,1),jitter(ConvertedComp,1),xlab="Age first started to code(Age1stCode)",ylab="Compensation in USD (ConvertedComp)", main="Age 1st Started to Code vs Total Compensation in USD")
abline(lm(ConvertedComp~Age1stCode), col="red")
legend("topright",c("Trend of Age1stCode vs ConvertedComp"),cex=.8,col=c("red"),pch=c(NA), lwd=1)


#print(typeof(Age1stCode))
#print(typeof(ConvertedComp))

#hist(ConvertedComp)
#hist(Age1stCode)


dt <- ConvertedComp
#print(typeof(dt))
dtMin=min(dt,na.rm=TRUE)
dtMax=max(dt,na.rm=TRUE)
dtMean=mean(dt,na.rm=TRUE)
dtSd=sd(dt,na.rm=TRUE)
h <- hist(dt, 
          breaks = 20, 
          density = 10,
          col = "lightgray", 
          ylab = "Frequency",
          xlab = "Compensation in USD (ConvertedComp)", 
          main = "Frequency Distribution of Total Compensation (ConvertedComp)",
          ylim = c(dtMin,30000), #frequency
          xlim = c(dtMin,2000000)) #compensation values
x <-seq(dtMin, dtMax, .1)  #creates a sequence of numbers between first 2 params
y1 <-dnorm(x, mean=dtMean, sd=dtSd) #creates a theoretical normal distribution based on that

y1 <- y1 *diff(h$mids[1:2]) *length(dt) #a multiplier to make it fit is the histogram
lines(x, y1, col="blue")
legend("topright",c("Normal Distribution of Total Compensation (ConvertedComp)"),cex=.8,col=c("blue"),pch=c(NA), lwd=1)

dev.off()