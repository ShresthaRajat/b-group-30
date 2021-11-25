library(tidyverse)
df <- read.csv("2020-survey_results_public.csv")[1:60000,] 
# df[, c(5)] <- sapply(df[, c(5)], as.numeric)
# df[[8]][df[[8]] < 10000 ] <- NA
# df[[8]][df[[8]] > 1000000 ] <- NA
# df[[5]][df[[5]] > 30 ] <- NA
# df[[5]][df[[5]] < 3 ] <- NA

Age1stCode <- as.numeric(df[[5]])
ConvertedComp <- df[[8]]
cor(ConvertedComp,Age1stCode,use="pairwise.complete.obs")
pdf("visualization.pdf")
plot(jitter(ConvertedComp,1),Age1stCode,xlab="Total Compensation Converted to USD",ylab="Age 1st Started to Code", main="Age 1st Started to Code vs Total Compensation")
abline(lm(Age1stCode~ConvertedComp), col="red")

print(typeof(Age1stCode))
print(typeof(ConvertedComp))

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
          xlab = "Compensation", 
          main = "Frequency Distribution of Compensation",
          ylim = c(dtMin,30000),
          xlim = c(0,2000000)) #you might want to tweak this
x <-seq(dtMin, dtMax, .1)  #creates a sequence of numbers between first 2 params
y1 <-dnorm(x, mean=dtMean, sd=dtSd) #creates a theoretical normal distribution based on that

y1 <- y1 *diff(h$mids[1:2]) *length(dt) #a multiplier to make it fit is the histogram
lines(x, y1, col="blue")
dev.off()