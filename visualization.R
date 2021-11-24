library(tidyverse)
df <- read.csv("2020-survey_results_public.csv")
Age1stCode <- as.numeric(df[[5]])
ConvertedComp <- df[[8]]
cor(ConvertedComp,Age1stCode,use="pairwise.complete.obs")
pdf("visualization.pdf")
plot(jitter(ConvertedComp,1),Age1stCode,xlab="ConvertedComp",ylab="Age1stCode", main="Age1stCode vs Compensation")
abline(lm(Age1stCode~ConvertedComp))

print(typeof(Age1stCode))
print(typeof(ConvertedComp))

hist(ConvertedComp)
hist(Age1stCode)


dt <- ConvertedComp
print(typeof(dt))
dtMin=min(dt,na.rm=TRUE)
dtMax=max(dt,na.rm=TRUE)
dtMean=mean(dt,na.rm=TRUE)
dtSd=sd(dt,na.rm=TRUE)
h <- hist(dt, 
          breaks = 20, 
          density = 10,
          col = "lightgray", 
          ylab = "Age first started to code",
          xlab = "Compensation", 
          main = "Frequency Distribution of Age and Salary",
          ylim = c(dtMin,30000),
          xlim = c(0,2000000)) #you might want to tweak this
x <-seq(dtMin, dtMax, .1)  #creates a sequence of numbers between first 2 params
y1 <-dnorm(x, mean=dtMean, sd=dtSd) #creates a theoretical normal distribution based on that

y1 <- y1 *diff(h$mids[1:2]) *length(dt) #a multiplier to make it fit is the histogram
lines(x, y1, col="blue")
dev.off()