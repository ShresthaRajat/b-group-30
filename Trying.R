library(tidyverse)
df <- read.csv("2020-survey_results_public.csv")
Age1stCode <- as.numeric(df[[5]])
ConvertedComp <- df[[8]]
cor(Age1stCode,ConvertedComp,use="pairwise.complete.obs")
pdf("trying.pdf")
plot(jitter(Age1stCode,1),ConvertedComp,xlab="Age first started to code",ylab="Compensation in USD", main="Age 1st Started to Code vs Total Compensation in USD")
abline(lm(ConvertedComp~Age1stCode))

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
          xlab = "Compensation", 
          main = "Frequency Distribution of Total Compensation",
          ylim = c(dtMin,30000),
          xlim = c(dtMin,2000000)) #you might want to tweak this
x <-seq(dtMin, dtMax, .1)  #creates a sequence of numbers between first 2 params
y1 <-dnorm(x, mean=dtMean, sd=dtSd) #creates a theoretical normal distribution based on that

y1 <- y1 *diff(h$mids[1:2]) *length(dt) #a multiplier to make it fit is the histogram
lines(x, y1, col="blue")
dev.off()