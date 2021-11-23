library(tidyverse)
age_compensation_correlations <- read_csv("2020-survey_results_public.csv", col_names = FALSE)
dfTsc<-as.data.frame(t(age_compensation_correlations))
dfTsc<-dfTsc[-1,]
names(dfTsc)[3] <- "Age1stCode"
names(dfTsc)[5] <- "ConvertedComp"
dfTsc$Age1stCode<-as.numeric(dfTsc$Age1stCode)
dfTsc$ConvertedComp<-as.numeric(dfTsc$ConvertedComp)
cor(dfTsc$ConvertedComp,dfTsc$Age1stCode,use="pairwise.complete.obs")
pdf("visualization.pdf")
plot(jitter(dfTsc$ConvertedComp,1),dfTsc$Age1stCode,xlab="ConvertedComp",ylab="Age1stCode", main="Age1stCode vs Compensation")
#plot(dfTsc$ConvertedComp,dfTsc$Age1stCode,xlab="Age1stCode",ylab="ConvertedComp",main="Age1stCode vs Compensation")
abline(lm(dfTsc$Age1stCode~dfTsc$ConvertedComp))

#hist(dfTsc$ConvertedComp)



dt <-dfTsc$Age1stCode
dtMin=min(dt,na.rm=TRUE)
dtMax=max(dt,na.rm=TRUE)
dtMean=mean(dt,na.rm=TRUE)
dtSd=sd(dt,na.rm=TRUE)
h <- hist(dt, breaks = 20, density = 10,
          col = "lightgray", 
          ylab = "Age first started to code",
          xlab = "Compensation", 
          main = "Frequency Distribution of Age and Salary",
          xlim=c(dtMin,5),
          ylim=c(0,100)) #you might want to tweak this
x <-seq(dtMin, dtMax, .1)  #creates a sequence of numbers between first 2 params
y1 <-dnorm(x, mean=dtMean, sd=dtSd) #creates a theoretical normal distribution based on that

y1 <- y1 *diff(h$mids[1:2]) *length(dt) #a multiplier to make it fit is the histogram
lines(x, y1, col="blue")
dev.off()

