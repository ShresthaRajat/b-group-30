library(tidyverse)
df <- read.csv("2020-survey_results_public.csv")
df <- df[!is.na(df[[8]]), ]
df <- df[!is.na(df[[5]]), ]
Age1stCode <- as.numeric(df[[5]])
ConvertedComp <- as.numeric(df[[8]])
cor.test(Age1stCode, ConvertedComp, method="spearman", exact =FALSE)


