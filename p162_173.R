df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5,4,3,4,NA))
is.na(df)
table(is.na(df))
table(is.na(df$sex))
table(is.na(df$score))
mean(df$score)
sum(df$score)
library(dplyr)
df %>% filter(is.na(score))
df %>% filter(!is.na(score))
df_nomiss <- df %>% filter(!is.na(score))
mean(df_nomiss$score)
sum(df_nomiss$score)
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss
df_nomiss2 <- na.omit(df)
df_nomiss2
mean(df$score, na.rm = T)
sum(df$score, na.rm = T)
exam <- read.csv("csv_exam.csv")
exam[c(3,8,15), "math"] <- NA
exam
exam %>% summarise(mena_math = mean(math))
exam %>% summarise(mean_math = mean(math, na.rm=T))
exam %>% summarise(mean_math = mean(math, na.rm = T),
                   sum_math = sum(math, na.rm = T),
                   median_math = median(math, na.rm = T))
mean(exam$math, na.rm = T)
exam$math <- ifelse(is.na(exam$math), 55, exam$math)
table(is.na(exam$math))
exam
mean(exam$math)
mpg<-as.data.frame(ggplot2::mpg)
mpg
mpg[c(65,124,131,153,212), "hwy"] <- NA
mpg
table(is.na(mpg$drv))
table(is.na(mpg$hwy))
mean(mpg$drv)
mean(mpg$drv, na.rm=T)
mpg %>% 
  group_by(drv) %>% 
  filter(!is.na(hwy)) %>% 
  summarise(mean_hwy = mean(hwy))
outlier <- data.frame(sex = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,6))
outlier
table(outlier$sex)
table(outlier$score)
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier$sex
outlier$score <- ifelse(outlier$score >5 , NA, outlier$score)
outlier
outlier %>% 
  filter(!is.na(sex) & !is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score = mean(score))
