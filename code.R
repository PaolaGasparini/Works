###########
#esercise 1
###########

rm(list=ls())
install.packages("tidyverse")
install.packages("plyr")
library(plyr)
library(tidyverse) 

setwd("D:/Documents/Paola/PEAK")
es2 <- read_csv("es2.csv")

ScoreCountry1 <- es2$Score[es2$CountryName=='X']
ScoreCountry2 <- es2$Score[es2$CountryName=='X']
shapiro.test(ScoreCountry1)
shapiro.test(ScoreCountry2)
#pvalue<0.05 reject null hypothesis, data not normally distributed. 
#if normally distributed: 
#Fisher test for homoskedasticity, homogeneity variances, 
var.test(ScoreCountry1,ScoreCountry2)
#p.value >0.05 homogeneity. 
#if normal and homogeneity:
t.test(ScoreCountry1,ScoreCountry2, var.equal=TRUE)
#if normal and not homogeneity of variances:
t.test(ScoreCountry1,ScoreCountry2, var.equal=FALSE)
#p.value <0.05  the averages of two groups are significantly different.
wilcox.test(ScoreCountry1, ScoreCountry2)
#p.value <0.05  the two groups are significantly different.


###########
#ESERCISE 2
############
rm(list=ls())
install.packages("tidyverse")
library(tidyverse)
setwd("D:/Documents/Paola/PEAK")
data <- read_csv("data_science_data.csv")
data1<- data%>%
  arrange(game,norm_score) %>%
  group_by(user_id,game)%>%
  arrange(game,user_id,timestamp)%>%
  dplyr::slice(max(1):50) %>%  # Keep only last 50 times
  dplyr::mutate(N_time_played = 1:n())  %>% 
  dplyr::mutate(timestamp_number = as.numeric(as.POSIXct(timestamp)))  %>% 
  dplyr::mutate_at(vars(norm_score),
                 funs(
                   norm_score_diff = -(lag(.) - .)
                 )) %>% 
  dplyr::mutate_at(vars(timestamp_number),
                   funs(
                     time_diff = -(lag(.) - .)
                   ))
install.packages("RSQLite")
install.packages("tcltk")
library(sqldf)
data2 <- sqldf('select user_id,game, count(*) as Max_N_played,avg(norm_score) as Avg_NormScore, sum(case when norm_score_diff>0 then 1 else 0 end) as Npositive,  sum(norm_score_diff) as Delta_score_norm, sum(time_diff) as Delta_time from data1 group by 1,2 order by game,count(*) desc')

data2 <- data2%>%
  dplyr::mutate(NpositiveOverTotal = round(Npositive/Max_N_played))
#write.csv(data1, file = "data1.csv")
#write.csv(data, file = "data.csv")
#write.csv(data2, file = "data2.csv")
#write.csv(data3, file = "data3.csv")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

data3<- na.omit(data2)

data3BAG <- data3[data2$game=='BAG',]
data3RUS <- data3[data2$game=='RUS',]
data3FLP <- data3[data2$game=='FLP',]
data3MEM <- data3[data2$game=='MEM',]
data3BAG<- na.omit(data3BAG)
data3RUS<- na.omit(data3RUS)
data3FLP<- na.omit(data3FLP)
data3MEM<- na.omit(data3MEM)

data3 <- data3 %>% 
  dplyr::mutate(Engagementnumerator = range01((data3$Max_N_played*data3$Delta_time)))
data3$BAGMaxPlayed <-data3BAG$Max_N_played
data3$RUSMaxPlayed<-data3RUS$Max_N_played
data3$FLPMaxPlayed<-data3FLP$Max_N_played
data3$MEMMaxPlayed <- data3MEM$Max_N_played
write.csv(data3, file = "data3.csv")


ScoreBAG <-data3[data3$game=='BAG',]
ScoreRUS <-data3[data3$game=='RUS',]
ScoreFLP <-data3[data3$game=='FLP',]
ScoreMEM <-data3[data3$game=='MEM',]

ScoreBAG <-ScoreBAG$Avg_NormScore
ScoreRUS <-ScoreRUS$Avg_NormScore
ScoreFLP <-ScoreFLP$Avg_NormScore
ScoreMEM <-ScoreMEM$Avg_NormScore

ScoreTotal <- cbind(ScoreBAG,ScoreFLP)

ScoreTotalTotal <-  cbind(ScoreTotal,ScoreRUS)

ScoreTotalTotaTotal <-  cbind(ScoreTotalTotal,ScoreMEM)

pairs(ScoreTotalTotaTotal)

scatterplotMatrix(~ ScoreBAG + ScoreFLP +ScoreMEM +ScoreRUS, data= ScoreTotalTotaTotal)

library(ggplot2)
install.packages("ggplot2")
plotmatrix(with(ScoreTotalTotaTotal, data.frame(ScoreBAG, ScoreFLP, ScoreMEM, ScoreRUS)))

data3BAGprototipo <- data3BAG[data3BAG$Max_N_played==50,]


boxplot(data3BAG$Engagement ~ data3BAG$Avg_NormScore, data =data3BAG, add = FALSE,
        boxwex = 0.25, 
         col = "red",
       
        xlab = "Score Improvement",
        ylab = "Numeber of time Game played",
        yaxs = "i")









lm_dataBAG <- lm(formula = data2BAG$Max_N_played ~  data2BAG$Delta_score_norm + data2BAG$NpositiveOverTotal)
summary(lm_dataBAG)

lm_dataRUS <- lm(formula = data2RUS$Max_N_played ~  data2RUS$NpositiveOverTotal)
summary(lm_dataRUS)

lm_dataFLP <- lm(formula = data2FLP$Max_N_played ~  data2FLP$NpositiveOverTotal)
summary(lm_dataFLP)

jpeg('rplot.jpg')
png("rplot.png")
pdf('filename.pdf')
pdf('filename.pdf')
jpeg('rplot2.jpg')
par(mfrow=c(2,2))

boxplot(data2$Max_N_played ~ data2$NpositiveOverTotal, 
        subset = data2$game == "BAG", col = "yellow",
        main = "BAG",
        xlab = "Score Improvement ",
        ylab = "Numeber of time Game played",
        yaxs = "i")

boxplot(data2$Max_N_played ~ data2$NpositiveOverTotal, data = data2, add = FALSE,
        boxwex = 0.25, 
        subset = data2$game == "RUS", col = "orange",
        main = "RUS",
        xlab = "Score Improvement",
        ylab = "Numeber of time Game played",
        yaxs = "i")

boxplot(data2$Max_N_played ~ data2$NpositiveOverTotal, data = data2, add = FALSE,
        boxwex = 0.25, 
        subset = data2$game == "FLP", col = "red",
        main = "FLP",
        xlab = "Score Improvement",
        ylab = "Numeber of time Game played",
        yaxs = "i")

boxplot(data2$Max_N_played ~ data2$NpositiveOverTotal,  data = data2, add = FALSE,
        boxwex = 0.25, 
        subset = data2$game == "MEM", col = "bisque",
        main = "MEM",
        xlab = "Score Improvement",
        ylab = "Numeber of time Game played",
        yaxs = "i")

dev.off()




