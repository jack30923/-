# -
data <- read.csv("./Data/cs-training.csv")
data <- data[-which(data[,"age"]==0),]

data.MI.NA <- data[which(is.na(data[,"MonthlyIncome"])),] #Monthly Income is NA
data.MI.notNA <- data[which(!is.na(data[,"MonthlyIncome"])),] #Monthly Income is NA

data.DP.NA <- data[which(is.na(data[,"NumberOfDependents"])),] #Monthly Income & Dependents is NA

#Observe NA values，if the dependent is NA,MonthlyIncome is NA.

#Transform numeric variables into factor

breaks.age <- c(0,30,40,50,60,70,109)
labels.age <- c("< 30","30-40","40-50","50-60","60-70","> 70")

breaks.DP <- c(-1,0.01,1.1,2.1,3.1,25)
labels.DP <- c("0","1","2","3","> 3")

breaks.MI <- c(0,3400,5400,8249,3008750)
labels.MI <- c("0-3400","3401-5400","5401-8249","> 8249")

breaks.DR <- c(-1,0.2,0.4,0.6,0.8,1,329664)
labels.DR <- c("0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1","> 1")

breaks.Revolving <- c(-0.1,0.2,0.4,0.6,0.8,1,50708)
labels.Revolving <- c("0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1","> 1")

breaks.RE <- c(-0.1,0.1,1.1,2.1,3.1,4.1,54)
labels.RE <- c("0","1","2","3","4","> 5")

breaks.OC <- c(-0.1,5.1,8.1,11.1,58)
labels.OC <- c("0-5","6-8","9-11","> 11")

breaks.NT30.59DPDNW <- c(-0.1,0.9,1.1,2.1,3.1,98)
labels.NT30.59DPDNW <- c("0","1","2","3","> 3")

breaks.NT60.89DPDNW <- c(-0.1,0.9,1.1,2.1,3.1,98)
labels.NT60.89DPDNW <- c("0","1","2","3","> 3")

breaks.NT90DPDNW <- c(-0.1,0.9,1.1,2.1,3.1,98)
labels.NT90DPDNW <- c("0","1","2","3","> 3")


#Data visualiazion in different data tables.

library(ggplot2)
library(tidyverse)

data.stat.MI.NA %>%
  group_by(Y , Dependents) %>%
  summarise(Median = median(Debt.ratio.n),
            Mean = mean(Debt.ratio.n)) %>%
  ggplot(mapping = aes(
    x = Dependents ,
    y = Median ,
    colour = Y,
    group = Y
  )) +
  geom_point(size = 5) + geom_line() + geom_text(
    aes(
      x = Dependents ,
      y = Median ,
      colour = Y,
      group = Y ,
      label = round((Median), 3)
    )
    ,
    vjust = 1.5
    ,
    size = 4
  ) + labs(x = "公開貸款和信用上線數量",
           y = "債務比例中位數",
           colour = "是否違約") + theme_bw()



#違約比例by OC

data.stat.MI.NA %>%
  group_by(Open.Credit, Y) %>%
  summarise(
    Median = median(Debt.ratio.n),
    Mean = mean(Debt.ratio.n),
    Freq = n()
  ) %>%
  mutate(Propor = Freq / sum(Freq)) %>%
  ggplot() + geom_bar(aes(
    x = Open.Credit ,
    y = Propor ,
    fill = Y ,
    label = paste(Propor, '%', sep = '')
  ),
  stat = 'identity') + scale_fill_manual(
    "Default",
    values = c("違約" = "#2c7fb8", "不違約" = "#edf8b1")) + geom_text(
      aes(
        x = Open.Credit ,
        y = Propor ,
        fill = Y ,
        label = paste(round(Propor, 3) *
                        100, '%', sep = '')
      ),
      position = position_stack(vjust = 0.5),
      size = 4
    )


#違約比例by DR

data.stat.MI.NA %>%
  group_by(Y, Debt.ratio) %>%
  summarise(
    Median = median(Debt.ratio.n),
    Mean = mean(Debt.ratio.n),
    Freq = n()
  ) %>%
  mutate(Propor = Freq / sum(Freq)) %>%
  ggplot() + geom_bar(aes(
    x = Y ,
    y = Propor ,
    fill = Debt.ratio ,
    label = paste(Propor, '%', sep = '')
  ),
  stat = 'identity') + geom_text(
    aes(
      x = Y ,
      y = Propor ,
      fill = Debt.ratio ,
      label = paste(round(Propor, 3) *
                      100, '%', sep = '')
    ),
    position = position_stack(vjust = 0.5),
    size = 3
  ) 


