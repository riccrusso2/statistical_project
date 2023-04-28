rm(list = ls())
library(dplyr)
library(tidyr)
library(readxl)


life_bad <- read_excel("life.xlsx")
life_bad$Status <- as.factor(life_bad$Status)
colnames(life_bad) <- gsub(" ", x = colnames(life_bad), replacement = "_")

View((life_bad))

life_noNA <- na.omit(life)

life %>% 
  group_by(Country) %>% 
  mutate_at(vars(all_of(.)), ~replace_na(., mean(., na.rm = TRUE)))


for (i in 1:ncol(life_bad)) {
  if(sum(is.na(life_bad[,i])) == 0){
    cat(colnames(life_bad)[i], sum(is.na(life_bad[,i])), "\n")
    # life2 <- life %>% 
    #           group_by(Country) %>% 
    #           mutate_at(vars(colnames(life)[i]), ~replace_na(., mean(., na.rm = TRUE)))
  }
    
  
    
  #cat(colnames(life)[i], sum(is.na(life[,i])), sep = " : ", "\n")
}


unique(life$Country[is.nan(life$Population)]) 
sum(is.nan(life$GDP)) 


############################################################################

rm(list = ls())

library(psych)
library(corrplot)
library(car)

life <- read_csv("Life-Expectancy-Data-Updated.csv")

# PREPROCESSING

life2 <- life[,-c(1,3)]
life2$Region <- as.factor(life2$Region)

temp <- life2$Economy_status_Developed - life2$Economy_status_Developing 
temp[temp == 1] = "Developed"
temp[temp == -1] = "Developing"
life2$Status <- as.factor(temp)

life2 <- life2[,-c(17,18)]


# EDA





par(mfrow = c(1,1))
plot(density((life2$Life_expectancy)))


boxplot(life$Life_expectancy~life$Region)

boxplot(Life_expectancy~Status, data = life2)

plot(Life_expectancy ~ log(Infant_deaths), life2)

plot(Life_expectancy ~ log(Alcohol_consumption), life2)

plot(Life_expectancy ~ (Incidents_HIV), life2)

summary(life2)

boxplot(life2$Alcohol_consumption)



corr_mat <- round(cor(subset(life2, select = -c(Region, Status) )), 3)

corrplot(corr_mat, method = "number")

par(las=2)
corPlot(corr_mat, upper = T) #, type = "lower")#, (100))

# To compute the vifs:

mod1 <- lm(Life_expectancy~ ., data = subset(life2, select =-c(Status)))
barplot(vif(mod1)[,1])
abline(h = 5)


mod2 <- lm(Life_expectancy~ ., data = subset(life2, select =-c(Status, Under_five_deaths, Diphtheria, Thinness_five_nine_years)))
barplot(vif(mod2)[,1])
abline(h = 5)

corr_mat2 <- round(cor(subset(life2, select = -c(Region, Status, Under_five_deaths, Diphtheria, Thinness_five_nine_years) )), 3)
par(las=2)
corPlot(corr_mat2, upper = T) 

?corPlot

m1 <- lm(Life_expectancy ~ (Incidents_HIV), life2)
summary(m1)
plot(m1)

View(var(life2))

model1 <- lm(Life_expectancy~Region, data = life)
summary(model1)


plot(density(life2$Incidents_HIV))


pairs(life[,-1])

life3 <- cbind(life2$Life_expectancy, life2[,-17])

par(mfrow = c(4,5))
for(p in 1:17){
  plot(life3[,1]~life3[,p])
}


















