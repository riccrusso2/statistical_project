rm(list = ls())

setwd("C:/Users/ricca/Documents/statistical_learning/Project")

library(dplyr)
library(tidyr)
library(readxl)
library(psych)
library(corrplot)
library(car)
library(leaps)
library(glmnet)
library(readr)
library(ggplot2)
library(ggcorrplot)
 


data <- read_csv("New_dataset_LifeExpectancy.csv",col_types = cols(X1 = col_skip()))
measles <- read_delim("Measles.csv", ";", escape_double = FALSE, trim_ws = TRUE)
colnames(measles)[1] <- "Country" 

# Measles are number of measles cases by country!!!


# Data Imputation ####

# Replacing NAs with median imputation

# Replace the missing value with the median of the other values for the same Country in different years:


# Adult_mortality



countries <- unique(data$Country[is.na(data$Adult_mortality)])
for (c in countries) {
  x <- data$Adult_mortality[data$Country == c]
  x[is.na(x)] <- median(x, na.rm = T)
  data$Adult_mortality[data$Country == c] <- x
}


# GDP_per_capita

sum(is.na(data$GDP_per_capita))
countries <- unique(data$Country[is.na(data$GDP_per_capita)])
for (c in countries) {
  x <- data$GDP_per_capita[data$Country == c]
  x[is.na(x)] <- median(x, na.rm = T)
  data$GDP_per_capita[data$Country == c] <- x
}
sum(is.na(data$GDP_per_capita))



# Hepatitis_B

sum(is.na(data$Hepatitis_B))
countries <- unique(data$Country[is.na(data$Hepatitis_B)])
for (c in countries) {
  x <- data$Hepatitis_B[data$Country == c]
  x[is.na(x)] <- median(x, na.rm = T)
  data$Hepatitis_B[data$Country == c] <- x
}
sum(is.na(data$Hepatitis_B))


# Schooling

sum(is.na(data$Schooling))
countries <- unique(data$Country[is.na(data$Schooling)])
for (c in countries) {
  x <- data$Schooling[data$Country == c]
  x[is.na(x)] <- median(x, na.rm = T)
  data$Schooling[data$Country == c] <- x
}
sum(is.na(data$Schooling))

# Check if there are any other missing values:

cat("Are there any other missing values in this dataset? = ", !(sum(complete.cases(data)) == nrow(data)))


# PREPROCESSING ####

life0 <- data

life <- merge(life0, measles, by = c("Year", "Country"))

country_to_status <- as.data.frame(cbind("Country" = life$Country, "Status" = life$Economy_status_Developed))
country_to_status$Status <- ifelse(country_to_status$Status == 1, "Developed", "Developing")
country_to_status <- country_to_status[!duplicated(country_to_status),]

temp <- as.data.frame(cbind("Country" = life$Country, "Region" = life$Region))
#temp$Region <- as.character(life3$Region)
temp$Region[temp$Region == "Asia" | temp$Region == "Oceania"] <- "AsiaOceania" 
temp$Region[temp$Region == "Central America and Caribbean" | temp$Region == "South America"] <- "South America" 
temp$Region <- as.factor(temp$Region)
country_to_region <- temp[!duplicated(temp),]


life <- life[,-9] # Remove old Mesales
life2 <- life[,-c(1,2)] # Remove country and year
life2$Region <- as.factor(life2$Region)
temp <- life2$Economy_status_Developed - life2$Economy_status_Developing 
temp[temp == 1] = "Developed"
temp[temp == -1] = "Developing"
life2$Status <- as.factor(temp)
life2 <- life2[,-c(16,17)]
# colnames(life2)

life2$MeaslesCases <- life2$MeaslesCases/life2$Population_mln



# EDA ####

par(mfrow = c(1,1))


# In ggplot:
my_colors <- c("#003f5c","#2f4b7c","#665191", "#a05188", "#d45087", "#f95d6a","#a45154" , "#ff7c43",  "#ffa600")
v <- levels(life2$Region)
life2.1  <- life2
life2.1$Region <- factor(life2.1$Region, levels = v, labels = c("Africa", "Asia", "CA&C", "EU", "M.E.", "N.A.", "Oceania", "RofE", "S.A."))
ggplot(life2.1, aes(x = Region, y = Life_expectancy, fill = Region)) +
  geom_boxplot() +
  ggtitle("Life Expectancy vs Region") +
  scale_fill_manual(values = my_colors, labels = v) + # use custom colors
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(y = "Life Expectancy")



my_colors <- c("#42ff48","#00a6f9")
ggplot(life2, aes(x = Status, y = Life_expectancy, fill = Status)) +
  geom_boxplot() +
  ggtitle("Life Expectancy vs Status") +
  scale_fill_manual(values = my_colors) + # use custom colors
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  labs(y = "Status")



plot(Life_expectancy ~ log(Infant_deaths), life2)

ggplot(life2, aes(x = (Infant_deaths), y = Life_expectancy)) +
  ggtitle("Infant Deaths vs Life Expectancy") +
  geom_point(color = "#0072B2") +
  labs(x = "Infant Deaths", y = "Life Expectancy") +
  theme_bw()



plot(Life_expectancy ~ log(Alcohol_consumption), life2)

ggplot(life2, aes(x = log(Alcohol_consumption), y = Life_expectancy)) +
  ggtitle("log(Alcohol_consumption) vs Life Expectancy") +
  geom_point(color = "#0072B2") +
  scale_x_continuous(trans = "log10", breaks = c(1, 10, 100)) +
  labs(x = "Alcohol_consumption (log)", y = "Life Expectancy") +
  theme_bw()



plot(Life_expectancy ~ (Incidents_HIV), life2)

ggplot(life2, aes(x = (Incidents_HIV), y = Life_expectancy)) +
  ggtitle("Incidents HIV vs Life Expectancy") +
  geom_point(color = "#0072B2") +
  labs(x = "Incidents HIV", y = "Life Expectancy") +
  theme_bw()


corr_mat <- round(cor(subset(life2, select = -c(Region, Status) )), 3)

ggcorrplot(corr_mat,
           outline.color = "white", lab = TRUE, 
           lab_size = 3, ggtheme = ggplot2::theme_gray)

v_temp <- c("Region","Infant Deaths","<5 Deaths","Adult Mortality",
          "Alcohol","HepatitisB","Measles","BMI","Polio","Diphtheria",
            "HIV","GDPxCap","Population","Thinness10/19","Thinness5/9","Schooling")

# To compute the vifs:
par(mfrow = c(1,1))
mod1 <- lm(Life_expectancy~ ., data = subset(life2, select =-c(Status)))
vif1 <- data.frame("Variable" =  names(vif(mod1)[,1]), "Vif" = vif(mod1)[,1],
                   "VarNames" = v_temp,row.names = NULL)

ggplot(vif1, aes(x = Variable, y = Vif)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  geom_text(aes(label = VarNames, y = 1), vjust = -0.5, angle = 90, hjust = 0, size = 7, nudge_x = 0.2) +
  labs(x = "Variable", y = "VIF", title = "Variance Inflation Factors") +
  theme_bw()+
  theme(axis.text.x = element_blank())+
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", size = 0.8)


v_temp <- c("Region","Infant Deaths","Adult Mortality",
            "Alcohol","HepatitisB","Measles","BMI","Polio",
            "HIV","GDPxCap","Population","Thinness10/19","Schooling")

mod2 <- lm(Life_expectancy~ ., data = subset(life2, select =-c(Status, Under_five_deaths, Diphtheria, Thinness_five_nine_years)))
vif2 <- data.frame("Variable" =  names(vif(mod2)[,1]), "Vif" = vif(mod2)[,1],
                   "VarNames" = v_temp,row.names = NULL)

ggplot(vif2, aes(x = Variable, y = Vif)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  geom_text(aes(label = VarNames, y = 0.5), vjust = -0.5, angle = 90, hjust = 0, size = 7, nudge_x = 0.2) +
  labs(x = "Variable", y = "VIF", title = "Variance Inflation Factors") +
  theme_bw()+
  theme(axis.text.x = element_blank())+
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", size = 0.8)


v_temp <- c("Region","Adult Mortality",
            "Alcohol","HepatitisB","Measles","BMI","Polio",
            "HIV","GDPxCap","Population","Thinness10/19","Schooling")

mod3 <- lm(Life_expectancy~ ., data = subset(life2, select =-c(Infant_deaths, Status, Under_five_deaths, Diphtheria, Thinness_five_nine_years)))
vif3 <- data.frame("Variable" =  names(vif(mod3)[,1]), "Vif" = vif(mod3)[,1],
                   "VarNames" = v_temp,row.names = NULL)

ggplot(vif3, aes(x = Variable, y = Vif)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  geom_text(aes(label = VarNames, y = 0.5), vjust = -0.5, angle = 90, hjust = 0, size = 7, nudge_x = 0.2) +
  labs(x = "Variable", y = "VIF", title = "Variance Inflation Factors") +
  theme_bw()+
  theme(axis.text.x = element_blank())+
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", size = 0.8)







corr_mat2 <- round(cor(subset(life2, select = -c(Infant_deaths, Region, Status, Under_five_deaths, Diphtheria, Thinness_five_nine_years) )), 3)
ggcorrplot(corr_mat2,
           outline.color = "white", lab = TRUE, 
           lab_size = 3, ggtheme = ggplot2::theme_gray)


##################################################################################################

# Modelling ####



life3 <- subset(life2, select = -c(Infant_deaths, Under_five_deaths, Diphtheria, Thinness_five_nine_years))


full_model <- lm(Life_expectancy~ ., data = life3)
summary(full_model)


# Let's put together Asia + Oceania and Central America and Caribbean + South America

life3$Region <- as.character(life3$Region)

life3$Region[life3$Region == "Asia" | life3$Region == "Oceania"] <- "AsiaOceania" 
life3$Region[life3$Region == "Central America and Caribbean" | life3$Region == "South America"] <- "South America" 

life3$Region <- as.factor(life3$Region)
levels(life3$Region)

#contrasts(life3$Region)

life3$Region <- relevel(life3$Region, ref= "European Union")







full_model2 <- lm(Life_expectancy~ ., data = life3)
s <- summary(full_model2)
s


windows()
par(mfrow = c(2,2), las = 1)
plot(full_model2)



life4 <- subset(life3, select = -c(Hepatitis_B))
model3 <- lm(Life_expectancy~ ., data = life4)
summary(model3)


life5 <- subset(life4, select = -c(Population_mln))
model4 <- lm(Life_expectancy~ ., data = life5)
summary(model4)


life6 <- subset(life5, select = -c(Alcohol_consumption))
model5 <- lm(Life_expectancy~ ., data = life6)
summary(model5)


life7 <- subset(life6, select = -c(Thinness_ten_nineteen_years))
model6 <- lm(Life_expectancy~ ., data = life7)
summary(model6)


life8 <- subset(life7, select = -c(BMI))
model7 <- lm(Life_expectancy~ ., data = life8)
summary(model7)





windows()
par(mfrow = c(2,2), las = 1)
plot(model6)
par(mfrow = c(1,1))


# Code from internet

regfit.best <- regsubsets(Life_expectancy~., data= life3, nvmax = 30, method = "forward")
reg.summary <- summary(regfit.best)

par(mfrow=c(2,2))




#- residual sum of squares:
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
which.min(reg.summary$rss)
points(which.min(reg.summary$rss),reg.summary$rss[which.min(reg.summary$rss)], col="red",cex=2,pch=20)

# adjusted-R^2 with its largest value
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
which.max(reg.summary$adjr2)
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)], col="red",cex=2,pch=20)

# Mallow's Cp with its smallest value
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="red",cex=2,pch=20)

# BIC with its smallest value
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic)
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red",cex=2,pch=20)



# summary(regfit.best)$which[which.max(summary(regfit.best)$adjr2), ]

summary(regfit.best)$which[which.min(summary(regfit.best)$bic), ]

# sum(summary(regfit.best)$which[which.min(summary(regfit.best)$bic), ])



################################################################################################

# Ridge and Lasso ####

# Ridge with CV

X <- model.matrix(Life_expectancy~., data=life7)
X <- X[,-1]

y <- life7$Life_expectancy

grid <- 10^seq(10, -10, length=100)
plot(grid, type="l")

# Perform 10-fold cross-validated Lasso regression

#set.seed(123) # Set seed for reproducibility

cv <- cv.glmnet(X, y, alpha = 0, lambda=grid, nfolds = 10)

par(mfrow = c(1,1))
# Plot the cross-validation results
plot(cv)

# Get the best value of lambda
best_lambda <- cv$lambda.min
best_lambda

# 0.001873817
# 0.00475081


model_ridge <- glmnet(X, y, alpha = 0, lambda = best_lambda)

ridge.pred.train1 <- predict(model_ridge, s = best_lambda, newx = X, type="response")


mean((ridge.pred.train1 - y)^2)


# WITH THE FULL DATASET (LIFE3)

life3.1 <- life3

X <- model.matrix(Life_expectancy~., data=life3.1)
X <- X[,-1]
y <- life3$Life_expectancy


training_size <- round(0.8*nrow(life3))
test_size <- nrow(life3) - training_size 

indices <- sample.int(nrow(life3), size = training_size, replace = F)

life3_train <- life3.1[indices,]
life3_test <- life3.1[-indices,]


X_train <- model.matrix(Life_expectancy~., data=life3_train)
X_train <- X_train[,-1]

X_test <- model.matrix(Life_expectancy~., data=life3_test)
X_test <- X_test[,-1]

y_train <- life3_train$Life_expectancy
y_test <- life3_test$Life_expectancy


grid <- 10^seq(10, -10, length=100)



# LINEAR MODEL

linear_model <- lm(Life_expectancy~ ., data = life3_train)
lm.pred <- predict(linear_model, life3_test[,-12], type = "response")
# lm.pred.train <- predict(linear_model, life3_train[,-12], type = "response")
mse_linear <- mean((lm.pred.train - y_train)^2)


# RIDGE MODEL

# Choose the optimal value of lambda using cross-validation
set.seed(123) # for reproducibility
cv_model <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 10, type.measure = "mse", lambda = grid)
best_lambda_ridge <- cv_model$lambda.min
ridge_model <- glmnet(X_train, y_train, alpha = 0, lambda = best_lambda_ridge)
ridge.pred <- predict(ridge_model, s = best_lambda_ridge, newx = X_test, type="response")
# ridge.pred.train <- predict(ridge_model, s = best_lambda_ridge, newx = X_train, type="response")


# LASSO MODEL

# Lambda for Lasso
set.seed(123) 
cv_model <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 10, type.measure = "mse", lambda = grid)
best_lambda_lasso <- cv_model$lambda.min
lasso_model <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda_lasso)
lasso.pred <- predict(lasso_model, s = best_lambda_lasso, newx = X_test, type="response")
lasso.pred.train <- predict(lasso_model, s = best_lambda_lasso, newx = X_train, type="response")




# Train

# cat("MSE on training for Linear Model:", mse_linear, "\n")
# cat("MSE on training for Ridge Model:", mean((ridge.pred.train - y_train)^2),"with lambda = ",best_lambda_ridge,  "\n")
# cat("MSE on full dataset for Ridge Model:", mean((ridge.pred.train1 - y)^2),"with lambda = ",best_lambda_ridge1,  "\n")
# cat("MSE on training for Lasso Model:", mean((lasso.pred.train - y_train)^2),"with lambda = ",best_lambda_lasso,  "\n")


# Test:

cat("MSE on test for Linear Model on life3:", mean((lm.pred - y_test)^2), "\n")
cat("MSE on test for Ridge Model on life3:", mean((ridge.pred - y_test)^2),"with lambda = ",best_lambda_ridge,  "\n")
cat("MSE on test for Lasso Model on life3:", mean((lasso.pred - y_test)^2),"with lambda = ",best_lambda_lasso,  "\n")




# Data Collection 2016 ####

# Data collection:


# Adult Mortality

ad_mort <- read_csv("data/WHOSIS_000004.csv")
colnames(ad_mort) <- ad_mort[1,]
ad_mort <- ad_mort[-1,]

adult_mortality <- ad_mort[,1:3]

colnames(adult_mortality)[3] <- "Adult_mortality"

adult_mortality$Year <- as.numeric(adult_mortality$Year) 
adult_mortality$Adult_mortality <- as.numeric(adult_mortality$Adult_mortality)


# BMI #

data_bmi <- read_delim("data/data_bmi.csv", ";", escape_double = FALSE, trim_ws = TRUE)

bmi<- data_bmi[,-c(1,2,4)]
bmi <- bmi[!(bmi$BMI == "No"),]
bmi$BMI <- as.numeric(bmi$BMI)


# Polio #


polio <- read_delim("data/Polio.csv", ";", escape_double = FALSE, trim_ws = TRUE)
polio <- polio[,-c(2,3,4)]


# HIV !!!!!! 

# It does not have countries but just regions
# 
# HIV <- read_delim("HIV.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# colnames(HIV)[c(1,3)] <- c("Region_weird","HIV_cases")
# head(HIV)
# unique(HIV$Region)
# 
# # reg1_to_region <- data.frame("RegionWeird" = unique(HIV$Region), "Region" = c())
# # unique(life3$Region)
# regions_weird <- c("South-East Asia Region", "European Region" , "Africa Region", "Region of the Americas", "Eastern Mediterranean Region","European Region"  ,"Region of the Americas")
# regions_true <- c("AsiaOceania", "Rest of Europe", "Africa", "South America", "Middle East", "European Union", "North America")
# reg1_to_region <- data.frame("Region_weird" = regions_weird,"Region" = regions_true)

# It is better to impute values from 2015 to 2016:

HIV_fake <- cbind("Country" = life$Country[life$Year == 2015], "Year" = rep(2016, sum(life$Year == 2015)), "Incidents_HIV" = life$Incidents_HIV[life$Year == 2015])
HIV_fake <- as.data.frame(HIV_fake)
HIV_fake$Incidents_HIV <- as.numeric(HIV_fake$Incidents_HIV)


# GDP per Capita

GDPxCAP <- read_delim("data/GDPxCAP.csv", ";", escape_double = FALSE, trim_ws = TRUE)
colnames(GDPxCAP)[1] <- "Country"
GDPxCAP <- GDPxCAP[,-2]
GDPxCAP$GDPxCAP <- GDPxCAP$GDPxCAP/1e15
colnames(GDPxCAP)[3] <- "GDP_per_capita"

GDPxCAP <- cbind("Country" = life$Country[life$Year == 2015], "Year" = rep(2016, sum(life$Year == 2015)), "GDP_per_capita" = life$GDP_per_capita[life$Year == 2015])
GDPxCAP <- as.data.frame(GDPxCAP)
GDPxCAP$GDP_per_capita <- as.numeric(GDPxCAP$GDP_per_capita)




# Schooling

schooling <- read_csv("data/mean-years-of-schooling-long-run.csv")
schooling <- schooling[,-2]
colnames(schooling) <- c("Country", "Year", "Schooling")


# Life Expectancy 

life_expectancy <- read_csv("data/life-expectancy-at-birth-total-years.csv")
life_expectancy <- life_expectancy[,-2]
colnames(life_expectancy) <-  c("Country", "Year", "Life_expectancy")


# Infant Deaths MIGHT NOT BE NEEDED

child_mortality <- read_delim("data/child_mortality.csv", ";", escape_double = FALSE, trim_ws = TRUE)
child_mortality <- child_mortality[,-2]
colnames(child_mortality) <- c("Country", "Year", "InfantDeaths")


# Alcohol Consumption


alcohol <- read_delim("data/alcohol.csv", ";", escape_double = FALSE, trim_ws = TRUE)
alcohol <- alcohol[,-c(1,2,4)]
colnames(alcohol)[c(1,3)] <- c("Country", "Alcohol_consumption" )


# Measles

measles <- read_delim("data/Measles.csv", ";", escape_double = FALSE, trim_ws = TRUE)
colnames(measles)[1] <- "Country" 


# Population (MLN) 

population <- read_delim("data/population-and-demography.csv",";", escape_double = FALSE, trim_ws = TRUE)
population$`Population (MLN)` <- gsub(",", replacement = ".", x = population$`Population (MLN)`)
population$`Population (MLN)` <- as.numeric(population$`Population (MLN)`)
colnames(population)[3] <- "Population_mln"


# HepatitisB


hepatitisb <- read_excel("data/epatiteb.xlsx")
hepatitisb <- hepatitisb[,-c(2,3,4)]
hepatitisb$Value <- hepatitisb$Value/1000
colnames(hepatitisb)[3] <- "Hepatitis_B"


# Thinness 10-19 


thinness <- read_excel("data/thinness1019.xlsx")
thinness <- thinness[thinness$Sex == "Both sexes",]
thinness <- thinness[,-c(2,3,4,6,7)]
colnames(thinness)[3] <- "Thinness"
thinness <- thinness[complete.cases(thinness),]
thinness$Thinness <- as.numeric(thinness$Thinness)
thinness$Thinness[thinness$Thinness > 1] <- thinness$Thinness[thinness$Thinness > 1]/1000
colnames(thinness)[3] <- "Thinness_ten_nineteen_years"
  




mega_data <- merge(
  adult_mortality,bmi,
  by = c("Year","Country")
)
#View(mega_data)

# polio,
mega_data <- merge(
  mega_data, polio,
  by = c("Year","Country")
)


# HIV,
mega_data <- merge(
  mega_data, HIV_fake,
  by = c("Year","Country")
)

# GDPxCAP,
mega_data <- merge(
  mega_data,GDPxCAP,
  by = c("Year","Country")
)

# schooling,
mega_data <- merge(
  mega_data,schooling,
  by = c("Year","Country")
)

# life_expectancy,
mega_data <- merge(
  mega_data, life_expectancy,
  by = c("Year","Country")
)


# alcohol,
mega_data <- merge(
  mega_data, alcohol,
  by = c("Year","Country")
)

# measles,
mega_data <- merge(
  mega_data, measles,
  by = c("Year","Country")
)

# population
mega_data <- merge(
  mega_data, population,
  by = c("Year","Country")
)

# thinness

mega_data <- merge(
  mega_data, thinness,
  by = c("Year","Country")
)


# hepatitisb

mega_data <- merge(
  mega_data, hepatitisb,
  by = c("Year","Country")
)

mega_data <- merge(
  mega_data, country_to_status,
  by = c("Country")
)

# View(mega_data_final)

mega_data_final <- merge(mega_data, country_to_region, by = "Country")
mega_data_final <- mega_data_final[!duplicated(mega_data_final),]
colnames(mega_data_final)

mega_data_final$MeaslesCases <- mega_data_final$MeaslesCases/mega_data_final$Population_mln

mega_data_final$Status <- as.factor(mega_data_final$Status)


# Prediction 2016 ####
mega_data_final$Region <- relevel(mega_data_final$Region, ref= "European Union")

X_predict <- (mega_data_final[,-c(1,2, 9)]) # Remove country, Year, Life_expectancy
# X_predict_lm <- X_predict[,-c(2,7,8,10,11)]

y_true <- mega_data_final$Life_expectancy

y_pred_linearmodel <- predict(model7 ,(X_predict))

mse_linear <- (sum((y_pred_linearmodel - y_true)^2))/length(y_true)
mse_linear

# length(y_true)
# par(mfrow = c(1,1))
# plot(x = 1:length(y_true), y = y_true, type = "b", col = "blue")
# lines(x = 1:length(y_true), y = y_pred_linearmodel, type = "b", col = "red")

X_test_2016 <- model.matrix(Life_expectancy~., data=mega_data_final[,-c(1,2)])
X_test_2016 <- X_test_2016[,-1]
X_test_2016 <- X_test_2016[,match(colnames(X_train), colnames(X_test_2016))]


y_pred_ridge <- predict(ridge_model, s = best_lambda_ridge, newx = X_test_2016, type="response")
mse_ridge <- (sum((y_pred_ridge - y_true)^2))/length(y_true)
mse_ridge


y_pred_lasso <- predict(lasso_model,  s = best_lambda_lasso, newx = X_test_2016, type="response")
mse_lasso <- (sum((y_pred_lasso - y_true)^2))/length(y_true)
mse_lasso




# lines(x = 1:length(y_true), y = y_pred_ridge, type = "b", col = "green")
# lines(x = 1:length(y_true), y = y_pred_lasso, type = "b", col = "orange")


par(mfrow = c(3,1))
plot(x = 1:length(y_true), y = y_true, type = "b", col = "blue", main = "Linear Regression")
lines(x = 1:length(y_true), y = y_pred_linearmodel, type = "b", col = "red")
legend(x = 0, y = 64, legend = c("True", "Predicted"), col = c("blue", "red"), pch = 1, pt.bg = "white", pt.cex = 2, lwd = 2, bty = "n")

plot(x = 1:length(y_true), y = y_true, type = "b", col = "blue", main = "Ridge Regression")
lines(x = 1:length(y_true), y = y_pred_ridge, type = "b", col = "green")
legend(x = 0, y = 64, legend = c("True", "Predicted"), col = c("blue", "green"), pch = 1, pt.bg = "white", pt.cex = 2, lwd = 2, bty = "n")

plot(x = 1:length(y_true), y = y_true, type = "b", col = "blue", main = "Lasso Regression")
lines(x = 1:length(y_true), y = y_pred_lasso, type = "b", col = "orange")
legend(x = 0, y = 64, legend = c("True", "Predicted"), col = c("blue", "orange"), pch = 1, pt.bg = "white", pt.cex = 2, lwd = 2, bty = "n")
















