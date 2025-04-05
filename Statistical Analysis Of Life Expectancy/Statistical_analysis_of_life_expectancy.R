library(ggplot2)
library(e1071)
library(ggcorrplot)
library(car)
library(leaps)
library(Metrics)

#load data
data <- read.csv("D://SEM  3//R Programming//Package//life-expect-data.csv")
head(data)
sprintf("Dataset size: [%s]", toString(dim(data)))

#clean annd filter data

#Remove unnessary data
data <- subset(data, select = -c(Country, Year))

#check missing data
missing.rows = dim(data)[1] -  dim(na.omit(data))[1]
sprintf("Dataset size: [%s]", toString(dim(data)))
sprintf("Missing rows: %s (%s%%)", missing.rows, round((missing.rows*100)/dim(data)[1], 2))

missings_df <- data.frame(type=c("missing", "non-missing") ,count = c(missing.rows,  dim(na.omit(data))[1]))

par(pin = c(16, 4))
ggplot(missings_df, aes(fill=type, y="", x=count)) +
  geom_bar(position="stack", stat="identity")+
  ggtitle("Missing vs Non-missing row counts") +
  xlab("Missing count") + ylab("") +
  theme(text = element_text(size = 18))+
  scale_fill_brewer(palette="Set1")

missing_counts <- data.frame(feature = factor(names(data)),
                             counts=sapply(data, function(x) sum(is.na(x))))

par(pin = c(16, 8))
ggplot(missing_counts,
       aes(x=reorder(feature, -counts), y=counts, fill=counts)) +
  geom_bar(stat="identity") +
  ggtitle("Missing counts in each feature") +
  xlab("Feature") + ylab("Missing count") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 10))+
  scale_fill_continuous(trans = 'reverse')

#data imputation
par(mfrow = c(2, 7), mar = c(4, 4, 2, 1))  # Set up a 2-row by 7-column layout for the boxplots
boxplot(data$Life.expectancy, ylab = "Life Expectancy", main = " Life Expectancy", col = "red", outcol = "red")
boxplot(data$Adult.Mortality, ylab = "Adult Mortality", main = " Adult Mortality", col = "red", outcol = "red")
boxplot(data$Alcohol, ylab = "Alcohol", main = " Alcohol", col = "green", outcol = "green")
boxplot(data$Hepatitis.B, ylab = "Hepatitis B", main = "Hepatitis B", col = "red", outcol = "red")
boxplot(data$BMI, ylab = "BMI", main = " BMI", col = "green", outcol = "green")
boxplot(data$Polio, ylab = "Polio", main = "Boxplot of Polio", col = "red", outcol = "red")
boxplot(data$Total.expenditure, ylab = "Total Expenditure", main = "Total Expenditure", col = "red", outcol = "red")
boxplot(data$Diphtheria, ylab = "Diphtheria", main = " Diphtheria", col = "red", outcol = "red")
boxplot(data$GDP, ylab = "GDP", main = "GDP", col = "red", outcol = "red")
boxplot(data$Population, ylab = "Population", main = "Population", col = "red", outcol = "red")
boxplot(data$thinness..1.19.years, ylab = "Thinness 1-19 years", main = " Thinness for 1-19 years old", col = "red", outcol = "red")
boxplot(data$thinness.5.9.years, ylab = "Thinness 5-9 years", main = " Thinness for 5-9 years old", col = "red", outcol = "red")
boxplot(data$Income.composition.of.resources, ylab = "Income Composition", main = "Income Composition", col = "green", outcol = "green")
boxplot(data$Schooling, ylab = "Schooling", main = " Schooling", col = "red", outcol = "red")

#apply imputation

#median for high outlier variables

Life.expectancy_median <- median(data$Life.expectancy, na.rm = TRUE)
Adult.Mortality_median <- median(data$Adult.Mortality, na.rm = TRUE)
Hepatitis.B_median <- median(data$Hepatitis.B, na.rm = TRUE)
Polio_median <- median(data$Polio, na.rm = TRUE)
Diphtheria_median <- median(data$Diphtheria, na.rm = TRUE)
Total.expenditure_median <- median(data$Total.expenditure, na.rm = TRUE)
GDP_median <- median(data$GDP, na.rm = TRUE)
Population_median <- median(data$Population, na.rm = TRUE)
thinness..1.19.years_median <- median(data$thinness..1.19.years, na.rm = TRUE)
thinness.5.9.years_median <- median(data$thinness.5.9.years, na.rm = TRUE)
Schooling_median <- median(data$Schooling, na.rm = TRUE)

#mean for low outlier variables
Alcohol_mean <- mean(data$Alcohol, na.rm = TRUE)
BMI_mean <- mean(data$BMI, na.rm = TRUE)
Income.composition.of.resources_mean <- mean(data$Income.composition.of.resources, na.rm = TRUE)

#replacing the missing values
#medians
data$Life.expectancy[is.na(data$Life.expectancy)] <- Life.expectancy_median
data$Adult.Mortality[is.na(data$Adult.Mortality)] <- Adult.Mortality_median
data$Hepatitis.B[is.na(data$Hepatitis.B)] <- Hepatitis.B_median
data$Polio[is.na(data$Polio)] <- Polio_median
data$Diphtheria[is.na(data$Diphtheria)] <- Diphtheria_median
data$Total.expenditure[is.na(data$Total.expenditure)] <- Total.expenditure_median
data$GDP[is.na(data$GDP)] <- GDP_median
data$Population[is.na(data$Population)] <- Population_median
data$thinness..1.19.years[is.na(data$thinness..1.19.years)] <- thinness..1.19.years_median
data$thinness.5.9.years[is.na(data$thinness.5.9.years)] <- thinness.5.9.years_median
data$Schooling[is.na(data$Schooling)] <- Schooling_median
#means
data$Alcohol[is.na(data$Alcohol)] <- Alcohol_mean
data$BMI[is.na(data$BMI)] <- BMI_mean
data$Income.composition.of.resources[is.na(data$Income.composition.of.resources)] <- Income.composition.of.resources_mean

#factorize categorical variables
data$Status <- as.factor(data$Status)

#data transformation

copy_data <- data

par(pin = c(10, 8))
ggplot(data, aes(x=Life.expectancy)) +
  geom_density(alpha=.3, fill="red", color="red", size=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)), size=1)+
  ggtitle("Distribution density of Life.expectancy") +
  theme(text = element_text(size = 18))

sprintf("Skewness: [%s]", toString(skewness(data$Life.expectancy, na.rm = TRUE)))

data$Life.expectancy <- sqrt(max(data$Life.expectancy+1)- data$Life.expectancy)
data$Life.expectancy<- scale(data$Life.expectancy, scale=TRUE, center = TRUE)
ggplot(data, aes(x=Life.expectancy)) +
  geom_density(alpha=.3, fill="red", color="red", size=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)))+
  ggtitle("Distribution density of Life.expectancy") +
  theme(text = element_text(size = 18))

sprintf("Skewness: [%s]", toString(skewness(data$Life.expectancy, na.rm = TRUE)))

data <- as.data.frame(scale(subset(data,select = -c(Status)), scale=TRUE, center = TRUE))
data$Status <- copy_data$Status
# data$Life.expectancy <- copy_data$Life.expectancy

data_FULL <- data

#comparing the plots ofdeveloped and developing countries
par(pin = c(10, 8))
ggplot(copy_data ,aes(x= Status,y=Life.expectancy, fill= Status)) +
  geom_boxplot() +
  ggtitle("Life expectancy per country Status")+
  theme(text = element_text(size = 18))+
  scale_fill_brewer(palette="Set1")
#for continuous variables
#finding evidence for these plus minus obs statistically
par(pin = c(16, 10))
corr <- round(cor(subset(data, select =-c(Status))), 3)
ggcorrplot(corr,type = "upper", lab = TRUE, outline.color = "black", lab_size = 4, legend.title = "Correlation")+
  ggtitle("Correlation Matrix")
#very strong correlation>=0.9
#correlation doesn't imply collinearity
#Variance Inflation Factor (VIF) in a linear model
#if vif>5 problematic
#after omitting infant detahs,gdp,thinness 1.19 yrs
par(pin = c(16, 10))
corr <- round(cor(subset(data_EDA, select =-c(Status))), 3)
ggcorrplot(corr,type = "upper", lab = TRUE, outline.color = "black", lab_size = 4, legend.title = "Correlation")

data_EDA <- data

#checking vif again
mod.linear <- lm(Life.expectancy~ ., data = subset(data_EDA, select =-c(Status)))
vifs <- data.frame(vif(mod.linear))

par(pin = c(16, 8))
ggplot(vifs, aes(y=vif.mod.linear., x=row.names(vifs))) +
  geom_bar(aes(fill=vif.mod.linear.<5),stat="identity")+
  scale_y_continuous(trans = "sqrt",  breaks = c(5, 10, 50, 100))+
  geom_hline(yintercept = 5, colour = "red") +
  ggtitle("VIF per feature") +
  xlab("Featurs") + ylab("VIF") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 10))+
  scale_fill_brewer(palette="Dark2")


# Best subset selection
regfit.best <- regsubsets(Life.expectancy ~ ., data = data_EDA, nvmax = 16)
reg.summary <- summary(regfit.best)

# Plot residual sum of squares (RSS)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # Set layout and margins

plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
which.min(reg.summary$rss)  # Find minimum RSS
points(16, reg.summary$rss[16], col = "red", cex = 2, pch = 20)

# Plot Adjusted R^2
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq", type = "l")
which.max(reg.summary$adjr2)  # Find maximum adjusted R^2
points(15, reg.summary$adjr2[15], col = "red", cex = 2, pch = 20)

# Plot Mallow's Cp
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)  # Find minimum Cp
points(13, reg.summary$cp[13], col = "red", cex = 2, pch = 20)

# Plot Bayesian Information Criterion (BIC)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(reg.summary$bic)  # Find minimum BIC
points(12, reg.summary$bic[12], col = "red", cex = 2, pch = 20)

# Reset plot layout
par(mfrow = c(1, 1))

# Forward selection
regfit.fwd <- regsubsets(Life.expectancy ~ ., data = data_EDA, nvmax = 16, method = "forward")
fwd.summary <- summary(regfit.fwd)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # Set layout and margins

# Plot 1: Residual Sum of Squares (RSS)
plot(fwd.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
which.min(fwd.summary$rss)
points(16, fwd.summary$rss[16], col = "red", cex = 2, pch = 20)

# Plot 2: Adjusted R²
plot(fwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R²", type = "l")
which.max(fwd.summary$adjr2)
points(15, fwd.summary$adjr2[15], col = "red", cex = 2, pch = 20)

# Plot 3: Mallow's Cp
plot(fwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(fwd.summary$cp)
points(13, fwd.summary$cp[13], col = "red", cex = 2, pch = 20)

# Plot 4: Bayesian Information Criterion (BIC)
plot(fwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(fwd.summary$bic)
points(12, fwd.summary$bic[12], col = "red", cex = 2, pch = 20)


# Backward Elimination
regfit.bwd <- regsubsets(Life.expectancy ~ ., data = data_EDA, nvmax = 16, method = "backward")
bwd.summary <- summary(regfit.bwd)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
# Plot RSS for backward elimination
plot(bwd.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
points(which.min(bwd.summary$rss), min(bwd.summary$rss), col = "red", cex = 2, pch = 20)

# Plot Adjusted R² for backward elimination
plot(bwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R²", type = "l")
points(which.max(bwd.summary$adjr2), max(bwd.summary$adjr2), col = "red", cex = 2, pch = 20)

# Plot Mallow's Cp for backward elimination
plot(bwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = 'l')
points(which.min(bwd.summary$cp), min(bwd.summary$cp), col = "red", cex = 2, pch = 20)

# Plot BIC for backward elimination
plot(bwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = 'l')
points(which.min(bwd.summary$bic), min(bwd.summary$bic), col = "red", cex = 2, pch = 20)


# Extracting coefficients for comparison
v_names <- rownames(as.data.frame(coef(regfit.best, 12)))
coefs <- data.frame(v_names)
coefs$best_coef_value <- coef(regfit.best, 12)
coefs$fwd_coef_value <- coef(regfit.fwd, 12)
coefs$bwd_coef_value <- coef(regfit.bwd, 12)

# Plot coefficients for best subset method
ggplot(coefs, aes(x = v_names, y = best_coef_value, fill = best_coef_value)) +
  geom_bar(stat = "identity") +
  ggtitle("Features & Coefficients: [method Best]") +
  xlab("Feature") + ylab("Coefficient Value") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), text = element_text(size = 18))

# Plot coefficients for forward selection
ggplot(coefs, aes(x = v_names, y = fwd_coef_value, fill = fwd_coef_value)) +
  geom_bar(stat = "identity") +
  ggtitle("Features & Coefficients: [method Forward inclusion]") +
  xlab("Feature") + ylab("Coefficient Value") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), text = element_text(size = 18))

# Plot coefficients for backward selection
ggplot(coefs, aes(x = v_names, y = bwd_coef_value, fill = bwd_coef_value)) +
  geom_bar(stat = "identity") +
  ggtitle("Feature & Coefficients: [method Backward elimination]") +
  xlab("Feature") + ylab("Coefficient Value") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), text = element_text(size = 10))

# Sub setting data with selected features
data_FS <- subset(data_EDA, select = c(Life.expectancy, Status, Adult.Mortality,
                                       percentage.expenditure, Hepatitis.B, Polio,
                                       BMI, thinness.5.9.years, Measles, Diphtheria,
                                       HIV.AIDS, Income.composition.of.resources, Schooling))

set.seed(123)

sample <- sample(c(TRUE, FALSE), nrow(data_FULL), replace=TRUE, prob=c(0.70,0.30))
train <- data_FULL[sample, ]
x.test <-data_FULL[!sample, ]
y.test <- data_FULL[!sample, ]$Life.expectancy
model.full<- lm(Life.expectancy~., data = train)
summary(model.full)

pred <- predict(model.full, newdata=x.test)
rmse(pred,y.test)
summary(model.full)$adj.r.squared
par(mfrow=c(2,2))
plot(model.full)

set.seed(123)

sample <- sample(c(TRUE, FALSE), nrow(data_EDA), replace=TRUE, prob=c(0.70,0.30))
train <- data_EDA[sample, ]
x.test <-data_EDA[!sample, ]
y.test <- data_EDA[!sample, ]$Life.expectancy
model.EDA <- lm(Life.expectancy~., data = train)
summary(model.EDA)

pred <- predict(model.EDA, newdata=x.test)
rmse(pred,y.test)
summary(model.EDA)$adj.r.squared
par(mfrow=c(2,2))
plot(model.EDA)

set.seed(123)

sample <- sample(c(TRUE, FALSE), nrow(data_FS), replace=TRUE, prob=c(0.70,0.30))
train <- data_FS[sample, ]
x.test <-data_FS[!sample, ]
y.test <- data_FS[!sample, ]$Life.expectancy
model.FS <- lm(Life.expectancy~., data = train)
summary(model.FS)

pred <- predict(model.FS, newdata=x.test)
rmse(pred,y.test)
summary(model.FS)$adj.r.squared
par(mfrow=c(2,2))
plot(model.FS)






