library(randomForest)
install.packages("dplyr")
library("dplyr")
install.packages("skimr")
library("skimr")
library("caret")
install.packages("vip")
library("vip")
library("ggplot2")
install.packages("lattice")
library("lattice")
# Put together data

skimr::skim(data)
rm(list=ls())
plot(Tand$tooth, Tand$progression_0to2)

data <- full_join(Tand, Patient, by = c("lopnr", "dropout"))
View(data)
data <- data[data$dropout == 0,]
data <- as.data.frame(data)
data <- data[, -c(1,2, 4, 5)]

sapply(data, class)

data[] <- lapply(data, as.factor)
str(data)

data$tooth <- as.integer(data$tooth)
data$bonelevel <- as.numeric(data$bonelevel)
data$depth <- as.numeric(data$depth)
data$age <- as.numeric(data$age)
str(data)




data$brotand[is.na(data$brotand)] <- 0

#### ####
data$tooth <- as.integer(data$tooth)
data$molar <- factor(data$molar)
data$furcat <- factor(data$furcat)
data$endodont <- factor(data$endodont)
data$plaque <- factor(data$plaque)
data$bleed <- factor(data$bleed)
data$dropout <- factor(data$dropout)
data$destr <- factor(data$destr)
data$no_progression_indicators <- factor(data$no_progression_indicators)
data$progression_yes_no <- factor(data$progression_yes_no)
data$progression_0to2 <- factor(data$progression_0to2)
data$restorat <- as.factor(data$restorat)
data$mobil <- as.factor(data$mobil)
data$molar <- as.factor(data$molar)
data$brotand <- as.factor(data$brotand)
data$DTMOTHER <- as.factor(data$DTMOTHER)
data$DTFATHER <- as.factor(data$DTFATHER)
data$DEPREG <- as.factor(data$DEPREG)
data$diabetes_notcontr <- as.factor(data$diabetes_notcontr)
data$diabetes_contr <- as.factor(data$diabetes_contr)
data$blood <- as.factor(data$blood)
data$monogen <- as.factor(data$monogen)
data$granulom <- as.factor(data$granulom)
data$drug <- as.factor(data$drug)
data$osteop <- as.factor(data$osteop)
data$malnutr <- as.factor(data$malnutr)
data$sjogrens <- as.factor(data$sjogrens)
data$antal3 <- as.factor(data$antal3)
data$DTINTDIS <- as.factor(data$DTINTDIS)
data$DTSOCEC <- as.factor(data$DTSOCEC)
data$SKHAB <- as.factor(data$SKHAB)
data$SKHIS <- as.factor(data$SKHIS)
data$opexp <- as.factor(data$opexp)
str(data)

data <- data[,- c(31)]



data_numeric <- data[, c(7,8, 14)]




# Example dataset with a categorical variable
data_patient_cat <- Patient[,4:21 ]

table(Patient$DTMOTHER)/213
sum(is.na(Patient$DTMOTHER))/213

table(Patient$DTFATHER)/213
sum(is.na(Patient$DTFATHER))/213

table(Patient$DEPREG)/213
sum(is.na(Patient$DEPREG))/213

table(Patient$diabetes_notcontr)/213
sum(is.na(Patient$diabetes_notcontr))/213

table(Patient$diabetes_contr)/213
sum(is.na(Patient$diabetes_contr))/213

table(Patient$blood)/213
sum(is.na(Patient$blood))/213

table(Patient$monogen)/213
sum(is.na(Patient$monogen))/213

table(Patient$granulom)/213
sum(is.na(Patient$granulom))/213

table(Patient$drug)/213
sum(is.na(Patient$drug))/213

table(Patient$osteop)/213
sum(is.na(Patient$osteop))/213

table(Patient$malnutr)/213
sum(is.na(Patient$malnutr))/213

table(Patient$sjogrens)/213
sum(is.na(Patient$sjogrens))/213

table(Patient$antal3)/213
sum(is.na(Patient$antal3))/213

table(Patient$DTINTDIS)/213
sum(is.na(Patient$DTINTDIS))/213

table(Patient$DTSOCEC)/213
sum(is.na(Patient$DTSOCEC))/213

table(Patient$SKHAB)/213
sum(is.na(Patient$SKHAB))/213

table(Patient$opexp)/213
sum(is.na(Patient$opexp))/213

mean(Patient$age)
sd(Patient$age)
sum(is.na(Patient$age))
#### missForest ####
set.seed(1337)
data1 <- data[!is.na(data$progression_0to2),]
Vi

x <- subset(data1, select = -progression_0to2)

y <- na.omit(data$progression_0to2)
y <- as.data.frame(y)
nrow(x)
nrow(y)


data1 <- as.data.frame(c(y,x))


str(data1)
View(data1)

install.packages("missForest")
library(missForest)
library("randomForest")
data_imp <- missForest(data1)

str(data_imp)

length(data_imp)

data_imp <- as.data.frame(data_imp$ximp)
View(data_imp)

train1 <- sample(nrow(data_imp), 0.7*nrow(data_imp), replace = FALSE)
TrainSet1 <- data_imp[train1,]
ValidSet1 <- data_imp[-train1,]

rf1 <- randomForest(y ~ ., data = TrainSet1, ntree = 500, mtry = 5, importance = TRUE)
rf1

predTrain1 <- predict(rf1, TrainSet1, type = "class")
table(predTrain1, TrainSet1$y)
mean(predTrain1 == TrainSet1$y)

predValid1 <- predict(rf1, ValidSet1, type = "class")
table(predValid1, ValidSet1$y)
mean(predValid1 == ValidSet1$y)

# Confusion Matrix
conf_matrix_model_1 <- confusionMatrix(predValid1, ValidSet1$y)
print(conf_matrix_model_1)


vip(rf1, importance = "MDA")

importance(rf1)

vip_model_1 <- vip(rf1, type = 1)
print(vip_model_2)
importance(rf1)
varImpPlot(rf1)
varImp(rf1) 


importance <- randomForest::importance(rf1, type = 1)

randomForest::varImpPlot(rf1,
                         sort=TRUE,
                         n.var = 10,
                         main="Variable Importance Plot")
## ROC Curve ##
# Load the pROC package
library(pROC)

# Combine classes 1 and 2 as positive class and convert to binary format
pred_binary_1 <- ifelse(predValid1 == 0, 0, 1)
true_binary_1 <- ifelse(ValidSet1$y == 0, 0, 1)

# Calculate the ROC curve and AUC score
roc_curve_1 <- roc(true_binary_1, pred_binary_1)
auc_score_1 <- auc(roc_curve_1)

# Plot the ROC curve
plot(roc_curve_1, main = "ROC Curve for Random Forest Model")
text(0.5, 0.3, paste("AUC =", round(auc_score_1, 3)))

ggroc(roc_curve_1, colour = 'steelblue', size = 2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc_score_1, ')'))




library("vip")
#### Model 2 ####
set.seed(1337)
data_model_2 <- data[, c(  "age", 
                          "DTMOTHER", "DTFATHER", 
                          "DTSOCEC", 
                          "DTINTDIS",
                          "SKHAB", 
                          "opexp",
                          "DEPREG", "diabetes_notcontr", "diabetes_contr", "blood", 
                          "monogen", "granulom", "drug", "osteop", "malnutr", "sjogrens",
                          "antal3",
                          "bleed", 
                          "mobil", 
                          "depth",
                          "restorat", 
                          "bonelevel", 
                          "plaque", 
                          "endodont", 
                          "brotand",  
                          "progression_0to2")]

data_model_2 <- data_model_2[!is.na(data_model_2$progression_0to2),]

# MISSING VALUES IMPUTATION
x_model_2 <- subset(data_model_2, select = -progression_0to2)
y_model_2 <- na.omit(data$progression_0to2)
y_model_2 <- as.data.frame(y_model_2)
nrow(x_model_2)  
nrow(y_model_2) 

data_model_2 <- as.data.frame(c(y_model_2,x_model_2))

data_imp_model_2 <- missForest(data_model_2)

data_imp_model_2 <- data_imp_model_2$ximp

View(data_imp_model_2)
nrow(data_imp_model_2)
str(data_imp_model_2)

# Analysis model 2
train_model_2 <- sample(nrow(data_imp_model_2), 0.7*nrow(data_imp_model_2), replace = FALSE)
TrainSet_model_2 <- data_imp_model_2[train_model_2,]
ValidSet_model_2 <- data_imp_model_2[-train_model_2,]

set.seed(1337)
rf_model_2 <- randomForest(y_model_2 ~ ., data = TrainSet_model_2, ntree = 500, mtry = 5, importance = TRUE)
rf_model_2

predTrain_model_2 <- predict(rf_model_2, TrainSet_model_2, type = "class")
table(predTrain_model_2, TrainSet_model_2$y_model_2)
mean(predTrain_model_2 == TrainSet_model_2$y_model_2)

predValid_model_2 <- predict(rf_model_2, ValidSet_model_2, type = "class")
table_Valid_model_2 <- table(predValid_model_2, ValidSet_model_2$y_model_2)
mean(predValid_model_2 == ValidSet_model_2$y_model_2)

## EVALUATION OF MODEL 2 ##
# Variable Importance
vip_model_2 <- vip(rf_model_2, , type = 1)
print(vip_model_2)
importance(rf_model_2)
varImpPlot(rf_model_2)
varImp(rf_model_2)

vip(rf_model_2, importance = "MDA")

sum(is.na(data_model_3$mobil))

## Confusion Matrix. ##
conf_matrix_model_2 <- confusionMatrix(predValid_model_2, ValidSet_model_2$y_model_2)
print_conf_matrix <- print(conf_matrix_model_2)
library(stargazer)
stargazer(table_Valid_model_2, type = "latex", title = "Confusion Matrix Model 1")


# Convert confusion matrix to table
conf_table <- table(as.factor(ValidSet_model_2$y_model_2), as.factor(predValid_model_2))

# Convert table to data frame
conf_df <- as.data.frame.matrix(conf_table)

# Generate LaTeX table
stargazer(conf_df, type = "latex", title = "Confusion Matrix Model 1")

## ROC Curve ##
# Load the pROC package
library(pROC)

# Combine classes 1 and 2 as positive class and convert to binary format
pred_binary_2 <- ifelse(predValid_model_2 == 0, 0, 1)
true_binary_2 <- ifelse(ValidSet_model_2$y_model_2 == 0, 0, 1)

# Calculate the ROC curve and AUC score
roc_curve_2 <- roc(true_binary_2, pred_binary_2)
auc_score_2 <- auc(roc_curve_2)

# Plot the ROC curve
plot(roc_curve_2, main = "ROC Curve for Random Forest Model")
text(0.5, 0.3, paste("AUC =", round(auc_score_2, 3)))

ggroc(roc_curve_2, colour = 'steelblue', size = 2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc_score_2, ')'))







#### Model 3 ####
set.seed(1337)
data_model_3 <- data[, c(  "age", "DTMOTHER", "DTFATHER", 
                         "DTSOCEC", 
                         "DTINTDIS",
                         "SKHAB", 
                         "opexp",
                         "DEPREG", "diabetes_notcontr", "diabetes_contr", "blood", 
                         "monogen", "granulom", "drug", "osteop", "malnutr", "sjogrens",
                         "bleed", "mobil", "progression_0to2")]
View(data_model_3)
data_model_3 <- data_model_3[!is.na(data_model_3$progression_0to2),]

# Missing values imputation
x_model_3 <- subset(data_model_3, select = -progression_0to2)
y_model_3 <- na.omit(data$progression_0to2)
y_model_3 <- as.data.frame(y_model_3)
nrow(x_model_3)  
nrow(y_model_3) 
  
data_model_3 <- as.data.frame(c(y_model_3,x_model_3))

data_imp_model_3 <- missForest(data_model_3)





data_imp_model_3 <- data_imp_model_3$ximp
View(data_imp_model_3)
nrow(data_imp_model_3)
str(data_imp_model_3)
# Analysis
train_model_3 <- sample(nrow(data_imp_model_3), 0.7*nrow(data_imp_model_3), replace = FALSE)
TrainSet_model_3 <- data_imp_model_3[train_model_3,]
ValidSet_model_3 <- data_imp_model_3[-train_model_3,]

set.seed(1337)
rf_model_3 <- randomForest(y_model_3 ~ ., data = TrainSet_model_3, ntree = 500, mtry = 4, importance = TRUE)
rf_model_3

predTrain_model_3 <- predict(rf_model_3, TrainSet_model_3, type = "class")
table(predTrain_model_3, TrainSet_model_3$y_model_3)
mean(predTrain_model_3 == TrainSet_model_3$y_model_3)

predValid_model_3 <- predict(rf_model_3, ValidSet_model_3, type = "class")
table_Valid_model_3 <- table(predValid_model_3, ValidSet_model_3$y_model_3)
mean(predValid_model_3 == ValidSet_model_3$y_model_3)

## EVALUATION OF MODEL 3 ##
# Variable Importance
vip_model_3 <- vip(rf_model_3, type = 1)
print(vip_model_3)
importance(rf_model_3)
varImpPlot(rf_model_3)
varImp(rf_model_3)

vip(rf1, importance = "MDA", n.var = 14)

sum(is.na(data_model_3$mobil))

## Confusion Matrix. ##
conf_matrix_model_3 <- confusionMatrix(predValid_model_3, ValidSet_model_3$y_model_3)
print(conf_matrix_model_3)
library(stargazer)
stargazer(table_Valid_model_3, type = "latex", title = "Confusion Matrix Model 1")


# Convert confusion matrix to table
conf_table <- table(as.factor(ValidSet_model_3$y_model_3), as.factor(predValid_model_3))

# Convert table to data frame
conf_df <- as.data.frame.matrix(conf_table)

# Generate LaTeX table
stargazer(conf_df, type = "latex", title = "Confusion Matrix Model 1")

## ROC Curve ##
# Load the pROC package
library(pROC)

# Combine classes 1 and 2 as positive class and convert to binary format
pred_binary_3 <- ifelse(predValid_model_3 == 0, 0, 1)
true_binary_3 <- ifelse(ValidSet_model_3$y_model_3 == 0, 0, 1)
mean(pred_binary_3 == ValidSet_model_3$y_model_3)

table(pred_binary_3, true_binary_3)

# Calculate the ROC curve and AUC score
roc_curve_3 <- roc(true_binary_3, pred_binary_3)
auc_score_3 <- auc(roc_curve_3)

# Plot the ROC curve
plot(roc_curve_3, main = "ROC Curve for Random Forest Model")
text(0.5, 0.3, paste("AUC =", round(auc_score_3, 3)))




install.packages("precrec")
library("precrec")

# Calculate ROC and precision-recall
curves <- evalmod(scores = pred_binary_3, labels = true_binary_3)

# Plot with ROC and precision-recall
autoplot(curves, curvetype = c("ROC"), show_cb = FALSE)
text(0.5, 0.3, paste("AUC =", round(auc_score_3, 3)))

library(ggplot2)

ggroc(roc_curve_3, colour = 'steelblue', size = 2) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc_score_3, ')'))








install.packages("ROCR")
library(ROCR)
pred <- prediction(pred_binary_3, true_binary_3)
perf <- performance(pred,"tpr",  x.measure = seq(0, 1, length.out = 200))
plot(perf, col="blue")

