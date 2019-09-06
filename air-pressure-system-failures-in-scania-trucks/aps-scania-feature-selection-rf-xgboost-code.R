library(qcc)
library(mice)
library(caret)
library(xgboost)
library(corrplot)
library(randomForest)

# WORKING DIRECTORY
#setwd("~/Harbinger/BIR/scania")

# IMPORTING TRAINING SET (df) AND TEST SET (dt)
dTrainRaw <- read.table(file = 'aps_failure_training_set.csv', sep = ',', dec = '.',na.strings = 'na', header = T, stringsAsFactors = F)
dTestRaw <- read.table(file = 'aps_failure_test_set.csv', sep = ',', dec = '.',na.strings = 'na', header = T, stringsAsFactors = F)

## MISSING ANALYSIS
### MCAR or MNAR (?)- Given the situation from the challenge will be considered MCAR

### Define variables NA % that contains > 5% missing values and remove them! (Based on a golden rule)
#### Analyze % NA
var_pct_na <- stack(100*colSums(is.na(dTrainRaw))/nrow(dTrainRaw))
#### How many variables have more than 5% NA
passing_vars <- as.character(var_pct_na[var_pct_na$values < 5,'ind'])
#### Change df & dt to remove Misssing vars
dTrainRaw <- dTrainRaw[passing_vars]
dTestRaw <- dTestRaw[passing_vars]

### Break DF & DT into X and Y
#### TRAIN
dTrainRaw_x <- dTrainRaw[-1]
dTrainRaw_y <- dTrainRaw[1]
dTrainRaw_y$class[dTrainRaw_y$class == "neg"] <- 0
dTrainRaw_y$class[dTrainRaw_y$class == "pos"] <- 1
#### TEST
dTestRaw_x <- dTestRaw[-1]
dTestRaw_y <- dTestRaw[1]
dTestRaw_y$class[dTestRaw_y$class == "neg"] <- 0
dTestRaw_y$class[dTestRaw_y$class == "pos"] <- 1

### PS: AVOID GOLDEN RULE: ELIMINATE SAMPLES THAT >50% NA VALUES

### Data Imputation with CART Imputations - m and maxit parameters are in the lower value to speed the code
#### TRAIN - Find NA values
tempData <- mice(data = dTrainRaw_x, method = 'cart', m = 1, maxit = 1)
#### TRAIN - Insert values into the dataset
dTrainRaw_x <- complete(tempData, 1)
#### TEST - Find NA values
tempData <- mice(data = dTestRaw_x, method = 'cart', m = 1, maxit = 1)
#### Test - Insert values into the dataset
dTestRaw_x <- complete(tempData, 1)

### Data Imputation with mean column values - Apparently, some values still not imputed by mice package
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
#### TRAIN - Insert values into the dataset(remaining)
dTrainRaw_x[] <- lapply(dTrainRaw_x, NA2mean) 
#### Test - Insert values into the dataset(remaining)
dTestRaw_x[] <- lapply(dTestRaw_x, NA2mean)

### Remove Unused Vars
rm(dTrainRaw,dTestRaw, var_pct_na, passing_vars, tempData)

## CORRELATION ANALYSIS
### Calculate SD to find constant variables to DROP!
sd_analysis <- sapply(dTrainRaw_x, sd, na.rm = TRUE)
#### TRAIN SET drop constant vars
dTrainRaw_x <- dTrainRaw_x[setdiff(colnames(dTrainRaw_x), names(sd_analysis[sd_analysis == 0]))]
#### TEST SET drop constant vars
dTestRaw_x <- dTestRaw_x[setdiff(colnames(dTestRaw_x), names(sd_analysis[sd_analysis == 0]))]

### Correlation Calculation
cor_matrix <- cor(x = dTrainRaw_x, method = 'spearman', use = 'everything')

### Correlation Plot
png(height=1200, width=1200, res=300, pointsize=6, file="corrPlot.png")
corrplot(corr = cor_matrix, type = "upper", tl.cex = 0.5, tl.pos = "td", tl.col = 'black')
dev.off()

### Drop High Correlated Variables using caret package
hcv <- caret::findCorrelation(x = cor_matrix, cutoff = 0.9, verbose = T)
hcv <- sort(hcv)
#### TRAIN DROP
dTrainRaw_x <- dTrainRaw_x[,-c(hcv)]
#### TEST DROP
dTestRaw_x <- dTestRaw_x[,-c(hcv)]

### Remove Unused Vars
rm(cor_matrix, hcv, sd_analysis, NA2mean)

## PCA DIMENSIONALITY REDUCTION
### PCA Calc
pca <- prcomp(x = dTrainRaw_x, center = T, scale. = T)

### DataFrame to hold Variance Explained
var_explained = round(pca$sdev^2/sum(pca$sdev^2)*100, 2)
var_explained = data.frame(c(1:length(pca$sdev)),var_explained)
names(var_explained)[1] = 'PCs'
names(var_explained)[2] = 'Variance'

### Pareto Plot
PCA = pca$sdev^2
names(PCA) = paste0('PC', var_explained$PCs)
qcc::pareto.chart(PCA)

### Define a x based on PCs - 60% Explained Var
dTrainPCA_x <-  data.frame(pca$x)
dTrainPCA_x <-  dTrainPCA_x[1:34]

### Transform TEST SET in PC Domain
dTestPCA_x <- predict(pca, newdata = dTestRaw_x)
dTestPCA_x  <- as.data.frame(dTestPCA_x )
dTestPCA_x  <- dTestPCA_x[1:34]

### Remove Unused Vars
rm(PCA, var_explained)

## MODEL GENERATED - RANDOM FOREST
### Data transform
dTrainRF <- cbind(as.factor(dTrainRaw_y$class), dTrainPCA_x)
colnames(dTrainRF)[1] <- 'class'
dTestRF <- cbind(as.factor(dTestRaw_y$class), dTestPCA_x)
colnames(dTestRF)[1] <- 'class'

### Model Creation
rfModel <- randomForest::tuneRF(x = dTrainRF[-1], y = dTrainRF$class, doBest = T)

### Model Prediction
rfPred <- predict(rfModel, newdata = dTestRF)

### Confusion Matrix
rfCM <- confusionMatrix(data = rfPred, reference = dTestRF$class)
rfCM

### Final Result Cost RF
500*rfCM$table[1,2] + 10*rfCM$table[2,1]

## MODEL GENERATED - XGBOOST CLASSIFIER
### Data transform
dTrainXG <- xgb.DMatrix(label = as.matrix(dTrainRaw_y), data = as.matrix(dTrainPCA_x))
dTestXG <- xgb.DMatrix(label = as.matrix(dTestRaw_y), data = as.matrix(dTestPCA_x))

### Model Creation
xgbModel <- xgboost(data = dTrainXG,
                    nrounds = 10,
                    objective = "binary:logistic",
                    verbose = 1)

### Model Prediction
xgbPred <- predict(xgbModel, dTestXG)
xgbPred <- as.numeric(xgbPred > 0.5)

### Confusion Matrix
xgbCM <- confusionMatrix(data = as.factor(xgbPred), reference = as.factor(dTestRaw_y$class))
xgbCM

### Final Result Cost XGB
500*xgbCM$table[1,2] + 10*xgbCM$table[2,1]

## INFERENCE WAY BACK MACHINE!
## Inference for the most impactfull original variables in the most important PC variable for RF model
### Var Importance for RF
varImp(rfModel)

### Most high loadings for the most important variable in rf model
head(sort(pca$rotation[,1], decreasing = T),3)
