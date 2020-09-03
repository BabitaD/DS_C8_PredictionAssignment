---
title: "Prediction Assignment"
author: "Babita"
date: "4 September 2020"
output: 
  html_document: 
    keep_md: yes
---



## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.


```r
library(RCurl)
library(caret)
```


## Downloading Data

```r
setwd("F:\\Work\\Techwalnut\\DS And ML\\Courses\\Coursera\\DS Specialization _JHU\\Course8_Practical_ML\\Week4\\Peer Assignment\\DS_C8_PredictionAssignment")

if (!file.exists("./pml-training.csv")) {
  url.training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  download.file(url.training, destfile = "./pml-training.csv")
}

if (!file.exists("./pml-testing.csv")) {
  url.testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
  download.file(url.testing, destfile = "./pml-testing.csv")
}
```

## Reading Data

```r
trainData<- read.csv("./pml-training.csv")
testData<- read.csv("./pml-testing.csv")
```


### Data Preprocessing
Removing N/As and unuseful columns

```r
trainData <- trainData[, colSums(is.na(trainData)) == 0] 
testData <- testData[, colSums(is.na(testData)) == 0] 
classe <- trainData$classe
trainR <- grepl("^X|timestamp|window", names(trainData))
trainData <- trainData[, !trainR]
trainM <- trainData[, sapply(trainData, is.numeric)]
trainM$classe <- classe
testR <- grepl("^X|timestamp|window", names(testData))
testData<- testData[, !testR]
testM <- testData[, sapply(testData, is.numeric)]
```

### Spliting Data into train and test

```r
set.seed(12345) 
inTrain <- createDataPartition(trainM$classe, p=0.70, list=F)
train_data <- trainM[inTrain, ]
test_data <- trainM[-inTrain, ]
```

### Random Forest Model

```r
setting <- trainControl(method="cv", 5)
RandomForest <- train(classe ~ ., data=train_data, method="rf", trControl=setting,
                      ntree = 100)
RandomForest
```

```
## Random Forest 
## 
## 13737 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 10987, 10990, 10990, 10991, 10990 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.9912639  0.9889488
##   27    0.9895169  0.9867389
##   52    0.9847859  0.9807534
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 2.
```


### Accuracy and out-of-sample error



```r
predict_RandomForest <- predict(RandomForest, test_data)
confusionMatrix(test_data$classe, predict_RandomForest)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1673    1    0    0    0
##          B    5 1133    1    0    0
##          C    0    4 1022    0    0
##          D    0    0   24  938    2
##          E    0    0    0    1 1081
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9935          
##                  95% CI : (0.9911, 0.9954)
##     No Information Rate : 0.2851          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9918          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9970   0.9956   0.9761   0.9989   0.9982
## Specificity            0.9998   0.9987   0.9992   0.9947   0.9998
## Pos Pred Value         0.9994   0.9947   0.9961   0.9730   0.9991
## Neg Pred Value         0.9988   0.9989   0.9949   0.9998   0.9996
## Prevalence             0.2851   0.1934   0.1779   0.1596   0.1840
## Detection Rate         0.2843   0.1925   0.1737   0.1594   0.1837
## Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
## Balanced Accuracy      0.9984   0.9972   0.9876   0.9968   0.9990
```

```r
accuracy <- postResample(predict_RandomForest, test_data$classe)
error<-1 - as.numeric(confusionMatrix(test_data$classe, predict_RandomForest)$overall[1])
error
```

```
## [1] 0.006457094
```

Accuracy: 99.3542906%

out-of-sample error: 0.6457094%

### Prediction on test data


```r
predict(RandomForest, testM)
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

