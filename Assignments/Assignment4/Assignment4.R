library(stats)
library(class)
library(adabag)
library(randomForest)
library(pROC)
rm(list = ls())

###### Download and Pre-process dataset ######
dataset <-
  read.table(
    "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data",
    sep = ","
  )
dataset$V1 <- NULL
names(dataset) <-
  c(
    "clump_thickness",
    "cell_size_uniformity",
    "cell_shape_uniformity",
    "marginal_adhesion",
    "single_epithelial_cell_size",
    "bare_nuclei",
    "bland_chromatin",
    "normal_nucleoli",
    "mitoses",
    "class"
  )

dataset[which(dataset[, 10] == 2), 10] = 0
dataset[which(dataset[, 10] == 4), 10] = 1

dataset = dataset[-which(duplicated(dataset) == TRUE), ]
rownames(dataset) = 1:nrow(dataset)

tmpSet = as.integer(dataset$bare_nuclei[which(dataset$bare_nuclei != "?")])
dataset$bare_nuclei[which(dataset$bare_nuclei == "?")] = round(mean(tmpSet))
dataset$bare_nuclei = as.integer(dataset$bare_nuclei)

maxs = apply(dataset, MARGIN = 2, max)
mins = apply(dataset, MARGIN = 2, min)

dataset = as.data.frame(scale(dataset, center = mins, scale = maxs - mins))

rm(maxs, mins, tmpSet)

signFunc = function(x) {
  if (x > 0)
    return(1)
  else
    return(0)
}


###### Create 10 equal size folds ######
folds <- cut(seq(1, nrow(dataset)), breaks = 10, labels = FALSE)


###### Logistic Regression ######
if (TRUE) {
  avgAUC = 0
  avgAccuracy = 0
  cat(
    "-----------------------------------------------------------------------------------\n"
  )
  cat(sprintf("Experiment#  :          Classifier,  n-Fold,    Accy,     AUC\n"))
  for (i in 1:10) {
    testIndexes <- which(folds == i, arr.ind = TRUE)
    testData <- dataset[testIndexes,]
    trainData <- dataset[-testIndexes,]
    
    glm.model <-
      glm(class ~ ., family = binomial(), data = trainData)
    glm.pred <- predict(glm.model, testData)
    glm.pred <- sapply(glm.pred, FUN = signFunc)
    accuracy <-
      length(which(testData[, 10] == glm.pred)) / length(testData[, 10])
    AUC = auc(roc(testData[, 10], glm.pred))
    
    avgAccuracy = avgAccuracy + accuracy
    avgAUC = avgAUC + AUC
  }
  avgAUC = avgAUC / 10
  avgAccuracy = avgAccuracy / 10
  
  cat(
    sprintf(
      "Experiment#%2d: Logistic Regression, 10-Fold, %.3f%%, %.3f%%\n",
      1,
      avgAccuracy * 100,
      avgAUC * 100
    )
  )
}

###### K-Nearest Neighbors ######
if (TRUE) {
  count = 0
  cat(
    "-----------------------------------------------------------------------------------\n"
  )
  cat(sprintf("Experiment#  :          Classifier,  n-Fold, kNN,    Accy,     AUC\n"))
  for (kvalue in 1:10) {
    count = count + 1
    avgAUC = 0
    avgAccuracy = 0
    for (i in 1:10)
    {
      testIndexes <- which(folds == i, arr.ind = TRUE)
      testData <- dataset[testIndexes,]
      trainData <- dataset[-testIndexes,]
      
      knn.pred <-
        knn(
          train =  trainData[, -10],
          test = testData[, -10],
          cl = trainData[, 10],
          k = kvalue,
          prob = TRUE
        )
      knn.pred = as.numeric(knn.pred) - 1
      accuracy <-
        length(which(testData[, 10] == knn.pred)) / length(testData[, 10])
      AUC = auc(roc(testData[, 10], knn.pred))
      avgAccuracy = avgAccuracy + accuracy
      avgAUC = avgAUC + AUC
    }
    
    avgAUC = avgAUC / 10
    avgAccuracy = avgAccuracy / 10
    cat(
      sprintf(
        "Experiment#%2d: k-Nearest Neighbors, 10-Fold, %3d, %.3f%%, %.3f%%\n",
        count,
        kvalue,
        avgAccuracy * 100,
        avgAUC * 100
      )
    )
  }
}

###### Bagging ######
if (TRUE) {
  count = 0
  cat(
    "-----------------------------------------------------------------------------------\n"
  )
  cat(sprintf(
    "Experiment#   : Classifier,  n-Fold, #trees, maxDepth,    Accy,     AUC\n"
  ))
  for (maxdepth in seq(3, 10, 3)) {
    for (mfinal in seq(1, 10, 3)) {
      count = count + 1
      avgAUC = 0
      avgAccuracy = 0
      for (i in 1:10)
      {
        testIndexes <- which(folds == i, arr.ind = TRUE)
        testData <- dataset[testIndexes,]
        trainData <- dataset[-testIndexes,]
        trainData$class <- as.factor(trainData$class)
        
        bagging.model <-
          bagging(
            class ~ .,
            data = trainData,
            mfinal = mfinal,
            control = rpart.control(maxdepth = maxdepth)
          )
        bagging.pred <-
          as.numeric(predict.bagging(bagging.model, newdata = testData)$class)
        
        accuracy <-
          length(which(testData[, 10] == bagging.pred)) / length(testData[, 10])
        AUC = auc(roc(testData[, 10], bagging.pred))
        
        avgAccuracy = avgAccuracy + accuracy
        avgAUC = avgAUC + AUC
      }
      
      avgAUC = avgAUC / 10
      avgAccuracy = avgAccuracy / 10
      cat(
        sprintf(
          "Experiment #%2d:    Bagging, 10-Fold, %6d, %8d, %.3f%%, %.3f%%\n",
          count,
          mfinal,
          maxdepth,
          avgAccuracy * 100,
          avgAUC * 100
        )
      )
    }
  }
}

###### Random Forest ######
if (TRUE) {
  count = 0
  cat(
    "-----------------------------------------------------------------------------------\n"
  )
  cat(sprintf("Experiment#  :    Classifier,  n-Fold, #var, #tree,    Accy,     AUC\n"))
  for (mtry in seq(1, 3, 1)) {
    for (ntree in seq(5, 70, 15)) {
      count = count + 1
      avgAUC = 0
      avgAccuracy = 0
      for (i in 1:10)
      {
        testIndexes <- which(folds == i, arr.ind = TRUE)
        testData <- dataset[testIndexes, ]
        trainData <- dataset[-testIndexes, ]
        
        rf.model <-
          randomForest(
            x = trainData[,-10],
            y = as.factor(trainData[, 10]),
            ntree = ntree,
            mtry = mtry
          )
        rf.pred = predict(rf.model, testData[,-10])
        
        accuracy <-
          length(which(testData[, 10] == rf.pred)) / length(testData[, 10])
        AUC = auc(roc(testData[, 10], as.numeric(rf.pred)))
        avgAccuracy = avgAccuracy + accuracy
        avgAUC = avgAUC + AUC
      }
      
      avgAUC = avgAUC / 10
      avgAccuracy = avgAccuracy / 10
      cat(
        sprintf(
          "Experiment#%2d: Random Forest, 10-Fold, %4d, %5d, %.3f%%, %.3f%%\n",
          count,
          mtry,
          ntree,
          avgAccuracy * 100,
          avgAUC * 100
        )
      )
    }
  }
}

###### Boosting ######
if (TRUE) {
  count = 0
  cat(
    "-----------------------------------------------------------------------------------\n"
  )
  cat(sprintf("Experiment#   : Classifier,  n-Fold, #tree,    Accy,     AUC\n"))
  for (mfinal in seq(1, 25, 3)) {
    count = count + 1
    avgAUC = 0
    avgAccuracy = 0
    for (i in 1:10)
    {
      testIndexes <- which(folds == i, arr.ind = TRUE)
      testData <- dataset[testIndexes,]
      trainData <- dataset[-testIndexes,]
      
      trainData$class = as.factor(trainData$class)
      boosting.model <-
        boosting(class ~ .,
                 data = trainData,
                 mfinal = mfinal,
                 boos = TRUE)
      boosting.pred <-
        predict.boosting(boosting.model, newdata = testData)
      
      accuracy <-
        length(which(testData[, 10] == as.numeric(boosting.pred$class))) / length(testData[, 10])
      AUC = auc(roc(testData[, 10], as.numeric(boosting.pred$class)))
      avgAccuracy = avgAccuracy + accuracy
      avgAUC = avgAUC + AUC
    }
    
    avgAUC = avgAUC / 10
    avgAccuracy = avgAccuracy / 10
    cat(
      sprintf(
        "Experiment #%2d:   Boosting, 10-Fold, %5d, %.3f%%, %.3f%%\n",
        count,
        mfinal,
        avgAccuracy*100,
        avgAUC*100
      )
    )
  }
}