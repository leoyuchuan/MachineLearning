library(rpart)
library(neuralnet)
library(e1071)

hw2 <-
  read.table(
    "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data",
    sep = ","
  )
hw2$V1 <- NULL
names(hw2) <-
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

hw2[which(hw2[, 10] == 2), 10] = 0
hw2[which(hw2[, 10] == 4), 10] = 1

hw2 = hw2[-which(duplicated(hw2) == TRUE), ]
rownames(hw2) = 1:nrow(hw2)

tmpSet = as.integer(hw2$bare_nuclei[which(hw2$bare_nuclei != "?")])
hw2$bare_nuclei[which(hw2$bare_nuclei == "?")] = round(mean(tmpSet))
hw2$bare_nuclei = as.integer(hw2$bare_nuclei)

maxs = apply(hw2, MARGIN = 2, max)
mins = apply(hw2, MARGIN = 2, min)

hw2 = as.data.frame(scale(hw2, center = mins, scale = maxs - mins))

rm(maxs, mins, tmpSet)

if (TRUE) {
  ###### Decision Tree ######
  cat("Experiment Runs for getting best parameter for Decision Tree classifier:\n")
  cat(sprintf("%s, %s, %s, %s\n", "Run#", "K-fold", "CP", "xerror"))
  count = 1
  for (i in 1:10) {
    dtree1 = rpart(
      class ~ .,
      method = "class",
      data = hw2,
      control = rpart.control(cp = 0.0000001, xval = 5),
      parms = list(split = "information")
    )
    minXerror = dtree1$cptable[which.min(dtree1$cptable[, "xerror"]), "xerror"]
    bestcp <-
      dtree1$cptable[which.min(dtree1$cptable[, "xerror"]), "CP"]
    cat(sprintf("%2d, %2d, %f, %f\n", count, 5, bestcp, minXerror))
    count = count + 1
  }
  
  for (i in 1:10) {
    dtree1 = rpart(
      class ~ .,
      method = "class",
      data = hw2,
      control = rpart.control(cp = 0.0000001, xval = 10),
      parms = list(split = "information")
    )
    minXerror = dtree1$cptable[which.min(dtree1$cptable[, "xerror"]), "xerror"]
    bestcp <-
      dtree1$cptable[which.min(dtree1$cptable[, "xerror"]), "CP"]
    cat(sprintf("%2d, %2d, %f, %f\n", count, 10, bestcp, minXerror))
    count = count + 1
  }
  
  for (i in 1:10) {
    dtree1 = rpart(
      class ~ .,
      method = "class",
      data = hw2,
      control = rpart.control(cp = 0.0000001, xval = 20),
      parms = list(split = "information")
    )
    minXerror = dtree1$cptable[which.min(dtree1$cptable[, "xerror"]), "xerror"]
    bestcp <-
      dtree1$cptable[which.min(dtree1$cptable[, "xerror"]), "CP"]
    cat(sprintf("%2d, %2d, %f, %f\n", count, 20, bestcp, minXerror))
    count = count + 1
  }
  rm(bestcp, count, dtree1 , minXerror, i)
  cat("\n")
}

if (TRUE) {
  ###### Perceptron ######
  cat("Experiment Runs for getting best parameter for Perceptron:\n")
  cat(
    sprintf(
      "%s, %s, %s, %s, %s\n",
      "Run#",
      "Ratio",
      "TrainError",
      "LearningRate",
      "Test Accuracy"
    )
  )
  
  
  n = names(hw2)
  f = as.formula(paste("class ~", paste(n[!n %in% "class"], collapse = " + ")))
  signFunc = function(x) {
    if (x > 0)
      return(1)
    else
      return(0)
  }
  
  count = 1
  trainDataIndex = sample(1:nrow(hw2), size = nrow(hw2) * 0.8)
  trainData = hw2[trainDataIndex,]
  testData = hw2[-trainDataIndex,]
  for (i in 1:20) {
    perceptron = neuralnet(
      f,
      data = trainData,
      hidden = 0,
      learningrate = 0.0001 * i,
      algorithm = "backprop",
      rep = 5
    )
    
    trainPred = compute(perceptron, trainData[1:9])$net.result[, 1]
    trainReal = trainData[, 10]
    names(trainReal) = rownames(trainData)
    trainPred = sapply(trainPred, FUN = signFunc)
    trainError = length(which(trainPred != trainReal)) / length(trainReal)
    
    
    testPred = compute(perceptron, testData[1:9])$net.result[, 1]
    testReal = testData[, 10]
    names(testReal) = rownames(testData)
    testPred = sapply(testPred, FUN = signFunc)
    testAccuracy = length(which(testPred == testReal)) / length(testReal)
    
    cat(
      sprintf(
        "%2d, %s, %f, %f, %f\n",
        count,
        "80/20",
        trainError,
        i * 0.0001,
        testAccuracy
      )
    )
    count = count + 1
  }
  cat("\n")
  rm(
    n,
    i,
    testData,
    trainData,
    count,
    f,
    testAccuracy,
    testPred,
    testReal,
    trainDataIndex,
    trainError,
    trainPred,
    trainReal,
    signFunc,
    perceptron
  )
}

if (TRUE) {
  ###### NeuralNet ######
  cat("Experiment Runs for getting best parameter for NeuralNet:\n")
  cat(
    sprintf(
      "%s, %s, %s, %s, %s\n",
      "Run#",
      "Ratio",
      "HiddenLayers",
      "Error",
      "Test Accuracy"
    )
  )
  
  
  n = names(hw2)
  f = as.formula(paste("class ~", paste(n[!n %in% "class"], collapse = " + ")))
  signFunc = function(x) {
    if (x > 0)
      return(1)
    else
      return(0)
  }
  
  count = 1
  trainDataIndex = sample(1:nrow(hw2), size = nrow(hw2) * 0.8)
  trainData = hw2[trainDataIndex,]
  testData = hw2[-trainDataIndex,]
  for (i in 1:5) {
    nn = neuralnet(f,
                   data = trainData,
                   hidden = i,
                   rep = 5)
    
    trainPred = compute(nn, trainData[1:9])$net.result[, 1]
    trainReal = trainData[, 10]
    names(trainReal) = rownames(trainData)
    trainPred = sapply(trainPred, FUN = signFunc)
    trainError = length(which(trainPred != trainReal)) / length(trainReal)
    
    
    testPred = compute(nn, testData[1:9])$net.result[, 1]
    testReal = testData[, 10]
    names(testReal) = rownames(testData)
    testPred = sapply(testPred, FUN = signFunc)
    testAccuracy = length(which(testPred == testReal)) / length(testReal)
    
    cat(sprintf(
      "%2d, %s, %d, %f, %f\n",
      count,
      "80/20",
      i,
      trainError,
      testAccuracy
    ))
    count = count + 1
  }
  
  for (i in 2:3) {
    nn = neuralnet(f,
                   data = trainData,
                   hidden = c(5, i),
                   rep = 5)
    
    trainPred = compute(nn, trainData[1:9])$net.result[, 1]
    trainReal = trainData[, 10]
    names(trainReal) = rownames(trainData)
    trainPred = sapply(trainPred, FUN = signFunc)
    trainError = length(which(trainPred != trainReal)) / length(trainReal)
    
    
    testPred = compute(nn, testData[1:9])$net.result[, 1]
    testReal = testData[, 10]
    names(testReal) = rownames(testData)
    testPred = sapply(testPred, FUN = signFunc)
    testAccuracy = length(which(testPred == testReal)) / length(testReal)
    
    cat(sprintf(
      "%2d, %s, %s, %f, %f\n",
      count,
      "80/20",
      paste("c(5,", i, ")"),
      trainError,
      testAccuracy
    ))
    count = count + 1
  }
  
  nn = neuralnet(f,
                 data = trainData,
                 hidden = c(4, 2),
                 rep = 5)
  
  trainPred = compute(nn, trainData[1:9])$net.result[, 1]
  trainReal = trainData[, 10]
  names(trainReal) = rownames(trainData)
  trainPred = sapply(trainPred, FUN = signFunc)
  trainError = length(which(trainPred != trainReal)) / length(trainReal)
  
  
  testPred = compute(nn, testData[1:9])$net.result[, 1]
  testReal = testData[, 10]
  names(testReal) = rownames(testData)
  testPred = sapply(testPred, FUN = signFunc)
  testAccuracy = length(which(testPred == testReal)) / length(testReal)
  
  cat(sprintf(
    "%2d, %s, %s, %f, %f\n",
    count,
    "80/20",
    "c(4,2)",
    trainError,
    testAccuracy
  ))
  count = count + 1
  
  nn = neuralnet(f,
                 data = trainData,
                 hidden = c(3, 1),
                 rep = 5)
  
  trainPred = compute(nn, trainData[1:9])$net.result[, 1]
  trainReal = trainData[, 10]
  names(trainReal) = rownames(trainData)
  trainPred = sapply(trainPred, FUN = signFunc)
  trainError = length(which(trainPred != trainReal)) / length(trainReal)
  
  
  testPred = compute(nn, testData[1:9])$net.result[, 1]
  testReal = testData[, 10]
  names(testReal) = rownames(testData)
  testPred = sapply(testPred, FUN = signFunc)
  testAccuracy = length(which(testPred == testReal)) / length(testReal)
  
  cat(sprintf(
    "%2d, %s, %s, %f, %f\n",
    count,
    "80/20",
    "c(3,1)",
    trainError,
    testAccuracy
  ))
  count = count + 1
  
  rm(
    testData,
    trainData,
    count,
    f,
    i,
    n,
    nn,
    testAccuracy,
    testPred,
    testReal,
    trainDataIndex,
    trainError,
    trainPred,
    trainReal,
    signFunc
  )
  
  cat("\n")
}

if (TRUE) {
  ###### SVM ######
  cat("Experiment Runs for getting best parameter for SVM:\n")
  cat(
    sprintf(
      "%s, %s, %s, %s, %s, %s, %s\n",
      "Run#",
      "Ratio",
      "cost",
      "kernel",
      "Gamma",
      "TrainError",
      "Test Accuracy"
    )
  )
  signFunc = function(x) {
    if (x > 0)
      return(1)
    else
      return(0)
  }
  
  count = 1
  trainDataIndex = sample(1:nrow(hw2), size = nrow(hw2) * 0.8)
  trainData = hw2[trainDataIndex,]
  testData = hw2[-trainDataIndex,]
  
  for (i in 1:2) {
    svm = svm(
      class ~ .,
      data = trainData,
      cost =  10 ^ i,
      gamma = 1,
      kernel = "linear"
    )
    trainPred = predict(svm, trainData[1:9])
    trainReal = trainData[, 10]
    names(trainReal) = rownames(trainData)
    trainPred = sapply(trainPred, FUN = signFunc)
    trainError = length(which(trainPred != trainReal)) / length(trainReal)
    
    
    testPred = predict(svm, testData[1:9])
    testReal = testData[, 10]
    names(testReal) = rownames(testData)
    testPred = sapply(testPred, FUN = signFunc)
    testAccuracy = length(which(testPred == testReal)) / length(testReal)
    
    cat(
      sprintf(
        "%2d, %s, %d, %s, %d, %f, %f\n",
        count,
        "80/20",
        10 ^ i,
        "linear",
        1,
        trainError,
        testAccuracy
      )
    )
    count = count + 1
  }
  
  for (i in 1:2) {
    svm = svm(
      class ~ .,
      data = trainData,
      cost = 10 ^ i,
      gamma = 1,
      kernel = "radial"
    )
    trainPred = predict(svm, trainData[1:9])
    trainReal = trainData[, 10]
    names(trainReal) = rownames(trainData)
    trainPred = sapply(trainPred, FUN = signFunc)
    trainError = length(which(trainPred != trainReal)) / length(trainReal)
    
    
    testPred = predict(svm, testData[1:9])
    testReal = testData[, 10]
    names(testReal) = rownames(testData)
    testPred = sapply(testPred, FUN = signFunc)
    testAccuracy = length(which(testPred == testReal)) / length(testReal)
    
    cat(
      sprintf(
        "%2d, %s, %d, %s, %d, %f, %f\n",
        count,
        "80/20",
        10 ^ i,
        "radial",
        1,
        trainError,
        testAccuracy
      )
    )
    count = count + 1
  }
  
  for (i in 1:2) {
    svm = svm(
      class ~ .,
      data = trainData,
      cost =  10 ^ i,
      gamma = 1,
      kernel = "sigmoid"
    )
    trainPred = predict(svm, trainData[1:9])
    trainReal = trainData[, 10]
    names(trainReal) = rownames(trainData)
    trainPred = sapply(trainPred, FUN = signFunc)
    trainError = length(which(trainPred != trainReal)) / length(trainReal)
    
    
    testPred = predict(svm, testData[1:9])
    testReal = testData[, 10]
    names(testReal) = rownames(testData)
    testPred = sapply(testPred, FUN = signFunc)
    testAccuracy = length(which(testPred == testReal)) / length(testReal)
    
    cat(
      sprintf(
        "%2d, %s, %d, %s, %d, %f, %f\n",
        count,
        "80/20",
        10 ^ i,
        "sigmoid",
        1,
        trainError,
        testAccuracy
      )
    )
    count = count + 1
  }
  
  
  
  
  
  
  
  
  
  
  
  for (i in 1:2) {
    svm = svm(
      class ~ .,
      data = trainData,
      cost =  10 ^ i,
      gamma = 0.1,
      kernel = "linear"
    )
    trainPred = predict(svm, trainData[1:9])
    trainReal = trainData[, 10]
    names(trainReal) = rownames(trainData)
    trainPred = sapply(trainPred, FUN = signFunc)
    trainError = length(which(trainPred != trainReal)) / length(trainReal)
    
    
    testPred = predict(svm, testData[1:9])
    testReal = testData[, 10]
    names(testReal) = rownames(testData)
    testPred = sapply(testPred, FUN = signFunc)
    testAccuracy = length(which(testPred == testReal)) / length(testReal)
    
    cat(
      sprintf(
        "%2d, %s, %d, %s, %f, %f, %f\n",
        count,
        "80/20",
        10 ^ i,
        "linear",
        0.1,
        trainError,
        testAccuracy
      )
    )
    count = count + 1
  }
  
  for (i in 1:2) {
    svm = svm(
      class ~ .,
      data = trainData,
      cost = 10 ^ i,
      gamma = 0.1,
      kernel = "radial"
    )
    trainPred = predict(svm, trainData[1:9])
    trainReal = trainData[, 10]
    names(trainReal) = rownames(trainData)
    trainPred = sapply(trainPred, FUN = signFunc)
    trainError = length(which(trainPred != trainReal)) / length(trainReal)
    
    
    testPred = predict(svm, testData[1:9])
    testReal = testData[, 10]
    names(testReal) = rownames(testData)
    testPred = sapply(testPred, FUN = signFunc)
    testAccuracy = length(which(testPred == testReal)) / length(testReal)
    
    cat(
      sprintf(
        "%2d, %s, %d, %s, %f, %f, %f\n",
        count,
        "80/20",
        10 ^ i,
        "radial",
        0.1,
        trainError,
        testAccuracy
      )
    )
    count = count + 1
  }
  
  for (i in 1:2) {
    svm = svm(
      class ~ .,
      data = trainData,
      cost =  10 ^ i,
      gamma = 0.1,
      kernel = "sigmoid"
    )
    trainPred = predict(svm, trainData[1:9])
    trainReal = trainData[, 10]
    names(trainReal) = rownames(trainData)
    trainPred = sapply(trainPred, FUN = signFunc)
    trainError = length(which(trainPred != trainReal)) / length(trainReal)
    
    
    testPred = predict(svm, testData[1:9])
    testReal = testData[, 10]
    names(testReal) = rownames(testData)
    testPred = sapply(testPred, FUN = signFunc)
    testAccuracy = length(which(testPred == testReal)) / length(testReal)
    
    cat(
      sprintf(
        "%2d, %s, %d, %s, %f, %f, %f\n",
        count,
        "80/20",
        10 ^ i,
        "sigmoid",
        0.1,
        trainError,
        testAccuracy
      )
    )
    count = count + 1
  }
  rm(testData, trainData, count, i, svm, testAccuracy, testPred, testReal, trainDataIndex, trainError, trainPred, trainReal, signFunc)
  cat("\n")
}


if (TRUE) {
  ###### Naive Bayes ######
  cat("Experiment Runs for getting best parameter for Naive Bayes:\n")
  cat(
    sprintf(
      "%s, %s, %s, %s, %s\n",
      "Run#",
      "Ratio",
      "laplace",
      "TrainError",
      "Test Accuracy"
    )
  )
  
  for(i in 1:10){
    hw2[i] = factor(hw2[,i])
  }
  
  count = 1
  trainDataIndex = sample(1:nrow(hw2), size = nrow(hw2) * 0.8)
  trainData = hw2[trainDataIndex,]
  testData = hw2[-trainDataIndex,]
  
  for (i in 0:10) {
    nb = naiveBayes(class~., data = trainData, laplace = i*1)
    trainPred =predict(nb, trainData[1:9])
    trainReal = trainData[, 10]
    trainError = length(which(trainPred != trainReal)) / length(trainReal)
    
    
    testPred = predict(nb, testData[1:9])
    testReal = testData[, 10]
    testAccuracy = length(which(testPred == testReal)) / length(testReal)
    
    cat(
      sprintf(
        "%2d, %s, %d, %f, %f\n",
        count,
        "80/20",
        i*1,
        trainError,
        testAccuracy
      )
    )
    count = count + 1
  }
  rm(list=ls())
  
  cat("\n")
}