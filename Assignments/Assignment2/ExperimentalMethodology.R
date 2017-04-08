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

signFunc = function(x) {
  if (x > 0)
    return(1)
  else
    return(0)
}

###### Experimental Methodology ######
cat(sprintf("%s, %s, %s, %s, %s, %s, %s\n", "Sample#", "Ratio", "DTree", "Perceptron", "NeuralNet", "SVM", "NaiveBayes"))
for(numSample in 1:5){
  trainDataIndex = sample(1:nrow(hw2), size = nrow(hw2) * 0.8)
  trainData = hw2[trainDataIndex,]
  testData = hw2[-trainDataIndex,]
  
  ###### Decision Tree ######
  dtree1 = rpart(
    class ~ .,
    method = "class",
    data = hw2,
    control = rpart.control(cp = 0.0000001, xval = 5),
    parms = list(split = "information")
  )
  dtree1 = prune(dtree1, cp = 0.004296)
  pred = predict(dtree1, testData, type = "c")
  real = factor(testData[,10])
  accuracy.dtree = (length(which(real==pred)))/(length(real))

  ###### Perceptron ######
  n = names(hw2)
  f = as.formula(paste("class ~", paste(n[!n %in% "class"], collapse = " + ")))
  perceptron = neuralnet(
    f,
    data = trainData,
    hidden = 0,
    learningrate = 0.002,
    algorithm = "backprop",
    rep = 5
  )

  testPred = compute(perceptron, testData[1:9])$net.result[, 1]
  testReal = testData[, 10]
  names(testReal) = rownames(testData)
  testPred = sapply(testPred, FUN = signFunc)
  accuracy.perceptron = length(which(testPred == testReal)) / length(testReal)
  
  ###### NeuralNet ######
  
  nn = neuralnet(f,
                 data = trainData,
                 hidden = c(5,3),
                 rep = 5)
  testPred = compute(nn, testData[1:9])$net.result[, 1]
  testReal = testData[, 10]
  names(testReal) = rownames(testData)
  testPred = sapply(testPred, FUN = signFunc)
  accuracy.nn = length(which(testPred == testReal)) / length(testReal)
  
  ###### SVM ######
  svm = svm(
    class ~ .,
    data = trainData,
    cost =  10,
    gamma = 0.1,
    kernel = "radial"
  )
  
  testPred = predict(svm, testData[1:9])
  testReal = testData[, 10]
  names(testReal) = rownames(testData)
  testPred = sapply(testPred, FUN = signFunc)
  accuracy.svm = length(which(testPred == testReal)) / length(testReal)
  
  ###### Naive Bayes ######
  tmpTrain = trainData
  tmpTest = testData
  for(i in 1:10){
    tmpTrain[i] = factor(tmpTrain[,i])
    tmpTest[i] = factor(tmpTest[,i])
  }
  
  nb = naiveBayes(class~., data = tmpTrain, laplace = 1)
  testPred = predict(nb, tmpTest[1:9])
  testReal = tmpTest[, 10]
  accuracy.nb = length(which(testPred == testReal)) / length(testReal)
  
  cat(sprintf("%2d, %s, %2.3f%%, %2.3f%%, %2.3f%%, %2.3f%%, %2.3f%%\n", numSample, "80/20", accuracy.dtree*100, accuracy.perceptron*100, accuracy.nn*100, accuracy.svm*100, accuracy.nb*100))
  
}

