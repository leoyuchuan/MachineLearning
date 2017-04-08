library(h2o)
library(data.table)
library(Metrics)
options(warn = -1)
###### Read Data Path ######
args = commandArgs(trailingOnly = TRUE)

if(length(args)<1){
  stop("Please provide path of training data & testing data")
}

path = paste(dirname(file.path(args[1])), "/", sep = "")

###### Random Forest ######

###### Initialize h2o cluster/ Build model/ Predict ###### 
invisible(h2o.init(nthreads = -1, max_mem_size = '4g'))

trainData = h2o.importFile(paste(path, 'pp_train.csv', sep = ''))
testData = h2o.importFile(paste(path, 'pp_test.csv', sep = ''))

model <- h2o.randomForest(y=7, x=1:6, training_frame = trainData, ntrees = 100, mtries = -1, max_depth = 3, nfolds = 10)
pred.train <- as.data.frame(h2o.predict(model, trainData))
pred.test <- as.data.frame(h2o.predict(model, testData))

###### Shut down h2o cluster ######
mse.train = h2o.performance(model)@metrics$MSE
mse.test = h2o.performance(model, testData)@metrics$MSE
invisible(h2o.shutdown(prompt = FALSE))
rm(testData, trainData, model)

###### Evaluate MAP@12 For Training Data ######
actual.train = fread(paste(path, 'pp_train.csv', sep = ''))
actual.train = cbind(actual.train, pred.train)

rm(pred.train)
invisible(gc())

setkey(actual.train, 'predict')
pred.train <- actual.train[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
invisible(gc())
setkey(pred.train,"display_id")

actual.train = actual.train[which(actual.train$clicked==1),]
actual.train <- actual.train[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(actual.train, 'display_id')

actual.train = actual.train[pred.train, on='display_id']
actual.train[, c('i.ad_id'):=NULL]

actual = strsplit(actual.train$ad_id, " ")
actual = lapply(actual, as.integer)

predicted = strsplit(pred.train$ad_id, " ")
predicted = lapply(predicted, as.integer)

rm(actual.train, pred.train)
MAP12 = mapk(12, actual, predicted)

str = sprintf("%s: Mean Squared Error For Training Data is %.4f\n", "Random Forest", mse.train)
str = paste(str, sprintf("%s: Mean Average Precision @12 For Training Data is %.4f\n", "Random Forest", MAP12), sep = "")
cat(str)
write(str, file = paste(path, "output.log", sep = ''), append = TRUE)
rm(actual, predicted, MAP12, str)

###### Evaluate MAP@12 For Testing Data ######
actual.test = fread(paste(path, 'pp_test.csv', sep = ''))
actual.test = cbind(actual.test, pred.test)

rm(pred.test)
invisible(gc())

setkey(actual.test, 'predict')
pred.test <- actual.test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
invisible(gc())
setkey(pred.test,"display_id")

actual.test = actual.test[which(actual.test$clicked==1),]
actual.test <- actual.test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(actual.test, 'display_id')

actual.test = actual.test[pred.test, on='display_id']
actual.test[, c('i.ad_id'):=NULL]

actual = strsplit(actual.test$ad_id, " ")
actual = lapply(actual, as.integer)

predicted = strsplit(pred.test$ad_id, " ")
predicted = lapply(predicted, as.integer)

rm(actual.test, pred.test)
MAP12 = mapk(12, actual, predicted)

str = sprintf("%s: Mean Squared Error For Testing Data is %.4f\n", "Random Forest", mse.test)
str = paste(str, sprintf("%s: Mean Average Precision @12 For Testing Data is %.4f\n", "Random Forest", MAP12), sep = "")
cat(str)
write(str, file = paste(path, "output.log", sep = ''), append = TRUE)
rm(actual, predicted, MAP12, str)
rm(mse.test, mse.train)

















###### Regression ######

###### Initialize h2o cluster/ Build model/ Predict ###### 
invisible(h2o.init(nthreads = -1, max_mem_size = '4g'))

trainData = h2o.importFile(paste(path, 'pp_train.csv', sep = ''))
testData = h2o.importFile(paste(path, 'pp_test.csv', sep = ''))

model <- h2o.glm(y=7, x=1:6, training_frame = trainData, family = "gaussian", nfolds = 10)
pred.train <- as.data.frame(h2o.predict(model, trainData))
pred.test <- as.data.frame(h2o.predict(model, testData))

###### Shut down h2o cluster ######
mse.train = h2o.performance(model)@metrics$MSE
mse.test = h2o.performance(model, testData)@metrics$MSE
invisible(h2o.shutdown(prompt = FALSE))
rm(testData, trainData, model)

###### Evaluate MAP@12 For Training Data ######
actual.train = fread(paste(path, 'pp_train.csv', sep = ''))
actual.train = cbind(actual.train, pred.train)

rm(pred.train)
invisible(gc())

setkey(actual.train, 'predict')
pred.train <- actual.train[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
invisible(gc())
setkey(pred.train,"display_id")

actual.train = actual.train[which(actual.train$clicked==1),]
actual.train <- actual.train[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(actual.train, 'display_id')

actual.train = actual.train[pred.train, on='display_id']
actual.train[, c('i.ad_id'):=NULL]

actual = strsplit(actual.train$ad_id, " ")
actual = lapply(actual, as.integer)

predicted = strsplit(pred.train$ad_id, " ")
predicted = lapply(predicted, as.integer)

rm(actual.train, pred.train)
MAP12 = mapk(12, actual, predicted)

str = sprintf("%s: Mean Squared Error For Training Data is %.4f\n", "Regression", mse.train)
str = paste(str, sprintf("%s: Mean Average Precision @12 For Training Data is %.4f\n", "Regression", MAP12), sep = "")
cat(str)
write(str, file = paste(path, "output.log", sep = ''), append = TRUE)
rm(actual, predicted, MAP12, str)

###### Evaluate MAP@12 For Testing Data ######
actual.test = fread(paste(path, 'pp_test.csv', sep = ''))
actual.test = cbind(actual.test, pred.test)

rm(pred.test)
invisible(gc())

setkey(actual.test, 'predict')
pred.test <- actual.test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
invisible(gc())
setkey(pred.test,"display_id")

actual.test = actual.test[which(actual.test$clicked==1),]
actual.test <- actual.test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(actual.test, 'display_id')

actual.test = actual.test[pred.test, on='display_id']
actual.test[, c('i.ad_id'):=NULL]

actual = strsplit(actual.test$ad_id, " ")
actual = lapply(actual, as.integer)

predicted = strsplit(pred.test$ad_id, " ")
predicted = lapply(predicted, as.integer)

rm(actual.test, pred.test)
MAP12 = mapk(12, actual, predicted)

str = sprintf("%s: Mean Squared Error For Testing Data is %.4f\n", "Regression", mse.test)
str = paste(str, sprintf("%s: Mean Average Precision @12 For Testing Data is %.4f\n", "Regression", MAP12), sep = "")
cat(str)
write(str, file = paste(path, "output.log", sep = ''), append = TRUE)
rm(actual, predicted, MAP12, str)
rm(mse.test, mse.train)












###### Gradient Boosting ######

###### Initialize h2o cluster/ Build model/ Predict ###### 
invisible(h2o.init(nthreads = -1, max_mem_size = '4g'))

trainData = h2o.importFile(paste(path, 'pp_train.csv', sep = ''))
testData = h2o.importFile(paste(path, 'pp_test.csv', sep = ''))

model <- h2o.gbm(y=7, x=1:6, training_frame = trainData, ntrees = 100, max_depth = 4, sample_rate = 0.8, nfolds = 10)
pred.train <- as.data.frame(h2o.predict(model, trainData))
pred.test <- as.data.frame(h2o.predict(model, testData))

###### Shut down h2o cluster ######
mse.train = h2o.performance(model)@metrics$MSE
mse.test = h2o.performance(model, testData)@metrics$MSE
invisible(h2o.shutdown(prompt = FALSE))
rm(testData, trainData, model)

###### Evaluate MAP@12 For Training Data ######
actual.train = fread(paste(path, 'pp_train.csv', sep = ''))
actual.train = cbind(actual.train, pred.train)

rm(pred.train)
invisible(gc())

setkey(actual.train, 'predict')
pred.train <- actual.train[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
invisible(gc())
setkey(pred.train,"display_id")

actual.train = actual.train[which(actual.train$clicked==1),]
actual.train <- actual.train[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(actual.train, 'display_id')

actual.train = actual.train[pred.train, on='display_id']
actual.train[, c('i.ad_id'):=NULL]

actual = strsplit(actual.train$ad_id, " ")
actual = lapply(actual, as.integer)

predicted = strsplit(pred.train$ad_id, " ")
predicted = lapply(predicted, as.integer)

rm(actual.train, pred.train)
MAP12 = mapk(12, actual, predicted)

str = sprintf("%s: Mean Squared Error For Training Data is %.4f\n", "Gradient Boosting", mse.train)
str = paste(str, sprintf("%s: Mean Average Precision @12 For Training Data is %.4f\n", "Gradient Boosting", MAP12), sep = "")
cat(str)
write(str, file = paste(path, "output.log", sep = ''), append = TRUE)
rm(actual, predicted, MAP12, str)

###### Evaluate MAP@12 For Testing Data ######
actual.test = fread(paste(path, 'pp_test.csv', sep = ''))
actual.test = cbind(actual.test, pred.test)

rm(pred.test)
invisible(gc())

setkey(actual.test, 'predict')
pred.test <- actual.test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
invisible(gc())
setkey(pred.test,"display_id")

actual.test = actual.test[which(actual.test$clicked==1),]
actual.test <- actual.test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(actual.test, 'display_id')

actual.test = actual.test[pred.test, on='display_id']
actual.test[, c('i.ad_id'):=NULL]

actual = strsplit(actual.test$ad_id, " ")
actual = lapply(actual, as.integer)

predicted = strsplit(pred.test$ad_id, " ")
predicted = lapply(predicted, as.integer)

rm(actual.test, pred.test)
MAP12 = mapk(12, actual, predicted)

str = sprintf("%s: Mean Squared Error For Testing Data is %.4f\n", "Gradient Boosting", mse.test)
str = paste(str, sprintf("%s: Mean Average Precision @12 For Testing Data is %.4f\n", "Gradient Boosting", MAP12), sep = "")
cat(str)
write(str, file = paste(path, "output.log", sep = ''), append = TRUE)
rm(actual, predicted, MAP12, str)
rm(mse.test, mse.train)















###### Deep Learning ######

###### Initialize h2o cluster/ Build model/ Predict ###### 
invisible(h2o.init(nthreads = -1, max_mem_size = '4g'))

trainData = h2o.importFile(paste(path, 'pp_train.csv', sep = ''))
testData = h2o.importFile(paste(path, 'pp_test.csv', sep = ''))

model <- h2o.deeplearning(y=7, x=1:6, training_frame = trainData, hidden = c(20,20), epochs = 20, activation = 'Rectifier', nfolds = 10)
pred.train <- as.data.frame(h2o.predict(model, trainData))
pred.test <- as.data.frame(h2o.predict(model, testData))

###### Shut down h2o cluster ######
mse.train = h2o.performance(model)@metrics$MSE
mse.test = h2o.performance(model, testData)@metrics$MSE
invisible(h2o.shutdown(prompt = FALSE))
rm(testData, trainData, model)

###### Evaluate MAP@12 For Training Data ######
actual.train = fread(paste(path, 'pp_train.csv', sep = ''))
actual.train = cbind(actual.train, pred.train)

rm(pred.train)
invisible(gc())

setkey(actual.train, 'predict')
pred.train <- actual.train[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
invisible(gc())
setkey(pred.train,"display_id")

actual.train = actual.train[which(actual.train$clicked==1),]
actual.train <- actual.train[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(actual.train, 'display_id')

actual.train = actual.train[pred.train, on='display_id']
actual.train[, c('i.ad_id'):=NULL]

actual = strsplit(actual.train$ad_id, " ")
actual = lapply(actual, as.integer)

predicted = strsplit(pred.train$ad_id, " ")
predicted = lapply(predicted, as.integer)

rm(actual.train, pred.train)
MAP12 = mapk(12, actual, predicted)

str = sprintf("%s: Mean Squared Error For Training Data is %.4f\n", "Deep Learning", mse.train)
str = paste(str, sprintf("%s: Mean Average Precision @12 For Training Data is %.4f\n", "Deep Learning", MAP12), sep = "")
cat(str)
write(str, file = paste(path, "output.log", sep = ''), append = TRUE)
rm(actual, predicted, MAP12, str)

###### Evaluate MAP@12 For Testing Data ######
actual.test = fread(paste(path, 'pp_test.csv', sep = ''))
actual.test = cbind(actual.test, pred.test)

rm(pred.test)
invisible(gc())

setkey(actual.test, 'predict')
pred.test <- actual.test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
invisible(gc())
setkey(pred.test,"display_id")

actual.test = actual.test[which(actual.test$clicked==1),]
actual.test <- actual.test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
setkey(actual.test, 'display_id')

actual.test = actual.test[pred.test, on='display_id']
actual.test[, c('i.ad_id'):=NULL]

actual = strsplit(actual.test$ad_id, " ")
actual = lapply(actual, as.integer)

predicted = strsplit(pred.test$ad_id, " ")
predicted = lapply(predicted, as.integer)

rm(actual.test, pred.test)
MAP12 = mapk(12, actual, predicted)

str = sprintf("%s: Mean Squared Error For Testing Data is %.4f\n", "Deep Learning", mse.test)
str = paste(str, sprintf("%s: Mean Average Precision @12 For Testing Data is %.4f\n", "Deep Learning", MAP12), sep = "")
cat(str)
write(str, file = paste(path, "output.log", sep = ''), append = TRUE)
rm(actual, predicted, MAP12, str)
rm(mse.test, mse.train)





