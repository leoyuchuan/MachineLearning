library(h2o)
library(data.table)
library(Metrics)
options(warn = -1)
###### Read Data Path ######
args = commandArgs(trailingOnly = TRUE)

if(length(args)<1){
  stop("Please provide path of submission_test.csv")
}

path = paste(dirname(file.path(args[1])), "/", sep = "")

###### Random Forest ######

###### Initialize h2o cluster/ Build model/ Predict ###### 
invisible(h2o.init(nthreads = -1, max_mem_size = '4g'))

trainData = h2o.importFile(paste(path, 'pp_train.csv', sep = ''))
testData = h2o.importFile(paste(path, 'submission_test.csv', sep = ''))

model <- h2o.randomForest(y=7, x=1:6, training_frame = trainData, ntrees = 100, mtries = -1, max_depth = 3, nfolds = 10)
pred.test <- as.data.frame(h2o.predict(model, testData))

###### Shut down h2o cluster ######
invisible(h2o.shutdown(prompt = FALSE))
rm(testData, trainData, model)

###### Generate Submission ######
actual.test = fread(paste(path, 'submission_test.csv', sep = ''))
actual.test = cbind(actual.test, pred.test)

rm(pred.test)
invisible(gc())

setkey(actual.test, 'predict')
pred.test <- actual.test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
invisible(gc())
setkey(pred.test,"display_id")

rm(actual.test)
fwrite(pred.test, file = 'submission_rf.csv')
rm(pred.test)
invisible(gc())















###### Regression ######

###### Initialize h2o cluster/ Build model/ Predict ###### 
invisible(h2o.init(nthreads = -1, max_mem_size = '4g'))

trainData = h2o.importFile(paste(path, 'pp_train.csv', sep = ''))
testData = h2o.importFile(paste(path, 'submission_test.csv', sep = ''))

model <- h2o.glm(y=7, x=1:6, training_frame = trainData, family = "gaussian", nfolds = 10)
pred.test <- as.data.frame(h2o.predict(model, testData))

###### Shut down h2o cluster ######
invisible(h2o.shutdown(prompt = FALSE))
rm(testData, trainData, model)

###### Generate Submission ######
actual.test = fread(paste(path, 'submission_test.csv', sep = ''))
actual.test = cbind(actual.test, pred.test)

rm(pred.test)
invisible(gc())

setkey(actual.test, 'predict')
pred.test <- actual.test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
invisible(gc())
setkey(pred.test,"display_id")

rm(actual.test)
fwrite(pred.test, file = 'submission_regression.csv')
rm(pred.test)
invisible(gc())










###### Gradient Boosting ######

###### Initialize h2o cluster/ Build model/ Predict ###### 
invisible(h2o.init(nthreads = -1, max_mem_size = '4g'))

trainData = h2o.importFile(paste(path, 'pp_train.csv', sep = ''))
testData = h2o.importFile(paste(path, 'submission_test.csv', sep = ''))

model <- h2o.gbm(y=7, x=1:6, training_frame = trainData, ntrees = 100, max_depth = 4, sample_rate = 0.8, nfolds = 10)
pred.test <- as.data.frame(h2o.predict(model, testData))

###### Shut down h2o cluster ######
invisible(h2o.shutdown(prompt = FALSE))
rm(testData, trainData, model)

###### Generate Submission ######
actual.test = fread(paste(path, 'submission_test.csv', sep = ''))
actual.test = cbind(actual.test, pred.test)

rm(pred.test)
invisible(gc())

setkey(actual.test, 'predict')
pred.test <- actual.test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
invisible(gc())
setkey(pred.test,"display_id")

rm(actual.test)
fwrite(pred.test, file = 'submission_gb.csv')
rm(pred.test)
invisible(gc())














###### Deep Learning ######

###### Initialize h2o cluster/ Build model/ Predict ###### 
invisible(h2o.init(nthreads = -1, max_mem_size = '4g'))

trainData = h2o.importFile(paste(path, 'pp_train.csv', sep = ''))
testData = h2o.importFile(paste(path, 'submission_test.csv', sep = ''))

model <- h2o.deeplearning(y=7, x=1:6, training_frame = trainData, hidden = c(20,20), epochs = 20, activation = 'Rectifier', nfolds = 10)
pred.test <- as.data.frame(h2o.predict(model, testData))

###### Shut down h2o cluster ######
invisible(h2o.shutdown(prompt = FALSE))
rm(testData, trainData, model)

###### Generate Submission ######
actual.test = fread(paste(path, 'submission_test.csv', sep = ''))
actual.test = cbind(actual.test, pred.test)

rm(pred.test)
invisible(gc())

setkey(actual.test, 'predict')
pred.test <- actual.test[,.(ad_id=paste(rev(ad_id),collapse=" ")),by=display_id]
invisible(gc())
setkey(pred.test,"display_id")

rm(actual.test)
fwrite(pred.test, file = 'submission_dl.csv')
rm(pred.test)
invisible(gc())

