###### Read Data Path ######
args = commandArgs(trailingOnly = TRUE)

if (length(args) == 0)
  stop("Please provide path of training data.")

library(data.table)

trainData = args[1]

path = paste(dirname(file.path(trainData)), "/", sep = "")
options(warn = -1)


###### Combine Train Data with events ######
datasetTmp = fread(paste(path, 'events.csv', sep = ''))
datasetTmp[, c('uuid', 'geo_location') := NULL]
datasetTmp$platform = as.integer(datasetTmp$platform)
datasetTmp[which(is.na(datasetTmp$platform)), 'platform'] = 1

dataset = fread(paste(path, 'clicks_train.csv', sep = ''))
dataset = datasetTmp[dataset, on = 'display_id']
rm(datasetTmp)
invisible(gc())

###### Combine Train Data with Documents_meta ######
datasetTmp = fread(paste(path, 'documents_meta.csv', sep = ''))
datasetTmp[, c('publish_time', 'source_id') := NULL]
dataset = datasetTmp[dataset, on = 'document_id']
rm(datasetTmp)
invisible(gc())
dataset[which(is.na(dataset$publisher_id)), 'publisher_id'] = 0
invisible(gc())

###### Combine Train Data with Advertiser_ID ######
datasetTmp = fread(paste(path, 'promoted_content.csv', sep = ''))
datasetTmp[, c('document_id', 'campaign_id') := NULL]
dataset = datasetTmp[dataset, on = 'ad_id']
rm(datasetTmp)
invisible(gc())

###### Sample two different days from Train Data, used for training/testing ######
startTime = 1465876800000 - 1465876799998
day = sample(0:12, 2)
startTime = startTime + 86400000 * day[1]
endTime = startTime + 86400000

trainData = dataset[which(dataset$timestamp >= startTime &
                          dataset$timestamp < endTime),]
trainData[, c('timestamp') := NULL]
fwrite(trainData, 'pp_train.csv')
rm(trainData)

startTime = 1465876800000 - 1465876799998
startTime = startTime + 86400000 * day[2]
endTime = startTime + 86400000

testData = dataset[which(dataset$timestamp >= startTime &
                            dataset$timestamp < endTime),]
testData[, c('timestamp') := NULL]
fwrite(testData, 'pp_test.csv')

rm(dataset, testData, startTime, endTime, day)
invisible(gc())

###### Process test data for submission ######
datasetTmp = fread(paste(path, 'events.csv', sep = ''))
datasetTmp[, c('uuid', 'timestamp', 'geo_location') := NULL]
datasetTmp$platform = as.integer(datasetTmp$platform)
datasetTmp[which(is.na(datasetTmp$platform)), 'platform'] = 1

dataset = fread(paste(path, 'clicks_test.csv', sep = ''))
dataset = datasetTmp[dataset, on = 'display_id']
rm(datasetTmp)
invisible(gc())

datasetTmp = fread(paste(path, 'documents_meta.csv', sep = ''))
datasetTmp[, c('publish_time', 'source_id') := NULL]
dataset = datasetTmp[dataset, on = 'document_id']
rm(datasetTmp)
invisible(gc())
dataset[which(is.na(dataset$publisher_id)), 'publisher_id'] = 0
invisible(gc())

datasetTmp = fread(paste(path, 'promoted_content.csv', sep = ''))
datasetTmp[, c('document_id', 'campaign_id') := NULL]
dataset = datasetTmp[dataset, on = 'ad_id']
rm(datasetTmp)
invisible(gc())

fwrite(dataset, 'submission_test.csv')

rm(list = ls())
invisible(gc())

cat("Data have been processed.")