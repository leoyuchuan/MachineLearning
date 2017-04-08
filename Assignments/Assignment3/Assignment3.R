library(e1071)

if (!exists('cmd_args'))
  cmd_args = commandArgs(trailingOnly = TRUE)
if (length(cmd_args) < 2)
  stop(
    paste(
      "The input parameters are not sufficient.\n",
      "Please make sure you provide",
      "('Training dataset path','Testing dataset path')"
    )
  )
if(length(cmd_args) > 2){
  stop("You provide more than two parameters. \nThis program only takes two paths to training and testing dataset. \nPlease correct parameters and run again.\n")
}

trainPath = cmd_args[1]
testPath = cmd_args[2]

if (!file.exists(trainPath))
  stop("The training dataset doesn't exist. Please make sure you provide correct path.")

if (!file.exists(testPath))
  stop("The testing dataset doesn't exist. Please make sure you provide correct path.")

trainData = read.table(trainPath,
                       header = TRUE,
                       blank.lines.skip = TRUE)
testData = read.table(testPath,
                      header = TRUE,
                      blank.lines.skip = TRUE)

if(length(which(names(trainData)==names(testData)))!=length(names(trainData))){
  stop("The training dataset and the testing dataset are not consistent due to inconsistent column names.")
}


trainData[] = lapply(trainData, factor)
testData[] = lapply(testData, factor)

f = as.formula(paste(names(trainData)[length(trainData)],"~","."))
nb = naiveBayes(f, data=trainData)

className = names(trainData)[length(names(trainData))]
attrNames = names(trainData)[-length(names(trainData))]

str0 = ""
str1 = ""

py0 = nb$apriori[1]/(nb$apriori[1]+nb$apriori[2])
py1 = nb$apriori[2]/(nb$apriori[1]+nb$apriori[2])

str0 = sprintf("P(%s=0)=%f", className, py0)
str1 = sprintf("P(%s=1)=%f", className, py1)

for(i in 1:length(attrNames)){
  str0 = sprintf("%s P(%s=0|%s=0)=%f", str0, attrNames[i], className, nb$tables[[attrNames[1]]][1,1])
  str0 = sprintf("%s P(%s=1|%s=0)=%f", str0, attrNames[i], className, nb$tables[[attrNames[1]]][1,2])
  
  str1 = sprintf("%s P(%s=0|%s=1)=%f", str1, attrNames[i], className, nb$tables[[attrNames[1]]][2,1])
  str1 = sprintf("%s P(%s=1|%s=1)=%f", str1, attrNames[i], className, nb$tables[[attrNames[1]]][2,2])
}

cat(paste(str0, "\n"))
cat("\n")
cat(paste(str1, "\n"))
cat("\n")


predTrain = factor(predict(nb, trainData))
predTest = factor(predict(nb, testData))

realTrain = trainData[,length(trainData)]
realTest = testData[,length(testData)]

cat(sprintf("Accuracy on training set (%d instances): %.1f%%\n", length(realTrain),length(which(realTrain==predTrain))/length(realTrain)*100))
cat("\n")
cat(sprintf("Accuracy on testing set (%d instances): %.1f%%\n", length(realTest),length(which(realTest==predTest))/length(realTest)*100))
cat("\n")
