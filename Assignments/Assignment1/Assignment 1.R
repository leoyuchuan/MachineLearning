######################################################################################################
######################################Load and process arguments######################################
######################################################################################################
if (!exists('cmd_args'))
  cmd_args = commandArgs(trailingOnly = TRUE)
if (length(cmd_args) < 3)
  stop(
    paste(
      "The input parameters are not sufficient.",
      "Please make sure you provide",
      "('Training dataset path','Testing dataset path','Pruning factor')"
    )
  )
useID3 = TRUE
for (i in 1:length(cmd_args)) {
  if (cmd_args[i] == '-r') {
    useID3 = FALSE
    cmd_args = cmd_args[-i]
    break
  }
}

trainingDatasetPath = cmd_args[1]
testingDatasetPath = cmd_args[2]
pruningFactor = cmd_args[3]

if (!file.exists(trainingDatasetPath))
  stop("The training dataset doesn't exist. Please make sure you provide correct path.")

if (!file.exists(testingDatasetPath))
  stop("The testing dataset doesn't exist. Please make sure you provide correct path.")

if (suppressWarnings(is.na(as.numeric(pruningFactor))))
  stop("The provided pruning factor is not valid.")

pruningFactor = as.numeric(pruningFactor)

######################################################################################################
################################End of loading and processing arguments###############################
######################################################################################################


trainData = read.table(trainingDatasetPath,
                       header = TRUE,
                       blank.lines.skip = TRUE)
testData = read.table(testingDatasetPath,
                      header = TRUE,
                      blank.lines.skip = TRUE)

calcEntropy = function(dataset) {
  stopifnot(is.data.frame(dataset))
  if (nrow(dataset) == 0)
    return(0)
  positiveClass = dataset[which(dataset[, ncol(dataset)] == 1), ]
  negativeClass = dataset[which(dataset[, ncol(dataset)] == 0), ]
  nPositive = nrow(positiveClass)
  nNegative = nrow(negativeClass)
  
  pPositive = (nPositive) / (nPositive + nNegative)
  pNegative = (nNegative) / (nPositive + nNegative)
  
  if (pPositive == 0)
    pPositive = 1
  if (pNegative == 0)
    pNegative = 1
  
  ret = -pPositive * log(pPositive, 2) - pNegative * log(pNegative, 2)
  return(ret)
}
getBestIGAttr = function(dataset, candidateAttrs) {
  if (length(candidateAttrs) == 0)
    return(NULL)
  minAttr = NULL
  minEntropy = 2
  for (i in 1:length(candidateAttrs)) {
    tmpSetA = dataset[which(dataset[, candidateAttrs[i]] == 0), ]
    tmpSetB = dataset[which(dataset[, candidateAttrs[i]] == 1), ]
    tmpEntropyA = calcEntropy(tmpSetA)
    tmpEntropyB = calcEntropy(tmpSetB)
    tmpNSetA = nrow(tmpSetA)
    tmpNSetB = nrow(tmpSetB)
    tmpN = tmpNSetA + tmpNSetB
    tmpEntropy = (tmpNSetA / tmpN) * tmpEntropyA + (tmpNSetB / tmpN) * tmpEntropyB
    if (tmpEntropy < minEntropy) {
      minEntropy = tmpEntropy
      minAttr = candidateAttrs[i]
    }
  }
  return(minAttr)
}
ID3 = function(examples, attrs) {
  root = list(
    leftNode = NA,
    rightNode = NA,
    dataset = examples,
    unusedAttr = attrs,
    isLeafNode = FALSE,
    predictedClass = NA,
    splitAttr = NA
  )
  positiveClass = root$dataset[which(root$dataset[, ncol(root$dataset)] ==
                                       1), ]
  negativeClass = root$dataset[which(root$dataset[, ncol(root$dataset)] ==
                                       0), ]
  nPositive = nrow(positiveClass)
  nNegative = nrow(negativeClass)
  if (nNegative == 0 && nPositive == 0) {
    root$isLeafNode = TRUE
    root$predictedClass = sample(0:1, 1)
    return(root)
  }
  if (nNegative == 0) {
    root$isLeafNode = TRUE
    root$predictedClass = 1
    return(root)
  }
  if (nPositive == 0) {
    root$isLeafNode = TRUE
    root$predictedClass = 0
    return(root)
  }
  if (length(attrs) == 0) {
    root$isLeafNode = TRUE
    root$predictedClass = ifelse(nPositive == nNegative,
                                 sample(0:1, 1),
                                 ifelse(nPositive >= nNegative, 1, 0))
    return(root)
  }
  
  root$splitAttr = getBestIGAttr(dataset = examples, candidateAttrs = attrs)
  remainingAttrs = attrs[-which(attrs == root$splitAttr)]
  
  
  leftDataSet = examples[which(examples[, root$splitAttr] == 0), ]
  rightDataSet = examples[which(examples[, root$splitAttr] == 1), ]
  
  
  leftChild = ID3(examples = leftDataSet, attrs = remainingAttrs)
  rightChild = ID3(examples = rightDataSet, attrs = remainingAttrs)
  
  
  root$leftNode = leftChild
  root$rightNode = rightChild
  
  return(root)
}
randomAttr = function(examples, attrs) {
  root = list(
    leftNode = NA,
    rightNode = NA,
    dataset = examples,
    unusedAttr = attrs,
    isLeafNode = FALSE,
    predictedClass = NA,
    splitAttr = NA
  )
  positiveClass = root$dataset[which(root$dataset[, ncol(root$dataset)] ==
                                       1), ]
  negativeClass = root$dataset[which(root$dataset[, ncol(root$dataset)] ==
                                       0), ]
  nPositive = nrow(positiveClass)
  nNegative = nrow(negativeClass)
  if (nNegative == 0) {
    root$isLeafNode = TRUE
    root$predictedClass = 1
    return(root)
  }
  if (nPositive == 0) {
    root$isLeafNode = TRUE
    root$predictedClass = 0
    return(root)
  }
  if (length(attrs) == 0) {
    root$isLeafNode = TRUE
    root$predictedClass = ifelse(nPositive == nNegative,
                                 sample(0:1, 1),
                                 ifelse(nPositive >= nNegative, 1, 0))
    return(root)
  }
  
  root$splitAttr = sample(attrs, 1)
  remainingAttrs = attrs[-which(attrs == root$splitAttr)]
  
  leftDataSet = examples[which(examples[, root$splitAttr] == 0), ]
  rightDataSet = examples[which(examples[, root$splitAttr] == 1), ]
  
  leftChild = randomAttr(examples = leftDataSet, attrs = remainingAttrs)
  rightChild = randomAttr(examples = rightDataSet, attrs = remainingAttrs)
  
  root$leftNode = leftChild
  root$rightNode = rightChild
  
  return(root)
}

nTreeNode = function(treeNode) {
  if (treeNode$isLeafNode)
    return(1)
  n1 = 0
  if (!is.na(treeNode)['leftNode']) {
    n1 = nTreeNode(treeNode$leftNode)
  }
  n2 = 0
  if (!is.na(treeNode)['rightNode']) {
    n2 = nTreeNode(treeNode$rightNode)
  }
  return(n1 + n2 + 1)
}
nLeafNode = function(treeNode) {
  if (treeNode$isLeafNode)
    return(1)
  n1 = 0
  if (!is.na(treeNode)['leftNode']) {
    n1 = nLeafNode(treeNode$leftNode)
  }
  n2 = 0
  if (!is.na(treeNode)['rightNode']) {
    n2 = nLeafNode(treeNode$rightNode)
  }
  return(n1 + n2)
}
accuracy = function(dataset, treeRoot) {
  correct = 0
  wrong = 0
  classIndex = ncol(dataset)
  nInstances = nrow(dataset)
  for (i in 1:nInstances) {
    tmpNode = treeRoot
    while (!tmpNode$isLeafNode) {
      direction = dataset[i, tmpNode$splitAttr]
      if (direction == 0) {
        if (is.na(tmpNode)['leftNode']) {
          break
        }
        tmpNode = tmpNode$leftNode
      } else{
        if (is.na(tmpNode)['rightNode']) {
          break
        }
        tmpNode = tmpNode$rightNode
      }
    }
    if (tmpNode$predictedClass == dataset[i, classIndex]) {
      correct = correct + 1
    } else{
      wrong = wrong + 1
    }
  }
  return(correct / (correct + wrong))
}

printDecisionTree = function(treeNode, level) {
  prefix = ''
  if (level == 0)
    prefix = ''
  else{
    for (i in 1:level) {
      prefix = paste(prefix, '|', sep = '')
    }
  }
  if(level == 0 && treeNode$isLeafNode){
  	cat(sprintf("%s : %d \n","Root", treeNode$predictedClass))
  	return()
  }
  
  if (is.na(treeNode)['leftNode']) {
  	cat(sprintf("%s%s = 0 : %d \n" ,
              prefix,
              treeNode$splitAttr,
              treeNode$predictedClass))
  }else{
  	if (treeNode$leftNode$isLeafNode == FALSE) {
	    cat(sprintf("%s%s = 0 : \n" ,
	              prefix,
	              treeNode$splitAttr))
	    printDecisionTree(treeNode$leftNode, level + 1)
	  } else{
	  	cat(sprintf("%s%s = 0 : %d \n" ,
	              prefix,
	              treeNode$splitAttr,
	              treeNode$leftNode$predictedClass))
	  }
  }

  if (is.na(treeNode)['rightNode']) {
  	cat(sprintf("%s%s = 1 : %d \n" ,
              prefix,
              treeNode$splitAttr,
              treeNode$predictedClass))
  }else{
  	if (treeNode$rightNode$isLeafNode == FALSE) {
	    cat(sprintf("%s%s = 1 : \n" ,
	              prefix,
	              treeNode$splitAttr))
	    printDecisionTree(treeNode$rightNode, level + 1)
	  } else{
	  	cat(sprintf("%s%s = 1 : %d \n" ,
	              prefix,
	              treeNode$splitAttr,
	              treeNode$rightNode$predictedClass))
	  }
  }
}

printSummaryAndResult = function(title, trainData, testData, treeRoot) {
  cat(sprintf(
    "--------------------------------------------------------------\n"
  ))
  cat(sprintf("%s\n", title))
  cat(sprintf(
    "--------------------------------------------------------------\n"
  ))
  
  cat(sprintf("Number of training instances = %d\n", nrow(trainData)))
  cat(sprintf("Number of training attributes = %d\n", ncol(trainData) -
                1))
  cat(sprintf(
    "Total number of nodes in the tree = %d\n",
    nTreeNode(treeRoot)
  ))
  cat(sprintf(
    "Number of leaf nodes in the tree = %d\n",
    nLeafNode(treeRoot)
  ))
  cat(sprintf(
    "Accuracy of the model on the training dataset = %.2f%%\n",
    accuracy(trainData, treeRoot) * 100
  ))
  cat(sprintf("\n"))
  cat(sprintf("Number of training instances = %d\n", nrow(testData)))
  cat(sprintf("Number of training attributes = %d\n", ncol(testData) - 1))
  cat(sprintf(
    "Accuracy of the model on the testing dataset = %.2f%%\n",
    accuracy(testData, treeRoot) * 100
  ))
  cat(sprintf(
    "--------------------------------------------------------------\n"
  ))
  cat(sprintf(
    "--------------------------------------------------------------\n"
  ))
  cat(sprintf("\n"))
}

removeLeaf = function(treeNode) {
  leftEmpty = is.na(treeNode)['leftNode']
  rightEmpty = is.na(treeNode)['rightNode']
  
  if (leftEmpty && rightEmpty) {
    return(treeNode)
  }
  
  n = sample(0:1, 1)
  
  if (leftEmpty)
    n = 1
  if (rightEmpty)
    n = 0
  
  if (n == 0) {
    if (treeNode$leftNode$isLeafNode) {
      treeNode$leftNode = NA
      positiveClass = treeNode$dataset[which(treeNode$dataset[, ncol(treeNode$dataset)] ==
                                                       1), ]
      negativeClass = treeNode$dataset[which(treeNode$dataset[, ncol(treeNode$dataset)] ==
                                                       0), ]
      nPositive = nrow(positiveClass)
      nNegative = nrow(negativeClass)
      treeNode$predictedClass = ifelse(nPositive == nNegative,
                                               sample(0:1, 1),
                                               ifelse(nPositive >= nNegative, 1, 0))
      if (is.na(treeNode)['rightNode']) {
        treeNode$isLeafNode = TRUE
      }
    } else{
      treeNode$leftNode = removeLeaf(treeNode$leftNode)
    }
  } else{
    if (treeNode$rightNode$isLeafNode) {
      treeNode$rightNode = NA
      positiveClass = treeNode$dataset[which(treeNode$dataset[, ncol(treeNode$dataset)] ==
                                                       1), ]
      negativeClass = treeNode$dataset[which(treeNode$dataset[, ncol(treeNode$dataset)] ==
                                                       0), ]
      nPositive = nrow(positiveClass)
      nNegative = nrow(negativeClass)
      treeNode$predictedClass = ifelse(nPositive == nNegative,
                                               sample(0:1, 1),
                                               ifelse(nPositive >= nNegative, 1, 0))
      if (is.na(treeNode)['leftNode']) {
        treeNode$isLeafNode = TRUE
      }
    } else{
      treeNode$rightNode = removeLeaf(treeNode$rightNode)
    }
  }
  return(treeNode)
}

pruneTree = function(root) {
  nNodes = nTreeNode(root)
  nToPrune = floor(pruningFactor * nNodes)
  for (i in 1:nToPrune) {
    root = removeLeaf(root)
  }
  return(root)
}


if (useID3) {
  decisionTreeRoot = ID3(example = trainData, attrs = names(trainData)[1:length(names(trainData)) -
                                                                         1])
} else{
  decisionTreeRoot = randomAttr(example = trainData, attrs = names(trainData)[1:length(names(trainData)) -
                                                                                1])
}

printDecisionTree(decisionTreeRoot, 0)
printSummaryAndResult("Pre-Pruned Accuracy", trainData, testData, decisionTreeRoot)

decisionTreeRoot = pruneTree(decisionTreeRoot)
printDecisionTree(decisionTreeRoot, 0)
printSummaryAndResult("Post-Pruned Accuracy", trainData, testData, decisionTreeRoot)