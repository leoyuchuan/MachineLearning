hw2<- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", sep=",")
hw2$V1 <- NULL
names(hw2) <- c("clump_thickness", "cell_size_uniformity", "cell_shape_uniformity", "marginal_adhesion", "single_epithelial_cell_size", "bare_nuclei", "bland_chromatin", "normal_nucleoli", "mitoses", "class")

hw2[which(hw2[,10]==2),10] = 0
hw2[which(hw2[,10]==4),10] = 1

hw2 = hw2[-which(duplicated(hw2)==TRUE),]
rownames(hw2) = 1:nrow(hw2)

tmpSet = as.integer(hw2$bare_nuclei[which(hw2$bare_nuclei!="?")])
hw2$bare_nuclei[which(hw2$bare_nuclei=="?")] = round(mean(tmpSet))
hw2$bare_nuclei = as.integer(hw2$bare_nuclei)

maxs = apply(hw2, MARGIN = 2, max)
mins = apply(hw2, MARGIN = 2, min)

hw2 = as.data.frame(scale(hw2, center = mins, scale = maxs - mins))

rm(maxs, mins)



library(corrplot)
hw2_M <-cor(hw2)
corrplot(hw2_M, method = "circle")

for(i in 1:9){
  hist(hw2[,i], main = paste("Histogram of", names(hw2)[i]), xlab = "x")
}
rm(i)