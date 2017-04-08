args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args) != 3) {
  stop("All three arguments must be supplied (number of clusters, image file, output file)", call.=FALSE)
} 

number_of_clusters = args[1]
image_file = args[2]
output_file = args[3]

library(jpeg)
library(ggplot2)
library(stats)

img1 <- readJPEG(image_file)

# Obtain the dimension
imgDm <- dim(img1)

# Assign RGB channels to data frame
imgRGB <- data.frame(
  x = rep(1:imgDm[2], each = imgDm[1]),
  y = rep(imgDm[1]:1, imgDm[2]),
  R = as.vector(img1[,,1]),
  G = as.vector(img1[,,2]),
  B = as.vector(img1[,,3])
)

# ggplot theme to be used
plotTheme <- function() {
  theme(
    panel.background = element_rect(
      size = 3,
      colour = "black",
      fill = "white"),
    axis.ticks = element_line(
      size = 2),
    panel.grid.major = element_line(
      colour = "gray80",
      linetype = "dotted"),
    panel.grid.minor = element_line(
      colour = "gray90",
      linetype = "dashed"),
    axis.title.x = element_text(
      size = rel(1.2),
      face = "bold"),
    axis.title.y = element_text(
      size = rel(1.2),
      face = "bold"),
    plot.title = element_text(
      size = 20,
      face = "bold",
      vjust = 1.5)
  )
}


kClusters <- as.integer(number_of_clusters)
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])

jpeg(filename = paste("./clusteredImages/", output_file, ".jpg", sep=""))

ggplot(data = imgRGB, aes(x = x, y = y)) +
  geom_point(colour = kColours) +
  labs(title = paste("Clustered Image of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y") +
  plotTheme() 

dev.off()