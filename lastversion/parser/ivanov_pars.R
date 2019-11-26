setwd("C:/Users/Fujutsu/Documents/R/packeti/1")

df = read.csv("data_density.csv")
data = as.matrix(df)
df1 <- as.data.frame(t(data[,3:ncol(data)]))
write.csv(t(df1), file = "VectorsData.csv", row.names = FALSE)

classes = rep(data[,2],10)
dim(classes)=c(400,10)
df2 <- as.data.frame(t(classes))
write.csv(t(df2), file = "VectorsClasses.csv", row.names = FALSE)

number_of_classes_each_dimension = rep(2,10)
df3 <- as.data.frame(number_of_classes_each_dimension)
write.csv(t(df3), file = "dimensions.csv", row.names = FALSE)
