setwd("C:/Users/Fujutsu/Documents/R/packeti/1")

df <- read.delim("input.txt")
m <- data.matrix(df)

n = as.integer(m[1,1])  
N = as.integer(m[2,1]) 
eps = 0.5 
eeps = 1 

lo = as.integer(1)
hi = as.integer(3)
number_of_classes_each_dimension = as.integer((hi-lo) * runif(n) + lo)


left_border = -20
right_border = 20

centres = runif(n*3, min = left_border, max = right_border)
dim(centres) <- c(n, 3)
for(i in 1:n){
  centres[i, ] <- sort(centres[i, ])
  if (abs(centres[i,2]-centres[i,1] < 20)){
    centres[i,2:3] <- runif(2, min = centres[i,2] + 10, max = right_border + 10)
  }
  centres[i, 2:3] = sort(centres[i, 2:3])
  if (abs(centres[i,3]-centres[i,2] < 20)){
    centres[i,3] <- runif(1, min = right_border + 20, max = right_border + 30)
  }
}

for(i in 1:n){
  centres[i,1:(number_of_classes_each_dimension[i])] <- sort(centres[i,1:(number_of_classes_each_dimension[i])])
}


vectors <- rep(0, times = n*N)
vectors_classes <- rep(0, times = n*N)
dim(vectors) <- c(N, n)
dim(vectors_classes) <- c(N, n)


value = 5
vectors <- t(vectors)
vectors_classes <- t(vectors_classes)

for (i in 1:n) {
  
  counter = 1
  
  if (number_of_classes_each_dimension[i] == 1) {
    number_of_points_in_first_class <- N
    vectors[i, ] <- rnorm(number_of_points_in_first_class) * value
    vectors_classes[i, ]=1
  }
  
  if (number_of_classes_each_dimension[i] == 2) {
    number_of_points_in_first_class <- N/2
    number_of_points_in_second_class <- N/2
    vectors[i, 1:number_of_points_in_first_class] <- rnorm(number_of_points_in_first_class) * value + rep(centres[i, 1],times = number_of_points_in_first_class)
    vectors_classes[i, 1:number_of_points_in_first_class]=1
    vectors[i, (number_of_points_in_first_class+1):N] <- rnorm(number_of_points_in_second_class) * value+ rep(centres[i, 2],times = number_of_points_in_second_class)
    vectors_classes[i, (number_of_points_in_first_class+1):N]=-1  
    }
  

}
vectors <- t(vectors)
vectors_classes <- t(vectors_classes)

if (n == 3) {
  op <- par(bg = "light blue")
  plot(vectors[, 1], vectors[, 2], type = "p", pch = 21, bg = par("bg"), col = "blue", cex = .6,
       main = 'points x-y', xlab = "x", ylab = "y")
  plot(vectors[, 1], vectors[, 3], type = "p", pch = 21, bg = par("bg"), col = "blue", cex = .6,
       main = 'points x-z', xlab = "x", ylab = "z")
}


df1 <- as.data.frame(t(vectors))
write.csv(t(df1), file = "VectorsData.csv", row.names = FALSE)
df2 <- as.data.frame(number_of_classes_each_dimension)
write.csv(t(df2), file = "dimensions.csv", row.names = FALSE)
df3 <- as.data.frame(t(vectors_classes))
write.csv(t(df3), file = "VectorsClasses.csv", row.names = FALSE)
