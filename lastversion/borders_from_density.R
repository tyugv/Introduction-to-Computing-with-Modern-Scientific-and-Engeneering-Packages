setwd("C:/Users/Fujutsu/Documents/R/packeti/1")

#функция для нахождения пересечений
calculate_intersections <- function(points1,points2){ 
  dif = points1 - points2
  sign = dif[1:length(dif)-1]*dif[2:length(dif)]
  return (which(sign<0)+1)
}

#считываем данные
df1 = read.csv("VectorsData.csv")
df2 = read.csv("dimensions.csv")
df3 = read.csv("VectorsClasses.csv")

#выгружаем в переменные прочитанные данные
points = as.matrix(df1)
number_of_classes_each_dimension = as.matrix(df2)
points_classes = as.matrix(df3)

vector_dimension = ncol(df1)
vector_counts = nrow(df1)

#создаем массив для определения границ
border_points = list()


for (i in 1:vector_dimension){
  
  #если в данном измерении один класс
  
  if (number_of_classes_each_dimension[i] == 1){
    x = points[,i]
    border_points[[i]] = c(min(unlist(density(x)[1])), max(unlist(density(x)[1])))
    plot(density(x), col = 'red')
  }
  
  #если два класса
  if (number_of_classes_each_dimension[i] == 2){
    
    #получаем точки первого и второго класса
    x1 = points[points_classes[,i]==-1,i]
    x2 = df1[points_classes[,i]==1,i]
    
    #получаем значения распределения и 
    #область определения на всем промежутке для первого и второго класса
    x1_dens = unlist(density(x1,from = min(min(x1),min(x2)),to = max(max(x1),max(x2)))[2])
    x2_dens = unlist(density(x2,from = min(min(x1),min(x2)),to = max(max(x1),max(x2)))[2])
    x = unlist(density(x1,from = min(min(x1),min(x2)),to = max(max(x1),max(x2)))[1])
    
    #рисуем графики распределений
    plot(density(x1,from = min(min(x1),min(x2)),to = max(max(x1),max(x2))),
         ylim = c(0,max(max(x1_dens),max(x2_dens))), col = 'red')
    lines(density(x2,from = min(min(x1),min(x2)),to = max(max(x1),max(x2))),col='blue')

    #находим пересечения
    separate_points = calculate_intersections(x1_dens,x2_dens)
    
    #отмечаем точки пересечений на графике
    for (j in 1:length(separate_points)){
      points(x[separate_points[j]],x2_dens[separate_points[j]])
    }
    
    #кладем границы классов в массив
    if (length(separate_points) %%2 == 0){
      border_points[[i]]=list( c(min(x),x[separate_points],max(x)),
                               c(x[separate_points]))
    }
    else{
    border_points[[i]]=list( c(min(x),x[separate_points]),
                               c(x[separate_points],max(x)))
    }
    
  }

}
print(border_points)
