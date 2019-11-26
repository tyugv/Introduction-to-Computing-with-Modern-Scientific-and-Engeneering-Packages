setwd("C:/Users/Fujutsu/Documents/R/packeti/1") #они должны лежать в одной директории
df1 = read.csv("VectorsData.csv")
df2 = read.csv("dimensions.csv")
points = as.matrix(df1)
number_of_classes_each_dimension = as.matrix(df2)

# размерность векторов в input
vector_dimension = ncol(df1)
# количество векторов в input
vector_counts = nrow(df1)
# массив, где будут храниться точки разделения классов 
# для каждой размерности 
# максимум точек разделения может быть 2
separation_points = matrix(0, vector_dimension, 2)

for (i in 1:vector_dimension) {
  # вектор, составленный из i-х координат 
  i_dimension = df1[,i]
  
  # если количество классов по этому измерению == 1
  if (number_of_classes_each_dimension[i] == 1) {
    # первая точка разделения - минимальная координата по этому измерению
    separation_points[i, 1] = min(i_dimension)
    # вторая точка разделения - максимальная
    separation_points[i, 2] = max(i_dimension)
  }
  
  # если количество классов по этому измерению == 2
  if (number_of_classes_each_dimension[i] == 2) {
    # рандомно выбираем 2 индекса
    indices = sample(1:vector_counts, 2)
    point_1 = i_dimension[indices[1]]
    point_2 = i_dimension[indices[2]]
    
    # массив меток принадлежности к классам
    labels = c(1:vector_counts)
    
    
    for (k in 1:10) {
      # считаем расстояния и ставим метки классов
      for (j in 1:vector_counts) {
        
        distance_1 = abs(i_dimension[j] - point_1)
        distance_2 = abs(i_dimension[j] - point_2)
        
        min_distance = c(distance_1, distance_2)
        labels[j] = which.min(min_distance)
      }
      
      # пересчитываем центры классов как среднее в каждом
      index_1 = which(labels %in% c(1))
      point_1 = sum(i_dimension[index_1]) / length(index_1)
      
      index_2 = which(labels %in% c(2))
      point_2 = sum(i_dimension[index_2]) / length(index_2)
      
    }
    
    # находим точку разделения классов как максимум в классе с меньшими значениями
    index_1 = which(labels %in% c(1))
    max_1 = max(i_dimension[index_1])
    
    index_2 = which(labels %in% c(2))
    max_2 = max(i_dimension[index_2])
    
    max_index = c(max_1, max_2)
    
    separation_points[i, 1] = min(max_index)
    separation_points[i, 2] = max(max_index)
    
    separation_points
  }
  
  if (number_of_classes_each_dimension[i] == 3) {
    
    # рандомно выбираем 3 индекса
    indices = sample(1:vector_counts, 3)
    point_1 = i_dimension[indices[1]]
    point_2 = i_dimension[indices[2]]
    point_3 = i_dimension[indices[3]]
    
    # массив меток принадлежности к классам
    labels = c(1:vector_counts)
    
    for (k in 1:10) {
      # считаем расстояния и ставим метки классов
      for (j in 1:vector_counts) {
        
        distance_1 = abs(i_dimension[j] - point_1)
        distance_2 = abs(i_dimension[j] - point_2)
        distance_3 = abs(i_dimension[j] - point_3)
        
        min_distance = c(distance_1, distance_2, distance_3)
        labels[j] = which.min(min_distance)
      }
      # пересчитываем центры классов как среднее в каждом
      index_1 = which(labels %in% c(1))
      point_1 = sum(i_dimension[index_1]) / length(index_1)
      
      index_2 = which(labels %in% c(2))
      point_2 = sum(i_dimension[index_2]) / length(index_2)
      
      index_3 = which(labels %in% c(3))
      point_3 = sum(i_dimension[index_3]) / length(index_3)
      
    }
    
    # находим точку разделения классов как максимум в классе с меньшими значениями
    index_1 = which(labels %in% c(1))
    max_1 = max(i_dimension[index_1])
    
    index_2 = which(labels %in% c(2))
    max_2 = max(i_dimension[index_2])
    
    index_3 = which(labels %in% c(3))
    max_3 = max(i_dimension[index_3])
    
    max_index = c(max_1, max_2, max_3)
    
    first_min = which.min(max_index)
    separation_points[i, 1] = first_min
    
    max_index = max_index(-first_min)
    
    second_min = which.min(max_index)
    separation_points[i, 2] = second_min
    
  }
  
}

separation_points
plot(points,col=ifelse(labels==labels[1],"blue","green"))
