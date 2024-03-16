#Zad 1
set.seed(111)
xu<-runif(500,10,15)
xn<- rnorm(500,5,2)
xn
xu

#Zad2
data_g1 <-c("T","N","X")
g1<-sample(data_g1,500,T)
g2<-sample(1:5,500,T)
g2

#a
help(tapply)
tapply(xn,g1,mean)
tapply(xn,g1,sd)
tapply(xn,g1,var)

#b
tapply(xn,g2,mean)
tapply(xn,g2,sd)
tapply(xn,g2,var)

#c
tapply(xn,g1,range)
tapply(xn,g2,range)

#Zad3
help(list)
help(rt)
lista_3<-list(v1=rt(100,7),v2=rt(50,7),v3=rt(25,7),v4=rt(5,7),v5=rt(5,7))

lapply(lista_3,mean)
lapply(lista_3,sd)

#Zad4
probka<- c(-20:20)
set.seed(111)
A<-matrix(sample(probka,200,T),20,10)


#Zad5
set.seed(111)

generate_random_matrix <- function() {
  
  dim_matrix <- sample(2:10, 1)
  
  
  mean_val <- runif(1, min = 8, max = 12)
  
  
  sd_val <- runif(1, min = 1, max = 4)
  
  
  matrix_data <- matrix(rnorm(dim_matrix^2, mean = mean_val, sd = sd_val), nrow = dim_matrix)
  
  return(matrix_data)
}


matrix_list <- lapply(1:10, function(x) generate_random_matrix())

print(matrix_list)

matrix_dimensions <- sapply(matrix_list, function(mat) dim(mat))
print("(a) Wymiary wszystkich macierzy:")
print(matrix_dimensions)


det_matrix1 <- det(matrix_list[[1]])
det_matrix3 <- det(matrix_list[[3]])

cat("(b) Wyznacznik pierwszej macierzy: ", det_matrix1, "\n")
cat("(b) Wyznacznik trzeciej macierzy: ", det_matrix3, "\n")


det_all_matrices <- sapply(matrix_list, det)
print("(c) Wyznaczniki wszystkich macierzy:")
print(det_all_matrices)
