#Kacper Prorok
set.seed(11)


n <- sample(seq(5, 29, by = 2), 1)


B <- matrix(0, n, n)
diag(B) <- 1:n
diag(B[n:1, ]) <- 1:n


print(B) #1

# zad2


set.seed(41)


C <- matrix(runif(5 * 10, min = 2, max = 5), nrow = 5, ncol = 10)
D <- matrix(runif(5 * 10, min = 2, max = 5), nrow = 5, ncol = 10)

result_a <- t(C) %*% D


print(result_a) #2


print(t(crossprod(C,D))) #3

#zad3

x <- as.vector(c(C, D))

set.seed(40)


ind <- sample(length(x), 20)
x[ind] <- NA


mean_x <- mean(x, na.rm = TRUE)


sd_x <- sd(x, na.rm = TRUE)


print(mean_x) #4

print(sd_x) #5



#zad4
#a
liczba <- 10
mac4 <- matrix(outer(0:(liczba-1), 0:(liczba-1), "+") %% liczba, nrow = liczba)
print(mac4) #6
liczba2<-9
mac5<- matrix(outer(0:(liczba2-1), 0:(liczba2-1), "-") %% liczba2, nrow = liczba2)
print(mac5) #7

#zad5

n_rows <- 20
n_cols <- 10
min_ <- -20

max_ <- 20
set.seed(31)

G <- matrix(sample(min_:max_, n_rows * n_cols, replace = TRUE), nrow = n_rows)

print(G) #8


widd <- apply(G, 1, function(row) tail(sort(row), 2))
print(widd) #9

#zad6
set.seed(31)
m3 <- matrix(sample(1:10, 60, replace = TRUE), nrow = 6)
#a
sums <- which(outer(colSums(m3),colSums(m3),"+")>75,arr.ind = TRUE )

print(sums) #11
#b
pary_niepowtarzające <- which(sums > 75 & upper.tri(sums, diag = TRUE), arr.ind = TRUE)
print(pary_niepowtarzające) #12

#zad7

set.seed(27)
x<-sample(20:27, 200, T)
print(x) #13
y<-ifelse(x>=20 & x<=22, sample(c("lic","inż"), length(x[x>=20 & x<=22]), T, prob = c(0.4 , 0.6)),
          sample(c("mgr","mgr inż."), length(x[x>=20 & x<=22]), T, prob = c(0.3,0.7)))
print(y) #14
z<-sample(c("Kraków","Warszawa", "Katowice", "Rzeszów","Częstochowa"), 200, T)
print(z) #15
dane.stud<-data.frame(wiek = x, wykształcenie = y, adres = z)
print(dane.stud) #16


#zad8
#a
print(nrow(na.omit(dane.stud)))
#b
print(sum(duplicated(dane.stud), na.rm = T))



ile<-table(dane.stud$wiek)
print(ile) #19

#zad 9

subset_result <- subset(dane.stud, wiek > 20 & (adres == "Kraków" | adres == "Warszawa"), select = c(wiek, adres))

print(subset_result) #20

subset_result <- subset(dane.stud, wiek > 20 & (adres == "Kraków" | adres == "Warszawa"))

print(subset_result) #21



#zad 10
library(gridExtra)

tapply(dane.stud$wiek, list(dane.stud$wykształcenie, dane.stud$adres), mean, na.rm = T)
gridExtra::grid.table(tapply(dane.stud$wiek, list(dane.stud$wykształcenie, dane.stud$adres), mean, na.rm = T))



#zad11
set.seed(23)  
lista1 <- lapply(1:6, function(length) {
  runif(length, min = 2, max = 8)
})
print(lista1) #22
  #zad12
  lista2 <- lapply(1:6, function(length) {
    runif(length, min = 2, max = 8)
  })
    lista_sum <- Map(function(x, y) c(x, y), lista1, lista2)
  
    print(lista2) #23
    print(lista_sum) #24

