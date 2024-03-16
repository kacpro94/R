library(dplyr)
library(rvest)
library(tidyverse)
library(rvest)
library(mondate)
library(RCurl)
library(XML)
#zad1
minmaxK <- function(x, K = 5) {
  if (K >= length(x)) {
    return(list(sort(x), rev(sort(x))))
  }
  
  min_elements <- sort(x)[1:K]
  max_elements <- rev(sort(x))[1:K]
  
  return(list(min_elements, max_elements))
}

random_vector <- sample(1:4000, 100, replace = TRUE)

result <- minmaxK(random_vector)

cat("Wektor:", random_vector, "\n")
cat(length(result[[1]]), "najmniejszych elementów:", result[[1]], "\n")
cat(length(result[[2]]), "największych elementów:", result[[2]], "\n")



## zadanie 2
lDsknl <- function(x) {
  divisors <- numeric(0)
  for (i in 1:(x-1)) {
    if (x %% i == 0) {
      divisors <- c(divisors, i)
    }
  }
  return(sum(divisors) == x)
}

perfect_numbers <- numeric(0)


start_time <- Sys.time()
for (num in 2:10000) {
  if (lDsknl(num)) {
    perfect_numbers <- c(perfect_numbers, num)
  }
}
end_time <- Sys.time()

cat("Liczby doskonałe w zakresie od 1 do 10000:", perfect_numbers, "\n")
cat("Czas obliczeń:", end_time - start_time, "\n")
#zadanie 3
myNorm<-function(x){
  
  if(length(x)==0){stop("Wektor pusty")}
  min_ <-min(x)
  max_<-max(x)
  normalized_vector<-((x - min_)/max_)
  return(normalized_vector)
  
}
x<-c(10,6,9,69,69)
d<-myNorm(x)
print(d)

#zad4


myCorr <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Error: Vectors x and y must have the same length.")
  }
  
  pearson_corr <- cor(x, y, method = "pearson")
  kendall_corr <- cor(x, y, method = "kendall")
  spearman_corr <- cor(x, y, method = "spearman")
  
  return(c(pearson_corr, kendall_corr, spearman_corr))
}


set.seed(123)  
x <- runif(100, 0, 5)
e <- rnorm(100)


y <- x + e


correlation_results <- myCorr(x, y)

cat("Współczynnik korelacji Pearsona:", correlation_results[1], "\n")
cat("Współczynnik korelacji Kendalla:", correlation_results[2], "\n")
cat("Współczynnik korelacji Spearmana:", correlation_results[3], "\n")


#zad 5 

myStats<-function(x,p){
  if(p==0){
    return(c(mean(x),sd(x)))
  }
  else {
    return(c(median(x),mad(x)))
  }
}

myStats(random_vector,0)
myStats(random_vector,1)

myStats(x,0)
myStats(x,1)

myStats(e,0)
myStats(e,1)

#zad 6

library(rootSolve)

myFun <- function(x) 10*sin(1.5*x)* cos(.5*x^3) + (1/2)*sqrt(abs(x))

zero1 <- uniroot(myFun, interval = c(6, 7))
zero2 <- uniroot(myFun, interval = c(1, 2))
zero3 <- uniroot(myFun, interval = c(-5, 5))

zeros_all <- uniroot.all(myFun, interval = c(-3, 3))

x_vals <- seq(-5, 7, length.out = 1000)
y_vals <- myFun(x_vals)

plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2,
     xlab = "x", ylab = "f(x)", main = "Wykres funkcji myFun z miejscami zerowymi",
     cex.main = 1.5)

abline(h = 0, col = "red", lty = 2)  # Linia osi OX

points(zero1$root, 0, col = "green", pch = 16, cex = 1.5)
points(zero2$root, 0, col = "green", pch = 16, cex = 1.5)
points(zero3$root, 0, col = "green", pch = 16, cex = 1.5)
points(zeros_all, rep(0, length(zeros_all)), col = "purple", pch = 16, cex = 1.5)


#zad 7

myLin <- function(x){
  f1 = sum(c(2,3,-3)*x) +1
  f2 = sum(c(1,-1,-2)*x) -3
  f3 = sum(c(1,1,-2)*x) +4
  c(f1=f1,f2=f2,f3=f3)
}

myLin(c(0,0,0))
multiroot(f = myLin , c(0,0,0))


#zad8
#zad8
myNonLin <- function(x) {
  f1 <- sum(c(2, 1, -2)*x) - 2
  f2 <- sum(c(1, 2, -2)*x) - 3
  f3 <- sum(c(2, 1, -1)*x) - 3 
  c(f1=f1, f2=f2, f3=f3)
}
myNonLin(c(1, 1, 1))
multiroot(myNonLin, c(0, 0, 0))

#zad9
url<-"https://pl.wikipedia.org/wiki/Najwi%C4%99ksze_przedsi%C4%99biorstwa_%C5%9Bwiata"
x<-url
parse_result<-function(x){
  read_html(x)%>%
    html_nodes("table")%>%
    html_table()->tabele
}

zad9<-parse_result(url)
zad9 <- zad9[[1]]
str(zad9)
zad9 <- zad9 %>%
  filter(`Symbol giełdowy`!= "")

zad9 <- zad9 %>%
  mutate(`Przychód(mln $)` = str_replace_all(`Przychód(mln $)`, "\\s+", ""))%>%
  mutate(`Przychód(mln $)` = as.numeric(`Przychód(mln $)`))
data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==

zad9<- zad9%>%
  separate(Siedziba,c("Miasto","Kraj"),sep=", ")

summary(zad9$`Przychód(mln $)`)


zad9%>%
  top_n(10,`Przychód(mln $)`)

zad9<-zad9%>%
  filter(`Przychód(mln $)`<300000)

zad9$`Przychód(mln $)`<-myNorm(zad9$`Przychód(mln $)`)
