#zadanie 1
lista2 <- list(dane1 = c(NA, 1:10),
               dane2 = c(-5:5, NA))

lapply(lista2,mean,na.rm=T)

lapply(lista2,sd,na.rm=T)

lapply(lista2,quantile,na.rm=T)

?sapply()
#zad 2
lista4 <- list(dane1 = 20:1,
               dane2 = 1:10,
               dane3 = 1:5)

(sapply(lista4, max))
(sapply(lista4, min))
#Roznica jest to ze sapply zwraca wektor, a lapply liste
jj1<-lapply(lista4,max)
lapply(lista4,min)

funkcja_zmiennosc = function(szym){
  return(sd(szym,na.rm=T)/mean(szym,na.rm=T))
}
sapply(lista4,funkcja_zmiennosc)

#zad 3
?apply()
#bierze macierz zamiast listy czy wektora 
dupa<-apply(mtcars,2,mean)

#zad4
library(dplyr)
tapply(mtcars$mpg,mtcars$cyl, mean)
summary(tapply(mtcars$mpg,mtcars$cyl, mean))

tapply(mtcars$mpg,mtcars$cyl,summary)

#zad5
library(MASS)
?anscombe
mean(anscombe[2,])
dane<-fgl


#a
apply(dane[1:9],2,mean)
#b
str(fgl)
#c
apply(dane[1:9],2,median)
#sprawdz jaka roznica najwuieksza

#d
lapply(dane[1:9], sd)
lapply(dane[1:9],funkcja_zmiennosc)


#zad 6 

library(gapminder)
library(dplyr)
library(tidyr)
options(digits=4)
dane1<-gapminder%>%
  rename(rok=year, PKB=gdpPercap)
dane1<-dane1%>%
  mutate(pop=pop/1000000)

#c
dane1%>%
  filter(continent=="Africa",rok==1957)

rok_1957<-dane1%>%
  filter(continent=="Africa",PKB>12000)%>%
  count(rok)
library(ggplot2)

ggplot(rok_1957)+
  geom_point(mapping = aes(x = rok, y = n,
                           col="blue")+
               scale_x_discrete(breaks = seq(1957, 2022, by = 5)) +
               labs(x = "Rok", y = "Liczba"))

#d
dane1%>%
  filter(continent!="Africa",rok==1962,PKB<750)
  

#e
lata<-c("1977","2002","1952")
Polska<-dane1%>%
  filter(rok %in% lata,country=="Poland")%>%
  select(rok,country,lifeExp)


Ameryki_1952<-dane1%>%
  filter(rok==1952,continent=="Americas",lifeExp>Polska$lifeExp[1])

Ameryki_1977<-dane1%>%
  filter(rok==1977,continent=="Americas",lifeExp>Polska$lifeExp[2])

Ameryki_2002<-dane1%>%
  filter(rok==2002,continent=="Americas",lifeExp>Polska$lifeExp[3])

#f
populacja2007<-dane1%>%
  filter(rok==2007)%>%
  select(pop,continent)%>%
  group_by(continent)%>%
  summarise("mean"=mean(pop),
            "sd"=sd(pop),
            "min"=min(pop),
            "max"=max(pop),
            "me"=median(pop))
  

populacja2007

#g
ZYCIE<-dane1%>%
  filter(continent!="Americas",rok==1977,PKB<5000)%>%
  group_by(continent)%>%
  top_n(3,lifeExp)
#DOKONCZYC

#zad 7
library(ggplot2)
kraje<-c("Italy","Germany","Japan")
trzech1<-dane1%>%
  filter(country%in%kraje)%>%
  select(country,rok,lifeExp)%>%
  spread(key = rok,value = lifeExp)

trzech<-dane1%>%
  filter(country%in%kraje)%>%
  select(country,rok,lifeExp)
  

trzech%>%
  ggplot(aes(x=rok,y=lifeExp,col=country))+
  geom_line(linewidth=1)+
  geom_point(size=2)


#zad 8

?set.seed()

proba1<-sample(1:6,2,replace = T)
proba2<-sample(1:6,10,replace = T)
proba3<-sample(1:6,50,replace = T)
proba4<-sample(1:6,100,replace = T)
proba5<-sample(1:6,1000,replace = T)



mean(proba1)
mean(proba2)
mean(proba3)
mean(proba4)
mean(proba5)
wygraneNY<-0
wygraneCB<-0

for(i in 1:10000){
  losowanie<-sample(c("Bulls","Knicks"),7,prob = c(0.35,0.65),replace = T)
  if(sum(losowanie=="Knicks")>=4){
    wygraneNY<-wygraneNY+1
  }else{
    wygraneCB<-wygraneCB+1
  }
}
wygraneNY
wygraneCB


#zad 9
library(MASS)
library(robustbase)
mammals
Animals2
mammals1<-data.frame(rownames(mammals))
colnames(mammals1)[1]<-"zwierzeta"
animals2<-data.frame(rownames(Animals2))
colnames(animals2)[1]<-"zwierzeta"
zwierzaki<-inner_join(mammals1,animals2,by="zwierzeta")

zwierzaki<-intersect(mammals1,animals2)
count(zwierzaki)

cat("zwierzeta ktore sa w zestawie mammals, ale nie w animals2:")
setdiff(mammals1,animals2)

cat("zwierzeta ktore sa w zestawie animals2, ale nie w mammals:")
setdiff(animals2,mammals1)

#zad 10