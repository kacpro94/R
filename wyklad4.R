library(dplyr)
library(gapminder)
gapminder

Afrika1997<-gapminder%>%
  filter(gdpPercap>5000,year==1997,continent=="Africa")

bogactwo_Rok<-gapminder%>%
  group_by(year)%>%
  summarise(PKB=mean(gdpPercap))%>%
  arrange(desc(PKB))
bogactwo_Rok[1:8,]

mtcars
auta<-mtcars%>%
  filter(cyl==4,gear==5,am==1)%>%
  arrange(desc(mpg))

srednie<-tapply(mtcars$qsec,mtcars$cyl,mean)
srednie

kmh<-mtcars%>%
  mutate(spalanie_kmh=235.2/mpg)
kmh[1:10,]

data1<-inner_join(`NASDAQ.(4)`,`WIG20.(1)`)
data2<-full_join(`NASDAQ.(4)`,`WIG20.(1)`)
data2
