library(httr)
library(dplyr)
library(ggplot2)
library(corrplot)
url<-"https://pl.wikipedia.org/wiki/Województwo"
resp<-GET(url)

library(rvest)
dane<-read_html(url)

tab1 <- dane %>%
  html_nodes(xpath = "//table[contains(@class,'wikitable')]")
tab1


tabela <- tab1[[1]] %>% html_table(fill=T)
str(tabela)
head(tabela)

#Przekształcenie danych

tabela<-tabela%>%
  rename("Powierzchnia w km2"=`Powierzchnia[km²], 2021-12-31[3]`,"Ludność"=`Ludność2021-12-31[4]`,"Poziom urbanizacji"=`Poziomurbanizacji(31 XII 2018)`,"Stopa bezrobocia"=`Stopa bezrobocia(I 2019)[5]`,"PKB na jednego mieszkańca"=`PKB na 1 mieszkańca(31 XII 2018) [zł]`,"Rejstracja samochodowa"=`Wyróżnik na tablicachrejestracyjnych`)

tabela<-tabela%>%
  mutate(`Powierzchnia w km2`=as.numeric(gsub(" ","",`Powierzchnia w km2`)))

tabela<-tabela%>%
  mutate(Ludność=as.numeric(gsub(" ","",Ludność)))

tabela<-tabela%>%
  mutate(Województwo=as.factor(Województwo))


tabela<-tabela%>%
  arrange(`Powierzchnia w km2`)
#najwieksze powierzchniowo wojewodztwa w pl
ggplot(tabela)+
  geom_col(mapping=aes(x =`Powierzchnia w km2`, y=as.factor(Województwo)),
           size=3)+
  labs(x="powierzchnia w km2",
       y="wojewodztwo")


#wykres czy powierzchnia wojewodztwa ma wplyw na liczbe ludnosci

tabela<-tabela%>%
  arrange(Ludność)

ggplot()+
  geom_point(aes(x=tabela$Ludność,y=tabela$`Powierzchnia w km2`,
                 color=tabela$Województwo))+
             labs(x="Ludnosc",
                  y="powierzchnia[km2]",
                  color="Wojewodztwo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  

tabela<-tabela%>%
  mutate(`Gęstość zaludnienia[osób/km²]`=as.numeric(gsub(",",".",`Gęstość zaludnienia[osób/km²]`)))



cor=cor(tabela[,c("Powierzchnia w km2","Gęstość zaludnienia[osób/km²]","Ludność")],
        method="pearson")

#wykres zaleznosci
corrplot(cor)


#statystyki opisowe dla powierzchni w km2

statystyki<-tabela%>%
  summarize("średnia"=mean(`Powierzchnia w km2`),
            "IQR"=IQR(`Powierzchnia w km2`),
            "sd"=sd(`Powierzchnia w km2`),
            "mediana"=median(`Powierzchnia w km2`),
            "Q1"=quantile(`Powierzchnia w km2`,0.25),
            "Q3"=quantile(`Powierzchnia w km2`,0.75))
statystyki

tabela%>%
  ggplot(aes(x=1,y=`Powierzchnia w km2`))+
  geom_boxplot()+
  labs(title = "rozklad powierzchni w km2")



  



  