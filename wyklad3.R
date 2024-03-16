liczby<-c(10:99)
liczby<-liczby[liczby%%3==0
mtcars[mtcars$am==1,]

wiek<-c(15,8,2,1,112,50)
ranking<-c(1601,90,1220,2050,999,3000)
kategoria<-character(6)
for(i in 1:6){
  if(wiek[i]<10){
    kategoria[i]="A"
  }
  else{
    if(ranking[i]>1600){
      kategoria[i]="B"
    }
    else{
      kategoria[i]="C"
    }
  }
}
grupy<-data.frame("wiek"=wiek,"ranking"=ranking,"kategoria"=kategoria)
grupy

zwierzeta<-character(100)

pietro<-sample(1:10,100,replace = T)
for(i in 1:100){
  if(pietro[i]<=3){
    zwierzeta[i]="pies"
  }else if(pietro[i]>=7){
    zwierzeta[i]="kot"
  }
  else{
    zwierzeta[i]="rybki"
  }
}
informacja<-data.frame("pietro"=pietro,"zwierzatko"=zwierzeta)
informacja

getwd()
