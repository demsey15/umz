data <- read.delim('D:\\Uczelnia\\Informatyka - magisterskie\\Uczenie maszynowe - zastosowania\\umz2016-mieszkania-chyze_szynszyle\\dev-0\\in.tsv',
                   header=FALSE)
ceny <- read.delim('D:\\Uczelnia\\Informatyka - magisterskie\\Uczenie maszynowe - zastosowania\\umz2016-mieszkania-chyze_szynszyle\\dev-0\\expected.tsv',
                 header=FALSE)

pokoje = data[1]
metry = data[2]
pietro = data[3]
lokalizacja = data[4]
opis = data[5]
cena = ceny[1]

pokojeMean = mean(data$V1)
metryMean = mean(data$V2)
pietroMean = mean(data$V3)

pokojeSt = sd(data$V1)
metrySt = sd(data$V2)
pietroSt = sd(data$V3)

cechyTrenujacy = data.frame(rooms = pokoje, sqrMeters = metry, floor = pietro, desc = opis, cena = cena)

usun = c()
for (i in range(1, nrow(cechyTrenujacy))){
  
  if (cechyTrenujacy$V1[i] >= pokojeMean + pokojeSt || cechyTrenujacy$V1[i] <= pokojeMean - pokojeSt){
    usun = c(usun, i)  
  }else if (cechyTrenujacy$V2[i] >= metryMean + metrySt || cechyTrenujacy$V2[i] <= metryMean - metrySt){
    usun = c(usun, i)
  } else if (cechyTrenujacy$V3[i] >= pietroMean + pietroSt || cechyTrenujacy$V3[i] <= pietroMean - pietroSt){
    usun = c(usun, i)
   }
 }

cechyTrenujacy = cechyTrenujacy[-usun]
model = lm(cechyTrenujacy$V1.1~cechyTrenujacy$V1+cechyTrenujacy$V2+cechyTrenujacy$V3)

library(stats)
model2 =  glm(cechyTrenujacy$V1.1~cechyTrenujacy$V1+cechyTrenujacy$V2+cechyTrenujacy$V3)

pred = function(model, x1, x2, x3){
  return(coef(model)[1] + coef(model)[2]*x1 + coef(model)[3]*x2 + coef(model)[4]*x3) 
}

dataTest <- read.delim('D:\\Uczelnia\\Informatyka - magisterskie\\Uczenie maszynowe - zastosowania\\umz2016-mieszkania-chyze_szynszyle\\test-A\\in.tsv',
                   header=FALSE)

wyniki = c()
for (i in seq(1, nrow(dataTest))){
  wyniki = c(wyniki, pred(model, dataTest$V1[i], dataTest$V2[i], dataTest$V3)[i])
}


write.table(wyniki, file = "D:\\Uczelnia\\Informatyka - magisterskie\\Uczenie maszynowe - zastosowania\\umz2016-mieszkania-chyze_szynszyle\\test-A\\outDDodst.tsv", 
            row.names=FALSE, sep="\t")


wyniki2 = c()
for (i in seq(1, nrow(dataTest))){
  wyniki2 = c(wyniki2, pred(model2, dataTest$V1[i], dataTest$V2[i], dataTest$V3)[i])
}


write.table(wyniki2, file = "D:\\Uczelnia\\Informatyka - magisterskie\\Uczenie maszynowe - zastosowania\\umz2016-mieszkania-chyze_szynszyle\\test-A\\outDD2.tsv", 
            row.names=FALSE, sep="\t")
