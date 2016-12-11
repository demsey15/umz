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


cechyTrenujacy = data.frame(rooms = pokoje, sqrMeters = metry, floor = pietro, desc = opis, cena = cena)


model = lm(cechyTrenujacy$V1.1~cechyTrenujacy$V1+cechyTrenujacy$V2+cechyTrenujacy$V3)

pred = function(model, x1, x2, x3){
  return(coef(model)[1] + coef(model)[2]*x1 + coef(model)[3]*x2 + coef(model)[4]*x3) 
}

dataTest <- read.delim('D:\\Uczelnia\\Informatyka - magisterskie\\Uczenie maszynowe - zastosowania\\umz2016-mieszkania-chyze_szynszyle\\test-A\\in.tsv',
                   header=FALSE)

wyniki = c()
for (i in seq(1, length(dataTest))){
  wyniki = c(wyniki, pred(model, dataTest$V1, dataTest$V2, dataTest$V3))
}


write.table(wyniki, file = "D:\\Uczelnia\\Informatyka - magisterskie\\Uczenie maszynowe - zastosowania\\umz2016-mieszkania-chyze_szynszyle\\test-A\\outDD.tsv", 
            row.names=FALSE, sep="\t")
