# Lukasz Lapaj

path <- getwd()
dane1 <-read.table(file=paste0(path,"/zadanie1.txt"),
                   head=T,sep="\t")
str(dane1)
summary(dane1)
plot(Koszty~Produkcja, data=dane1, col=rgb(0,0,1,.6),pch=19)

#zad1
attach(dane1)
cor(Koszty, Produkcja, method="pearson")^2

#odp wsplczynnik korelacji pearsoana rowny = 0.97 swiadczy o duzej zaleznosci liniowej

#zadanie dodatkowe
x<-runif(50, -10, 10)
plot(x)
y<-rnorm(50, 0, 10)
plot(y)
cor(x,y)
y<-2*x +rnorm(50)
cor(x,y)
#zad1 p2
model <- lm(Koszty~Produkcja, data=dane1)
model
#odp zbudowany model
#Koszty = 17006.6 + 200.1(+-8.737) * produkcja +

summary(model)
#koszty = 17006.6(+-5177) + 200.1(+-8.737) * Produkcja 

#odp na pyt 2 zatem zwiekszenie produkcji o 1
#spowoduje zwiekszenie kosztow o okolo 200

#zalozenie alfa=0.05
#Poniewaz wartosci p-value dla obu parametrow sa mniejsze
#od alfa to parametry (wyraz wolny i wspolczynnik przy p
#sa istotnie rozne

#pyt3
#Na podstawie wspolczynnika determinacji R2 = 0.94 mozna powiedziec
#ze zmiennosc Kosztow jest wyjasniona w 94% przez zbudowany model.

#pyt4
#Na podstawie wspolczynnika zbieznosci fi = 1 - R2 = 1 - 0.94 = 0.06 mozna powiedziec, ze zmiennosc Kosztow jest nie wyjasniona w 6% przez zbudowany model. 

#pyt5
#Na podstawie Residual standard error = 10820
#okresla sie ze empiryczne wartosci Kosztow 
#roznia sie od odpowiadajacych im wartosci teoretycznych srednio o 10820.

#testowanie hipotezy o normalnosci reszt
model <- lm(Koszty~Produkcja, data=dane1)
shapiro.test(model$residuals)
#W = 0.97608, p-value = 0.6463
#wniosek poniewaz p-value = 0.6463 > alfa = 0.05
#to nie ma podstaw do odrzucenia hipotezy o normalnosci reszt

#Zadanie 3
read.table(file=paste0(path,"/zadanie3.txt"),
           head=T,sep="\t")
summary(dane3)
plot(Cena~Wiek, data=dane3, col=rgb(0,0,1,.6),pch=19)
plot(Cena~Przebieg, data=dane3, col=rgb(0,1,0,.6),pch=19)

#pyt1
#wspolczynniki korelacji
cor(dane3, method = "pearson")
#odp silne zaleznosci ujemne cena i wiek -0.90
#Cena i przebieg -0.91

#pyt3
model <- lm(Cena~Wiek+Przebieg, data=dane3)
model

#otrzymany model:
#Cena = 48.66 + (-1.7)*wiek + (-0.13)*Przebieg + reszty
summary(model)

#pyt2
#wspolczynnik korelacji wielorakiej
#Multiple R-squared: 0.8929
#swiadczy o silnej zaleznosci liniowej
#??? Jak wyznaczyc korelacje czastkowa

#poprawiamy o bledy
#Cena = 48.66(+-2.3) + (1.7 (+-0.75))*wiek + (-0.13 (+-0.04))*Przebieg + reszty 

#wn: poniewaz p-value dla wsp. wiek i wsp przebieg sa
#mniejsze od 0.05 to hipoteze o zerowaniu wspolczynnika
#nalezy odrzucic (wsp istotne)

#ocena dopasowania modelu

#pyt4 fi = 1 - 0.89 czyli okolo 11%
#pyt 5 R-kwadrat = 0.89%

#empiryczny blad modelu:
#Residual standard error: 3.526
#okresla sie ze empiryczne wartosci cen roznia
#sie od odpowiadajacych im wartosci teoreorytycznych
#srednio o 3.526 tys zl

#Testowanie istotnosci modelu
#Statystyka F (p-value = 1.511e-06) czyli jest mniejsze niz
#zalozony poziom istotnosci alfa
#czyli hipoteze o braku zaleznosci liniowej pomiedzy
#cena, a wiekiem i przebiegiem nalezy odrzucic
#model istotny

#test normalnosci reszt
shapiro.test(model$residuals)
#W = 0.97608, p-value = 0.6463