# Lukasz Lapaj

plik <- read.csv(file="http://artemis.wszib.edu.pl/~basiura/szeregi_d.csv", sep=";",dec=".", head=T)
attach(plik)
plot(szereg5, type="l", col="wheat")
dane <- szereg5
detach(plik)
print(summary(dane))

#Wyniki
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 22.50   27.45   31.25   31.76   34.94   49.52
#Wnioski
# Wartosc przecietna to 29.96
# Mediana wynosi 31.25 wiec polowa wynikow jest mniejsza od 31.25

#Modalna
print(n<-length(dane))
h<-hist(dane, ceiling(sqrt(n)))
print(h$counts)
print(max(h$counts))
print(k<-which.max(h$counts))
print(h$breaks[k])
dlugosc <- h$breaks[k]-h$breaks[k-1]
modalna <- h$breaks[k]+(h$counts-h$counts[k-1])/((h$counts[k]-h$counts[k-1])+(h$counts[k]+h$counts[k+1])) * dlugosc
print(modalna)

#Rozstep
print(min(dane))
print(max(dane))
print(max(dane) - min(dane))
# Rozstep wynosi 27.0244, wartosc minimalna to 27.0244 a wartosc maksymalna to 49.524
print(var(dane))
# Wariancja wynosi 29.7992
print(sd(dane))
# Odchylenie standardowe wynosi 14,83 a wartosc przecietna 31.76
# Najbardziej prawdopobne sa wyniki od 31.76 - 14,83 do 31.76 + 14,83

#Blad standarodwy sredniej
print(sd(dane))/sqrt(length(dane))
#Jezeli przyjmiemy
# Popelniany blad ma wartosc +-0.61
# Szacowana wartosc przecietna wybosi 31.76 +- 0.61

par(mfloc-c(2,1))
hist(dane, breaks = ceiling(sqrt(n)), col="wheat")
rug(dane, col="red4")
curve(25000*dnorm(x, mean=29, sd=15), add=T, col="red")
boxplot(dane, col="red", horizontal = TRUE)

# odchylenia od sredniej
dane.dev<-dane-mean(dane)
#trzecia potega odchyleñ i srednia
dane.dev.3 <- dane.dev^3
#to jest trzeci moment centralny
(dane.dev.3.mean<-mean(dane.dev.3))

dane.dev3.mean/sd(dane)^3


library(e1071)
#skosnosc 
skewness(dane)
# kurtoza
kurtosis(dane)

#wykres ramka z wasami:
boxplot(dane, main="Dane dla roku 2009 (mediana ws srednia)",plot = T)
sd.dane <- sd(dane)
m.dane <- mean(dane)
points(1,m.dane,col = "orange", pch = 18)   #wartosc srednia jest pomarañczowym punktem
arrows(1,m.dane-sd.dane,1,m.dane+sd.dane,code=3,col = "red", angle = 75, length = .1)