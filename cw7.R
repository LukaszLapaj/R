# Lukasz Lapaj

#zad1
alfa=0.04
zad1 <- read.table(file=paste0(file="zad1.txt"), header = T,sep=",",dec=".")
str(zad1)

zad1$Stanowisko  <- as.factor(zad1$Stanowisko)
#wykres 
boxplot(zad1$Czas~zad1$Stanowisko,col="red",border="darkblue",pch=19,main="srednie czasy")
#analiza wariancji
av <- aov(Czas ~ Stanowisko,data=zad1)
summary(av)
#wartosc obliczona F = 21.55
#Wniosek: 
#Wartosc p-value 5.77e-09

#wartosc krytyczna
alfa=0.04
k=3
n=dim(zad1)[1]
qf(1-alfa, k-1, n-k)
#wartosc krytyczna = 3.288
#Wniosek: Poniewaz F obliczone = 21.55 > F krytyczne = 3.29 to hipoteze zerowa mowiaca ze wszystkie srednie sa rowne nalezy odrzucic
#na korzysc hipotezy alternatywnej mowiacej ze przynajmniej jedna srednia rozni sie od pozostalych

#Wniosek 2
#Poniewaz p-value = 5.77*10^(-9) < alfa = 0.04 to h0 nalezy odrzucic

#Zadanie 2
zad2 <- read.table(file="zad2.csv", sep=";", dec='.', header = T)
View(zad2)
wydatki <- c(zad2[2,], zad2[3,], zad2[4])
miasto <- c(rep(c(1,2,3),3, each=54))
wydatki
#wykonanie analizy wariancji
aw2 <- aov(miasto~wydatki)
summary(aw2)
#Zalozenia zadania:
alfa=0.02
k=3
n=162 #lenght(miasto) lub lenght(wydatki)
#Wniosek

#Zadanie 4
zad2 <- read.csv(file=paste0(path,"/zad2.csv"), header = T,sep=";",dec=",")
r <- c(t(as.matrix(zad2[,2:4])))
f1<-c("MiastoD","MiastoS","MiastoM")
f2<-c("Wniz3","Mniz3")
k1<-length(f1)
k2<-length(f2)
n<-sum(zad2[,1]=="Wniz3")
str(zad2)
tm1 = gl(k1, 1, n*k1*k2, factor(f1))
tm2 = as.factor(c(rep('Wniz', 27*3), rep('Mniz', 27*3)))
av = aov(r ~ tm1 * tm2)
summary(av)
#
#Df Sum Sq Mean Sq F value   Pr(>F)    
#tm1           2 171518   85759  21.570 5.36e-09 ***
#tm2           1  15200   15200   3.823  0.05234 .  
#tm1:tm2       2  51854   25927   6.521  0.00191 ** 
#Residuals   156 620225    3976                    

#Wniosek:
#Na podstawie poziomow istotnosci p-value dla czynnika A(miasto) = 5.36*10^-9 < alfa 0.02
#czynnik A wplywa istotnie na poziom pomiaru wydatkow (h02 nalezy odrzucic)
#na podstawie poziomow istotnosci p-value dla czynnika B(wielkosc rodziny) 0.05 > alfa
#czynnik B nie pokazuje wplywu na pomiary wydatkow (h03 nie ma podstaw do odrzucenia)
#Odychlenie wartosci przecietnej w poszczegolnych podgrupach od ogolnej wartosci przecietnej
#nie bedzie rowne sumie efektow czynnika A i czynnika B (h4 nalezy odrzucic)
#poniewaz obliczne p-value = 0.00191 < alfa = 0.02.

#Ktora srednia sie wyroznia

#test tukeya
(tav<-TukeyHSD(av))

#diff      lwr       upr     p adj
#MiastoS-MiastoD 54.04611 25.33169  82.76053 0.0000474
#MiastoM-MiastoD 77.75426 49.03984 106.46868 0.0000000
#MiastoM-MiastoS 23.70815 -5.00627  52.42257 0.1273173

#zdecydowanie rozni sie od poziomu pozostalych wydatkow w miastach duzych

#diff        lwr      upr     p adj
#Wniz-Mniz 19.37272 -0.1983856 38.94382 0.0523389
#tutaj nie ma roznic

#wspolne roznice dla:
#MiastoS:Mniz-MiastoD:Mniz  87.823704  38.30586 137.34154879 0.0000132
#MiastoM:Mniz-MiastoD:Mniz 118.823704  69.30586 168.34154879 0.0000000
#MiastoD:Wniz-MiastoD:Mniz  69.270741  19.75290 118.78858582 0.0011760
#MiastoS:Wniz-MiastoD:Mniz  89.539259  40.02141 139.05710434 0.0000084
#MiastoM:Wniz-MiastoD:Mniz 105.955556  56.43771 155.47340064 0.0000001
#w pozostalych rozice nieistotne statystycznie
