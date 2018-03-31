# Lukasz Lapaj

set.seed(10)
n<-30; x <- rnorm(n, 25, 2)
mean(x)
mu<-15 ; alfa <-0.05; mean(x)

(t <- (mean(x)-mu)/sd(x)*sqrt(n))
#Wartosc krytyczna
qt(1-alfa/2,n-1)

#wniosek poniewaz I = 29.41 > u krytycznego = 2.045
#to hipoteze= zerowa mowiaca ze srednie x = 15 nalezy odrzucic
t.test(x, mu = 15, conf.level = 0.95)
#wniosek poniewaz obliczone p value < 2.2e-16
#i jest mniejsze niz zalozony poziom istotnosci alfa = 0.95

t.test(x,mu = 23.6, conf.level = 0.95)
#poniewaz obliczone p vaue = 0.3345 to nie ma podstaw do odrzucenia h0

#zad4
sen <-c(435, 533, 393, 458, 525, 481, 324, 433, 348, 503, 383, 395, 416, 555, 500, 488)
alfa <- 0.05
m <- 7
n <- length(sen)
(t <- (mean(sen)-mu)/sd(sen)*sqrt(n))
t.test(x, mu = 7, conf.level = 0.95)

#zad2
wydatki <- read.csv(file="http://galaxy.uci.agh.edu.pl/~bbasiura/hipotezy1.txt",head=F)
mu <- 310
(u <- (mean(wydatki$V1)-mu)/sd(wydatki$V1)*sqrt(n))
qnorm(1-alfa/2)
#wniosek poniewaz obliczona wartosc u = 2.27 < u krytyczne = 1.88  to H0 mowiace ze srednie wydatki na rowne 310zl nalezy odrzucic
t.test(wydatki$V1,mu=mu,sd=sd(wydatki$V1),conf.level = 0.94)

#zad3
wydatki <- read.csv(file="http://galaxy.uci.agh.edu.pl/~bbasiura/hipotezy1.txt",head=F)
n <- dim(czas)[1]
mu <- 45
alfa <- 0.05
(u <- (mean(wydatki$V1)-mu)/sd(wydatki$V1)*sqrt(n))
qnorm(1-alfa)

X<- c(5.7, 6.5, 6.1, 5.5, 5.0, 6.1, 6.2, 5.9) 
Y<- c(4.9, 5.0, 4.7, 5.0, 5.0, 4.0)
alfa=0.1
#Wartosc testowa
(mean(X)-mean(Y))/sqrt(var(X)/length(X)+var(Y)/length(Y))
qt(1-alfa/2,length(X)+length(Y)-2)
#wniosek poniewaz wartosc obliczona 4.805 > wartosci krytycznej 1.782288 to H0 nalezy odrzucic na korzysc hipotezy mowiacej ze srednie 
#zycie paliwa jest rozne przed i po usprawnieniu
#H1 srednie zuzycie paliwa jest mniejsze po usprawieniu niz przed usprawnieniem
qt(1-alfa,length(X)+length(Y)-2)

t.test(X, Y, conf.level = 0.9)

#zad6
czas <- read.csv(file="http://galaxy.uci.agh.edu.pl/~bbasiura/Poznan+Wroclaw.txt",head=F,sep="\t")
mean(czas$V1,na.rm=T)
sd(czas$V1,na.rm=T)
mean(czas$V2)
sd(czas$V2)

n1<-length(czas$V1)-sum(is.na(czas$V1))
n2<-length(czas$V2)-sum(is.na(czas$V2))
alfa=0.02
#Wartosc testowa
(mean(czas$V1,na.rm=T)-mean(czas$V2))/sqrt(var(czas$V1,na.rm=T)/n1+var(czas$V2)/n2)

qnorm(1-alfa/2)
#wniosek poniewaz wartosc u obliczona = 5.06 > u krytyczne 2.33
#hipoteze H0 nalezy odrzucic
t.test(czas$v1, czas$v2)
is.na(czas$v1)

X<- c(5.7, 6.5, 6.1, 5.5, 5.0, 6.1, 6.2, 5.9) 
Y<- c(4.9, 5.0, 4.7, 5.0, 5.0, 4.0)
n1<-length(X);n2<-length(Y);alfa=0.05
#Wartosc testowa
(n1*var(X)/(n1-1))/(n2*var(Y)/(n2-1))
#Wartosc krytyczna
qf(1-alfa, n1-1, n2-1)
#wniosek poniewaz F obliczone = 1.35 < F krytyczne = 4.88 to nie ma
#podstaw do odrzucenia H0 o rownosci wariancji w obu probach
var.test(X,Y,alternative = "two.sided",conf.level = 1-alfa)

#wariancja w zad6
#porownanie dwoch frakcji
n1<-1000;n2<-500;m1<-350;m2<-30
w1<-m1/n1;w2<-m2/n2;p<-(m1+m2)/(n1+n2)
n<-(n1*n2)/(n1+n2)
alfa=0.0000000000001
#Wartosc testowa
(w1-w2)/sqrt(p*(1-p)/n)
#Wartosc krytyczna
qnorm(1-alfa/2)
#wniosek poniewaz u obliczone = 12.17 > u krytyczne = 1.96 to
#hipoteze zerowa odrzucamy na korzysc hipotezy alternatywnej
#mowiacej ze frakcje zwolennikow partii ALFA sa rozne na wsi i w miescie

