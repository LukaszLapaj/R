# Lukasz Lapaj

library(MASS)
str(survey)
span <- survey$Wr.Hnd
#Estymacja punktowa wartosci przecietnej
mean(span,na.rm = T)
median(span, na.rm = T)
sigma <- 2.18
n <- length(span)
m <- mean(span, na.rm = T)
alfa <- 0.05
e <- qnorm(1 - alfa/2)
E <- e*sigma/sqrt(n)
m+c(-E,+E)
#Zad1 a)
n <- 8
czas <-c(25, 16, 12, 10, 12, 21, 25, 23)
n < - length(span[1:25])
m <- mean(span[1:25])
s <- sd(span[1:25], na.rm = TRUE)
e<- qt(1-0.05/2, df=n-1)
E<-e*s/sqrt(n)
m+c(-E,+E)

b<-length(span)
m<-mean(span,na.rm=TRUE)
alfa<-0.05
e<-qnorm(1-alfa/2)
E<-e*s/sqrt(n)
m+c(-E,+E)

x <- span[survey$Sex=="Male"]
plec <- survey$Sex
k <- sum(survey$Sex=="Female", na.rm = T)
n <- length(plec)
(f <- k/n)
SE <-sqrt(f*(1-f)/n)
E <- qnorm(1.0-0.05/2)*SE
f+c(-E,+E)

sigma<-3.18
n <- lenght(span)
m <- mean(span, na.rm = T)
d <- 0.28
e < qnorm(1-0.05/2)
(licz <- e^2*sigma^2/d^2)

n <- lenght(span)
m <- mean(span, na.rm = T)
s < sd(span, na.rm = T)
d <- 0.18
e <- qt(1-0.05/2, df=n-1)
(licz <- e^2*s^2/d^2)

srodki.przedzialow <- c(10, 20, 30, 40, 50)
licznosci <- c(2,16,25,50,27)
n <- sum(licznosci)
srednia <- sum(srodki.przedzialow*licznosci)/n
odchylenie.stand <- sqrt(sum(((srodki.przedzialow-srednia)^2)*licznosci)/(n-1))

k<-sum(survey$Sex=="Female", na.rm = T)
plec <- survey$Sex
n <-length(plec)
f <- k/n
d <- 0.06
e<-qnorm(1-0.05/2)
(licz <- e^2*f*(1-f)/d^2)

n < 2200
k <- 1386
f <- k/n
d <- 0.01
e <- qnorm(0.95/2)
licz <- e^2*f*(1-f)/d^2

d <- 0.08
e <- qnorm(1-0.05/2)
(licz <- e^2/(4*d^2))

d <- 0.02
e <- qnorm(0.99/2)
(licz <- e^2/(4*d^2))