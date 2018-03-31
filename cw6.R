# Lukasz Lapaj

library(MASS)
tbl = table(survey$Smoke, survey$Exer)
tbl
chisq.test(tbl)
#wniosek : p-value = 0.4828 alfa = 0.05 czyli
#nie ma podstaw do odrzucenia H0
#niepewne z powodu malych licznosci w tabeli

qchisq(1-0.05,(4-1)*(3-1))  # wartosc krytyczna
#wniosek 12.59 > 5.48 => nie ma podstaw do odrzucenia H0

(ctbl = cbind(tbl[,"Freq"], tbl[,"None"] + tbl[,"Some"]) )
chisq.test(ctbl)
#wniosek p-value = 0.3571 > alfa = 0.05 nie ma podstaw do odrzucenia hipotezy
#wartosc obliczona x-squared = 3.23, wartosc krytyczna 7.8
#wniosek: nie ma podstaw do odrzucenia hipotezy h0
qchisq(1-0.05,(4-1)*(2-1))  # wartosc krytyczna

(tabela <- cbind(c(30,170),c(80,220)))
chisq.test(tabela,correct = F)
#wniosek p-value = 0.002034 < alfa 0.01
#czyli hipoteze H0 nalezy odrzucic
#na noszenie okualarow nie ma wplyw dlugosc pracy przed komputerem
alfa=0.01
qchisq(1-alfa,(2-1)*(2-1))
#wartosc obliczona x-squared = 9.51, a wartosc krytyczna 6.63
tabela <- cbind(c(30,170),c(80,220))
t2<-chisq.test(tabela,correct = F)
names(t2$statistic) <- "V Cramera"
n<-sum(tabela)
(V <- sqrt(t2[[1]]/(n*min(dim(tabela)-1))))
#wynik 0.137

set.seed(39)
A <- sample(c('a','b','c'), size=250, replace = T, prob = c(.2,.4,.4))
B <- sample(c(1:4),size=250,replace=T, prob = c(.1,.6,.2,.1))
(mytable <- table(A,B))
chisq.test(mytable,correct = F)

qchisq(1-0.05,(4-1)*(3-1))
#wnioski p-value = 0.354 nie ma powodow do odrzucenia h0

reszty <- c(-3.9,-0.3,-3.1,-5.5,-6,-2.6,-2.3,3.4,4.7,2.6,3.1,0.8,9.7,5.4,1.6,1.4,4.2,-0.3,-7.4,-3.4,1,4.6,-2.7,-5.2)
shapiro.test(reszty)
#wniosek p-value > alfa = 0.05
#czyli nie ma podstaw do odrzucenia H0

#eksperyment symulacyjny
iteracja <- function(n,alfa){
  x<-rt(n,6)
  pwartosc <- shapiro.test(x)$p.value
  pwartosc < alfa
}

iteracja(30,0.05)
n <- 30
alfa <- 0.05
m <- 10000

set.seed(20)
sym <- replicate(m, iteracja(n,alfa))
head(sym)
mean(sym)
#wyznaczona moc testu to 0.2014

wmin <- c(1,1.4,1.8,2.2,2.6)
wmax <- c(1.4,1.8,2.2,2.6,3)
sr.p <- (wmin+wmax)/2 
licz <- c(15,45,70,50,20)
w.sr <- sum(sr.p*licz)/sum(licz)
w.sd <- sqrt(sum((sr.p-w.sr)^2*licz)/sum(licz))
ui <- (wmax - w.sr)/w.sd
n <- sum(licz)
pi <- c(pnorm(ui)[1],pnorm(ui)[2:5]-pnorm(ui)[1:4])
(chi.kw <- sum((licz-n*pi)^2/(n*pi)))
alfa <- 0.05
qchisq(1-alfa,(5-2-1))
#wniosek: wartosc krytyczna 5.99 > wartosc obliczona = 0.898 czyli nie ma podstaw
#do odrzucenia h0
#dane z rozkladu normalnego

set.seed(15)
x1 <- rt(30,2)
x2 <- rt(30,6)
x3 <- rt(30,10)
par(mfrow = c(2,2))
hist(x1)
hist(x2)
hist(x3)

n <- 30
l.w <- c(0,1,2,3,4,5)
l.s <-c(13,27,28,16,8,7)
w.sr <- sum(l.w*l.s)/sum(l.s)
ppois <- ppois(l.w, w.sr)
pi <- ppois[1]-ppois[0]
chi.kw <- sum(l.s-n*pi)^2/(n*pi)