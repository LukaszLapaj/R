# Lukasz Lapaj

#Zad3
PS = 0.98; PP = 0.02
(P = choose(10,0)*PS^10  +  choose(10,1)*PS^9*PP)

#zad7
lambda = 3; czas = 3
pexp(czas, rate=1/lambda, lower.tail=FALSE)

#zad1
omega <- 6^2
sprzyja_A <- 22
sprzyja_B <- 18
(PA <- sprzyja_A/omega)*(PB <- sprzyja_B/omega)
(PAB <- 6/omega)

#zad4
n = 440; q=0.008;lambda = q*n
(dpois(6, lambda=q*n))

#WSTEP
#Zadanie 1
(omega <- choose(52, 2))
sprzyja_A <- choose(13, 2)
sprzyja_B <- choose(16, 2)

(p_kier <- sprzyja_A/omega)
(p_figury <- sprzyja_B/omega)

(PAB <- choose(4, 2)/omega)
p_kier*p_figury

#Zadanie 3
PK = 0.5; PM = 0.5
PNZdK = 0.72; PNZdM = 0.56
#Prawdopodobieieñstwo nie zdania egzaminu
(pcalkowite = PK*PNZdK+PM*PNZdM)

#Zadanie 4
PS = 0.98; PP = 0.02
(P = choose(9,1)*PS^8*PP^1)
#a)
#Prawdopodobieieñstwo ze dokladnie druga awaria zdarzy sie w 10 probie wynosi
#P * PP = 0.003062747
(PP * P)
#b)
(P = dbinom(2, size=10, prob=0.02))
#c)
PS = 0.98; PP = 0.02
(P = choose(10,0)*PS^10  +  choose(10,1)*PS^9*PP)

#Zadanie 5
rpois(20, 5)

n = 410; q = 0.01; lambda = q*n
dpois(6, lambda =  q*n)

#Zadanie 6
lambda <- 3;
(dpois(3, lambda-3))

#Zadanie 7
min = 2; max = 14; czas = 6
(p <- punif(czas,min = 2,max = 14,lower.tail = T))

#Zadanie 8
sr = 96; sd = 6;kwota = 84
# prawdopodobieñstwo ze x < kwota
pnorm(kwota,mean = 96,sd = 6)
kwota = 108; pnorm(kwota,mean = 96,sd = 6,lower.tail = TRUE)
1-pnorm(84, mean = 96, sd = 6)

#Zadanie 9
lambda = 3