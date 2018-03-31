# Lukasz Lapaj

dane.sr <- seq(5000, 75000, 10000)
dane.u <- c(.1,.04,.54,.2,.02,.04,.02,.04)
length(dane.u)

#srednia:
(dane.mean <- sum(dane.sr*dane.u))

#Odchylenie standardowe
(dane.sd <- sqrt(sum(((dane.sr-dane.mean)^2)*dane.u)))

licznosc <- 50
(dane.licznosc <- dane.u*licznosc)
cumsum(dane.licznosc)

dane.l<-dane.sr - 5000
dane.p<-dane.sr + 5000
(dane.median <- dane.l[3] + (10000/dane.licznosc[3])*(licznosc/2-cumsum(dane.licznosc)[2]))