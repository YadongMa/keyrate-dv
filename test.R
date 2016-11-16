#Input:  spot rate 
#        maturity
#        CFs

CF <- c(rep(5,9),105)
ytm <- seq(0.02,by=0.005,length.out = 10)
maturity <- 1:10
discount <- 1/(1+ytm)^maturity
pv <- discount*CF
price <- sum(pv)


bondprice(ytm = ytm,maturity = maturity,CF = CF)

duration(ytm = ytm,maturity = maturity,CF = CF)

duration(ytm = ytm,maturity = maturity,CF = CF,DV = TRUE)

keyratechange(key=2,ytm = ytm,maturity = maturity,changeby = 0.0001)-ytm
keyratechange(key=5,ytm = ytm,maturity = maturity,changeby = 0.0001)-ytm
keyratechange(key=7,ytm = ytm,maturity = maturity,changeby = 0.0001)-ytm

keyratechange(key=2,ytm = ytm,maturity = maturity,changeby = 0.0001)-ytm+
keyratechange(key=5,ytm = ytm,maturity = maturity,changeby = 0.0001)-ytm+
keyratechange(key=7,ytm = ytm,maturity = maturity,changeby = 0.0001)-ytm

keyratedv(ytm = ytm,maturity = maturity,CF = CF,key = 2,DV = T)
keyratedv(ytm = ytm,maturity = maturity,CF = CF,key = 5,DV = T)
keyratedv(ytm = ytm,maturity = maturity,CF = CF,key = 7,DV = T)

sum(keyratedv(ytm = ytm,maturity = maturity,CF = CF,key = 2,DV = T)+
        keyratedv(ytm = ytm,maturity = maturity,CF = CF,key = 5,DV = T)+
        keyratedv(ytm = ytm,maturity = maturity,CF = CF,key = 7,DV = T))