bondprice <- function(ytm, maturity, CF){
    if (length(ytm)<length(CF)){
        stop("Insufficient spot rate")
    }
    discount <- 1/(1+ytm)^maturity
    pv <- discount*CF
    price <- sum(pv)
    return(price)
}

duration <- function(ytm, maturity, CF, DV=FALSE){
    if (length(ytm)<length(CF)){
        stop("Insufficient spot rate")
    }
    change <- 0.0001
    price_mid <- bondprice(ytm, maturity, CF)
    price_up <- bondprice(ytm+change, maturity, CF)
    price_down <- bondprice(ytm-change, maturity, CF)
    dv <- (price_down - price_up)/2
    duration <- dv/change/price_mid
    if(DV){
        return(dv)
    }else{
        return(duration)
    }
}

convexity <- function(ytm, maturity, CF, DV=FALSE){
    if (length(ytm)<length(CF)){
        stop("Insufficient spot rate")
    }
    deltaytm <- 0.0001
    price_mid <- bondprice(ytm, maturity, CF)
    price_up <- bondprice(ytm+deltaytm, maturity, CF)
    price_down <- bondprice(ytm-deltaytm, maturity, CF)
    convexity <- (price_down + price_up -2*price_mid)/(deltaytm^2)/price_mid
    return(convexity)
}

keyratechange <- function(key=2, allkey=c(2,5,7), ytm, maturity, changeby=0.0001){
    if (key==allkey[1]){
        keyrate <- ytm[which(maturity==key)]
        nextkey <- allkey[2]
        nextkeyrate <- ytm[which(maturity==nextkey)]
        length = which(maturity<=nextkey)
        for(i in length){
            if (maturity[i]<=key){
                ytm[i] <- ytm[i]+changeby
            }
            if (maturity[i]>key){
                ytm[i] <- keyrate+changeby + (nextkeyrate - (keyrate+changeby))/(nextkey - key)*(maturity[i] - key)
            }
        }
    }
    if (key==allkey[length(allkey)]){
        keyrate <- ytm[which(maturity==key)]
        prekey <- allkey[which(allkey==key)-1]
        prekeyrate <- ytm[which(maturity==prekey)]
        length = which(maturity>=prekey)
        for(i in length){
            if (maturity[i]>=key){
                ytm[i] <- ytm[i]+changeby
            }
            if (maturity[i]<key){
                ytm[i] <- prekeyrate + (keyrate+changeby - prekeyrate)/(key - prekey)*(maturity[i] - prekey)
            }
        }
    }
    if (key>allkey[1]&key<allkey[length(allkey)]){
        keyrate <- ytm[which(maturity==key)]
        prekey <- allkey[which(allkey==key)-1]
        prekeyrate <- ytm[which(maturity==prekey)]
        nextkey <- allkey[which(allkey==key)+1]
        nextkeyrate <- ytm[which(maturity==nextkey)]
        length = which(maturity>=prekey&maturity<=nextkey)
        for(i in length){
            if (maturity[i]>=key){
                ytm[i] <- keyrate+changeby + (nextkeyrate - (keyrate+changeby))/(nextkey - key)*(maturity[i] - key)
            }
            if (maturity[i]<key){
                ytm[i] <- prekeyrate + (keyrate+changeby - prekeyrate)/(key - prekey)*(maturity[i] - prekey)
            }
        }
    }
    return(ytm)
}


keyratedv <- function(ytm, maturity, CF, key, DV=TRUE){
    price_mid <- bondprice(ytm = ytm, maturity = maturity, CF = CF)
    ytm_up <- keyratechange(key = key,ytm = ytm,maturity = maturity) 
    price_up <- bondprice(ytm = ytm_up, maturity = maturity, CF = CF)
    dv <- price_mid-price_up
    duration <- dv*10000/price_mid
    if(DV){
        return(dv)
    }else{
        return(duration)
    }
}
