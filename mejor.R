getwd()
setwd("C:/Users/Iliana/Documents/Caso")
mejor <- function(estado,resultado){
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    head(outcome)
    x <- levels(factor(outcome[,7]))
    v <- c("falla", "ataque", "neumonia")
    if (estado %in% x == F){
        stop("Estado no válido")
        
        break
        
    }
    
    if (resultado == "falla") r <- 11
    else if (resultado == "ataque") r <- 17
    else if (resultado == "neumonia") r <- 23
    else if (resultado %in% v == F){
        stop("resultado inválido")
        
        break
        
    }
    
    mydata <- outcome[outcome$State == estado,]
    mnd <- mydata[,c(2,r)]
    if (sum(mnd[,2]=="Not Available") < 1) {
        out <- mnd[order(as.numeric(mnd[,2])),]
        out2 <- out[which(out[,2] == out[1,2]),]
        fo <- out2[order(out2[,1]),]
        fo[1,1]
        
        
        
    }
    
    else {
        
        final <- mnd[- grep("Not", mnd[,2]),]
        out <- final[order(as.numeric(final[,2])),]
        out2 <- out[which(out[,2] == out[1,2]),]
        fo <- out2[order(out2[,1]),]
        fo[1,1]
        
    }
    
}


mejor("TX", "ataque")
mejor("TX", "falla")
mejor("MD", "ataque")
mejor("MD", "neumonia")
mejor("BB", "ataque")
mejor("NY", "ataque")






