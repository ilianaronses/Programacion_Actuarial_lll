setwd("C:/Users/Iliana/Documents/Caso")
rankingcompleto <- function(resultado, num = "mejor") {
    outcome <- read.csv("outcome-of-care-measures.csv")
    v <- c("ataque", "falla", "neumonia")
    if (resultado == "ataque") col <- 11
    else if (resultado == "falla") col <- 17
    else if (resultado == "neumonia") col <- 23
    else if (resultado %in% v == F){
        stop("resultado inválido")
        
    }
    
    outcome[, col] <- suppressWarnings(as.numeric(levels(outcome[, col])[outcome[, col]]))
    outcome[, 2] <- as.character(outcome[, 2])

    output <- vector() 
    estados <- levels(outcome[, 7])
    for(i in 1:length(estados)) {
       databystate <- outcome[grep(estados[i], outcome[,7]), ]
        ordered <- databystate[order(databystate[, col], databystate[, 2], na.last = NA), ]
        if(num == "mejor") hospital <- ordered[1, 2]
        else if(num == "peor") hospital <- ordered[nrow(ordered), 2]
        else hospital <- ordered[num, 2]
        output <- append(output, c(hospital, estados[i]))
    }
    
    output <- as.data.frame(matrix(output, length(estados), 2, byrow = TRUE))
    colnames(output) <- c("hospital", "state")
    
    rownames(output) <- estados
    
    output
    
}


head(rankingcompleto("ataque", 20), 10)
tail(rankingcompleto("neumonia", "peor"), 3)
tail(rankingcompleto("falla"), 10)

