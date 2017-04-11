#---------------------------------------------------------
# gllaWMatrix -- Calculates a GLLA linear transformation matrix to 
#                create approximate derivatives
#
# Input:  embed -- Embedding dimension 
#           tau -- Time delay 
#        deltaT -- Interobservation interval
#         order -- Highest order of derivatives (2, 3, or more)

gllaWMatrix <- function(embed, tau, deltaT, order=2) {
    L <- rep(1,embed)
    for(i in 1:order) {
        L <- cbind(L,(((c(1:embed)-mean(1:embed))*tau*deltaT)^i)/factorial(i)) 
    }
    return(L%*%solve(t(L)%*%L))
}


#---------------------------------------------------------
# gllaEmbed -- Creates a time-delay embedding of a variable 
#              given a vector and an optional grouping variable
#              Requires equal interval occasion data ordered by occasion.
#              If multiple individuals, use the ID vector as "groupby"
#
# Input:      x -- vector to embed
#         embed -- Embedding dimension (2 creates an N by 2 embedded matrix)
#           tau -- rows by which to shift x to create each time delay column 
#       groupby -- grouping vector
#         label -- variable label for the columns
#      idColumn -- if TRUE, return ID values in column 1
#                  if FALSE, return the embedding columns only.
#
# Returns:  An embedded matrix where column 1 has the ID values, and the
#           remaining columns are time delay embedded according to the arguments.

gllaEmbed <- function(x, embed=2, tau=1, groupby=NA, label="x", idColumn=TRUE) {
    
    minLen <- (tau + 1 + ((embed - 2) * tau))
    if (!is.vector(groupby) | length(groupby[!is.na(groupby[])])<1) {
        groupby <- rep(1,length(x))
    }
    x <- x[!is.na(groupby[])]
    groupby <- groupby[!is.na(groupby[])]
    if (embed < 2 | is.na(embed) | tau < 1 | is.na(tau) | 
        !is.vector(x) | length(x) < minLen)
        return(NA)
    if (length(groupby) != length(x))
        return(NA)
    embeddedMatrix <- matrix(NA, length(x) + (embed*tau), embed+1)
    colNames <- c("ID", paste(label, "0", sep=""))
    for (j in 2:embed) {
        colNames <- c(colNames, paste(label, (j-1)*tau, sep=""))
    }
    dimnames(embeddedMatrix) <- list(NULL, colNames)
    tRow <- 1
    for (i in unique(groupby)) {
        tx <- x[groupby==i]
        if (length(tx) < minLen)
            next
        tLen <- length(tx) - minLen
        embeddedMatrix[tRow:(tRow+tLen), 1] <- i
        for (j in 1:embed) {
            k <- 1 + ((j-1)*tau)
            embeddedMatrix[tRow:(tRow+tLen), j+1] <- tx[k:(k+tLen)]
        }
        tRow <- tRow + tLen + 1
    }
    if (idColumn==TRUE) {
        return(embeddedMatrix[1:(tRow-1),])
    }
    return(embeddedMatrix[1:(tRow-1), 2:(embed+1)])
}
