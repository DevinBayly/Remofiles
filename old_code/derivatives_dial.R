#---------------------------------------------------------
# Calculate approximate derivatives for dial one person at a time so that dyads can have different numbers of 
# observations.
# Also doing one variable at a time because otherwise all the person's data is set to missing
# Include partner versions, since will need them in the CO model
data <- read.csv("physio.csv")

data <- subset(data, select= c("id","dial_pc","p_dial_pc"))

#        DeltaTime -- Interobservation interval
#           theTau -- Selected time delay 
#         theEmbed -- Selected number of embedding dimensions

deltaTime <- 1
theTau <- 2
theEmbed <- 5
## create a vector that can be used to sequentially to indicate ID 
## and a list called derivatives to collect results
newID <- unique(factor(data$id)) ## gain vector of just numbers included in the ids, no repeats
derivatives_dial <- list() ##make a simple list type

for (i in 1:length(newID)) {#this runs 88 times

datai <- data[data$id==newID[i],] # this could be done so much easier with dplyr right?
tMatrix <- as.matrix(datai) 
dimnames(tMatrix) <- list(NULL, c("id","dial_pc","p_dial_pc")) 
tFrame <- data.frame(tMatrix) # so we make a dataframe 

source("GLLAfunctions.R")

# Perform the state space embedding using theTau and theEmbed

dialMatrix <- gllaEmbed(tFrame$dial_pc, embed=theEmbed, tau=theTau)
p_dialMatrix <- gllaEmbed(tFrame$p_dial_pc, embed=theEmbed, tau=theTau)   

# Calculate Local Linear Approximation of derivatives

wMatrix <- gllaWMatrix(embed=theEmbed, tau=theTau, deltaT=deltaTime, order=2)
idLLA <- dialMatrix[,1]
dialMatrixLLA <- dialMatrix[,2:dim(dialMatrix)[2]] %*% wMatrix
p_dialMatrixLLA <- p_dialMatrix[,2:dim(p_dialMatrix)[2]] %*% wMatrix

# create ID variable of the right length, remove rows with missing data, and put into a list
id <- datai$id[1:length(idLLA)]
allMatrixLLA <- cbind(id, dialMatrixLLA[,1:3], p_dialMatrixLLA[,1:3])
allMatrixLLA <- allMatrixLLA[!is.na(apply(allMatrixLLA, 1, sum)),]
dimnames(allMatrixLLA) <- list(NULL, c("id","dial","ddial","d2dial","pdial","pddial","pd2dial"))
derivatives_dial[[i]] <- allMatrixLLA
}

derivatives_dial <- as.data.frame(do.call(rbind, derivatives_dial))
summary(derivatives_dial)

# Fit the damped linear oscillator model to get estimate of which settings give best derivative estimates

library(nlme) 
treg <- lme(d2dial ~  dial + ddial  - 1, random=list( ~ dial + ddial - 1 | id), 
            data=derivatives_dial, control=lmeControl(opt="optim"))
print(summary(treg))

cat(paste("\n average R^2 = "))
print(round(1 - (var(treg$residuals[,2]) / var(derivatives_dial$d2dial)),4))

cat("\n Average Cycle Length \n")

tLambda <- 2 * pi / sqrt(- treg$coefficients$fixed[1])
print(tLambda)

