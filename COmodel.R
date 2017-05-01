

library(nlme)

data <- read.csv("data_dialCO.csv")

## I realized another processing step we need to do just before fitting the models - we will want to compare nested models and that means they each need to be estimated on exactly the same data. But when we get to the moderated model we may have additional missing values if the moderator(s) have any missing. So, I think we need to pull out the variables from the bigger data set that we are going to need for a given analysis (e.g., derivatives for one setting of Tau/Dim, just the desired moderators, etc) and remove all rows with missing values.

## I also just realized we will need a time variable to have been carried forward to here (e.g., a variable that indicates the time point for each observation) - we will need this for the model to estimate start values (see below).

# subset and remove missing data
data <- subset(data, select=c(d2dial, male, female, dial, ddial, pdial, pddial, partsat1, Dyad))

dataNoNA <- data[complete.cases(data), ]


## New form of the CLO model

reduced <- lme(d2dial ~ male:dial + female:dial + male:ddial + female:ddial + male:pdial + female:pdial + male:pddial + female:pddial - 1,
			random=list(~ dial + ddial + pdial + pddial  - 1 | Dyad), data=dataNoNA, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
summary(reduced)


reducedMod <- lme(d2dial ~ male:dial + female:dial + male:ddial + female:ddial + male:pdial + female:pdial + male:pddial + female:pddial + male:partsat1 + female:partsat1 + male:dial:partsat1 + female:dial:partsat1 + male:ddial:partsat1 + female:ddial:partsat1 + male:pdial:partsat1 + female:pdial:partsat1 + male:pddial:partsat1 + female:pddial:partsat1 - 1,  random=list(~ dial + ddial + pdial + pddial  - 1 | Dyad), data=dataNoNA, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))

summary(reducedMod)
anova(reduced, reducedMod) # see the moderated model is worse than the simpler model


# the next step if we found the moderated model was better would be to decompose it to get separate parameter estimates for high and low combinations of the moderator - see below for an example of this process with the model for estimating start values.

################################
#################################

## Model for estimating start values


data_w_time <- read.csv("example.csv") # using different data due to needing a time variable
str(data)
summary(data$relsat) # centering moderator at high/low values - won't normally need to do this since it's part of your preprocessing
data$relsatL <- data$relsat - 2.5
data$relsatH <- data$relsat - 3

# model with both partner's low
initialLL <- lme(IBI ~ 0 + male + female + male:time + female:time + male:relsatL + female:relsatL + male:time:relsatL + female:time:relsatL, random = ~ 0 + male + female + male:time + female:time | Couple, data=data, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))

# model with male low and female H
initialLH <- lme(IBI ~ 0 + male + female + male:time + female:time + male:relsatL + female:relsatH + male:time:relsatL + female:time:relsatH, random = ~ 0 + male + female + male:time + female:time | Couple, data=data, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))

# model with male high and female L
initialLH <- lme(IBI ~ 0 + male + female + male:time + female:time + male:relsatH + female:relsatL + male:time:relsatH + female:time:relsatL, random = ~ 0 + male + female + male:time + female:time | Couple, data=data, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))

# model with both high
initialLH <- lme(IBI ~ 0 + male + female + male:time + female:time + male:relsatH + female:relsatH + male:time:relsatH + female:time:relsatH, random = ~ 0 + male + female + male:time + female:time | Couple, data=data, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))


## The estimated start values for each type of dyad (e.g., as distinguished by the moderator) are the "male" and "female" estimates in the summary output


################################
#################################


##### Old form of CLO model

full <- lme(d2dial ~ male:dial + female:dial + male:ddial + female:ddial + male:coupling + female:coupling- 1,
			random=list(~ male:dial + female:dial + male:ddial + female:ddial + male:coupling + female:coupling - 1 | Dyad), data=data, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
summary(full)


reduced <- lme(d2dial ~ male:dial + female:dial + male:ddial + female:ddial + male:coupling + female:coupling- 1,
			random=list(~ dial + ddial + coupling  - 1 | Dyad), data=data, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
summary(reduced)

anova(full, reduced)









