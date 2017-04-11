####
#### Cleaning data for CO model

##we will need to source the runDerivativesFunction
source("runDerivativesEstimate.R")
##we now have access to the runDerivatesEstimate function.

data_survey <- read.csv("survey.csv", header=TRUE)
data_physio <- read.csv("physio.csv", header=TRUE)

# check which IDs have no physio data, then remove them from the survey too
unique(data_physio$id)
unique(data_survey$ID)
data_survey <- subset(data_survey, data_survey$ID != 39 & data_survey$ID != 506)

# fix the fact that id is lower case in one file and upper case in the other
data_survey$id <- data_survey$ID
data_survey <- subset(data_survey, select= -c(ID))

# visually check dial data for dyads with one or both partners missing all data
# and for non-varying (garbage) data, then create clean data file for dial
require(lattice) ## remember that library does what require does, but require will only present a warning if the package isn't found, whereas library crashes program.
xyplot(dial_pc ~ time|factor(id), data=data_physio, layout=c(3,2)) ##tilde means dial_pc is a function of time, and attach the factor(id)
#remove dyads 1, 4 and 43
data_dial <- subset(data_physio, select = c(id, dial_pc, p_dial_pc))
data_dial <- subset(data_dial, id != 1 & id!= 501 & id != 4 & id != 504 & id != 43 & id != 543) ## this can be replaced with a dplyr select probably, we are saying don't give these right?
##filter(data,!(id %in% c(1,501,4,504,43,543)))
summary(data_dial)

### merge data for the coupled-oscillator 
### First create separate files for different frequency estimates - do this by running the derivatives code
### at different settings and saving the resulting file with a name that indicates the rough frequency estimate
### based on those setting.

##todo make sure that the derivatives_dial files has been run by this point, because otherwise it won't have a variable to refer to "derivatives_dial" by the next line
##todo make the variable derivatives_dial be indexed by experimentlike 10 or 08 whatnot


derivatives_dial7 = runDerivativesEstimate(1, 3, 1,data_dial)$deriv_dial ##the indexing has to do with what gets returned.
##todo make the return into an object with difnrfeet . attributes
derivatives_dial10 = runDerivativesEstimate(1, 3, 2,data_dial)$deriv_dial
derivatives_dial18 = runDerivativesEstimate(1, 7, 2,data_dial)$deriv_dial
derivatives_dial25 = runDerivativesEstimate(1, 7, 2,data_dial)$deriv_dial


data_dialCO7 <- merge(data_survey, derivatives_dial7, by="id") ## run with settings 1,1,3 this step maybe should have already been done?
data_dialCO10 <- merge(data_survey, derivatives_dial10, by="id") ## run with settings 1,2,3
data_dialCO18 <- merge(data_survey, derivatives_dial18, by="id") ## run with settings 1,2,5
data_dialCO25 <- merge(data_survey, derivatives_dial25, by="id") ## run with settings 1,2,7

# calculate coupling variable for each file
data_dialCO7$coupling <- data_dialCO7$pdial - data_dialCO7$dial ## here pdial is the partners dial score
data_dialCO10$coupling <- data_dialCO10$pdial - data_dialCO10$dial
data_dialCO18$coupling <- data_dialCO18$pdial - data_dialCO18$dial
data_dialCO25$coupling <- data_dialCO25$pdial - data_dialCO25$dial

# make sexf a factor for each file, reverse code them, and remove sexm to avoid confusion since R does it opposite of SAS
data_dialCO7$sexf <- as.factor(data_dialCO7$sexm)
data_dialCO7 <- subset(data_dialCO7, select = -c(sexm)) ## I wonder what the - infront of the c does maybe exclude?
data_dialCO10$sexf <- as.factor(data_dialCO10$sexm)
data_dialCO10 <- subset(data_dialCO10, select = -c(sexm))
data_dialCO18$sexf <- as.factor(data_dialCO18$sexm)
data_dialCO18 <- subset(data_dialCO18, select = -c(sexm))
data_dialCO25$sexf <- as.factor(data_dialCO25$sexm)
data_dialCO25 <- subset(data_dialCO25, select = -c(sexm))








##so let me see here we have a total of id, dialpc, p_dialpc, sexm, sexf 
