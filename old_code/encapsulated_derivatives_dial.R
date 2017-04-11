##todo make this into a function that accepts a filename argument and an argument to choose what column in the datafile to calculate the derivatives from.
library(lattice)
library(dplyr) ## this might need a require
##make table or matrix that has the combinations and the R^2 and the frequency period 
## todo prep for getting different data from the user.
source("GLLAfunctions.R")
source("envirovariables.R")
source("runDerivativesEstimate.R")
source("plotting_tool.R")

##think about putting this in, and have a scan that takes value from the user
xyplot(observation ~ time|factor(Dyad),group = ID, data=data,layout = c(4,4),type = "l",col= c("red","blue"))
## shouldn't need to make this interactive, just get the plot up, and they can take out the wrong stuff.
# cat("enter IDs that you would like excluded")
# selected_IDs =  aggregate_choices()
# filtering step
# make sure that the IDs are only being filtered if that's what the person specifies.
# data = data %>%
#   filter(!(ID %in% selected_IDs)) %>%
#   select(ID,observation,p_observation)

# data <- subset(data, select= c("ID","observation","p_observation"))## im pretty sure that these don't match eachother

##offer possibility of storing this list of toss indices todo




taus = c(1,2)
freq = numeric()
rsqrd = numeric()
freqDf = numeric()
RsqrdDf = numeric()
dims = c(3,5,7)
for (tau in taus){## current problem is that the taus aren't alternating in the way that we want
  for (dim in dims) {
    result = runDerivativesEstimate(1,dim,tau)
    freq = c(freq,round(result$freq,2))
    rsqrd = c(rsqrd,round(result$Rsqrd,2))
  }
}
Tau = sort(rep(taus,3)) ## we want to have both tau values matched with one of the dims
Dims = rep(dims,2)
df = data.frame(Tau,Dims,freq,rsqrd)
colnames(df) = c("Tau","Dims","Freq (s)","R^2")
View(df)

