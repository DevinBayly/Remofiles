## these things are technically supposed to be taken care of
##I need to figure out if the previous code wasn't fully made generic

library(dplyr)
source("R/GLLAfunctions.R")


basedat_physio <- read.csv("../findingbug/physio2.csv")
# basedat = read.csv("conv2secMods05.csv")# this is the importing step for the data

trimmed_data  <- build_data(basedat_physio,"id","dyad","dial","diastolic","sexm","time")
choose_tau_dim(trimmed_data,c(1),c(2))
drop_shorties <- trimmed_data %>%
  group_by(ID) %>%
  filter(length(ID)>15)

##experimenting with the creating of the strings to make the column headers need tehh assign function to make this easier
taus <- c(1,2,3)
dims <- c(4,5,6)

## making datafram from matrix, and then adding rows and columns
test_df <- data.frame(matrix(0,nrow=length(trimmed_data$ID),ncol=3))
#cehcking options for grouping and returning results to the test_DF
residualized <- makeResidCol(trimmed_data)
droped_residualized <- makeResidCol(drop_shorties)
droped_residualized %>%
  group_by(ID) %>%
  do(choose_tau_dim(.,c(1,2),c(5,6))) -> est_res

## source the derivatives estimate
source("../findingbug/derivatives_dial.R")
## and now subtract to check similarity or plot?
plot(est_res$resids_2_5 - derivatives_dial$dial,type = "l")
plot(est_res$resids_2_5 ,type = "l")




colnames(residualized)

residualized %>%
  group_by(ID) %>%
  do(runDerivativesEstimate(1,5,2,.))



theTau = 2
theEmbed = 5
residualized %>%
  filter(ID == 1) -> dat_param

residualized %>%
  group_by(ID) %>%
  do(choose_tau_dim(.,c(2),c(5)))

pid_two_est <- choose_tau_dim(residualized,c(2),c(5))

plot(derivatives_dial[1:24,2] - pid_two_est[1:24,1])
plot(derivatives_dial[1:24,3] - pid_two_est[1:24,2])
plot(derivatives_dial[1:24,4] - pid_two_est[1:24,3])


## checking estimates for second id

## trying to switch the estimate process to person by person, seems to be a big difference between current method and the previous version

