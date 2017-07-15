## these things are technically supposed to be taken care of
##I need to figure out if the previous code wasn't fully made generic

library(dplyr)


basedat_physio <- read.csv("../findingbug/physio2.csv")
# basedat = read.csv("conv2secMods05.csv")# this is the importing step for the data

#note that the id is being used again becaus there don't seem to be other good choices for moderators
trimmed_data  <- build_data(basedat_physio,"id","dyad","dial","diastolic","sexm","time")

no.dyad.check <- function (dat) {
  if (length(unique(dat$ID)) <2) {
    return (data.frame())
  } else {
    return (dat)
  }
}

drop_shorties <- trimmed_data %>%
  group_by(ID) %>%
  filter(length(ID)>15)

toss.non.dyads <- drop_shorties %>%
  group_by(Dyad) %>%
  do(no.dyad.check(.))

taus <- c(2,3,4)
dms <- c(5,6,7)
for (tau in taus) {
  for (dm in dms) {
    list.res <- process_data(toss.non.dyads,tau,dm)
    check.data <- list.res[[1]]
    fit <- base_co(check.data,tau,dm)
    capture.output(summary(fit),file = paste("../fit_summary",tau,dm,"txt",sep = "_"))
  }
}
## looking for missing dyads
