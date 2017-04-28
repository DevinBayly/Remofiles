## these things are technically supposed to be taken care of
##I need to figure out if the previous code wasn't fully made generic

test_names <- function (dat,name) {
    return(dat[[ name ]][1:5])
}

## removing factor
for (col in names(Filter(is.factor,trimmed_data))) {
  trimmed_data[[col]] <- as.numeric(trimmed_data[[col]])
  ## how to deal with cases where factor doesn't become 1s and 0s
  if
}

basedat = read.csv("conv2secMods05.csv")# this is the importing step for the data
trimmed_data  <- build_data(basedat,"Person","Couple","IBI","sexsat","sexf","timeCont")
crossed_data <- process_data(trimmed_data)
run_co_fitting(crossed_data)
View(head(crossed_data))


#debugging commands

basedata %>%
  filter(ID == 501) %>%
  mutate( dist0= as.numeric(Dist1))


## this is the space fortesting the centering, and residual, and mod column stuf,z

## oh yea, the resids from the obs must be cleaned out

resid_df  <- makeResidCol(basedat)
dyadlabs  <- factor(resid_df$Dyad,levels = unique(resid_df$Dyad),labels <- unique(resid_df$Dyad))
resid_df %>%
    group_by(Dyad) %>%
    xyplot(resids~time|dyadlabs,data = .,groups=Dist,type="l",main=.$Dyad)

## I think maybe the last runderiv was used after the cross happened, but that
## seems like more work, so lets just rewrite the thing making use of the dplyr
## stuff again

source("./GLLAfunctions.R")


## testing whether we can go through multiple tau, dim things

outDf  <- choose_tau_dim(resid_df)
write.table(outDf,file = "success_emily_review.csv",sep = ',',append= FALSE)
# seems to have worked over the different values!!!

## test the cross on the full data
crossed_dat  <- crossActorPartner(outDf)
## incremental testing of cross

