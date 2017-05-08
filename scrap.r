## these things are technically supposed to be taken care of
##I need to figure out if the previous code wasn't fully made generic

test_names <- function (dat,name) {
    return(dat[[ name ]][1:5])
}

## removing factor
for (col in names(Filter(is.factor,trimmed_data))) {
  trimmed_data[[col]] <- as.numeric(trimmed_data[[col]])
  ## how to deal with cases where factor doesn't become 1s and 0s
}

basedat = read.csv("conv2secMods05.csv")# this is the importing step for the data
trimmed_data  <- build_data(basedat,"Person","Couple","IBI","sexsat","female","timeCont")
crossed_data <- process_data(trimmed_data,c(1,2),c(5,6,7))
basic_co(crossed_data,1,1)
partnered_co(crossed_data,1,1)
moderator_co(crossed_data,1,1)
View(head(crossed_data))


## this is practice for the process of calculating the initial values for the osc model
library(nlme)
modLL <- lme(obs ~ 0 + Dist1 + Dist2 + Dist1:time + Dist2:time + Dist1:lowModResid + Dist2:lowModResid + Dist1:time:lowModResid + Dist2:time:lowModResid ,
             data=crossed_data,
             random = ~ 0 + Dist1 + Dist2 + Dist1:time + Dist2:time | Dyad  ,
             method="ML",
             na.action=na.omit,
             control=lmeControl(opt="optim"))

modLL_dist1 <- modLL$coefficients$fixed[1]
modLL_dist2 <- modLL$coefficients$fixed[2]
partner_fit <- partnered_co(crossed_data,1,1)
partner_fit$coefficients$fixed


library(deSolve)

t <- seq(0,100,by = .01)
pars <- c(eta = 0.5,zeta = 1.0)
init <- c(x = 1.0,y= 0)

prac <- function(time,state,params) {
  with(
    as.list(c(state,params)),{
      ## here's the formula line
      return(list(c(y,-eta*x + zeta*y)))
    }
  )
}

res <- ode(y = init,parms = pars,time = t,func = prac)

invert <- function(x) {
  return (1/x)
}
#
# p <- ggplot(data = as.data.frame(res),aes(time,y)) + geom_point()  + scale_y_continuous(labels = function(x))
# print(p)


### plugging in for actual formula
library(deSolve)
t <- seq(0,100,by =.5)
paramms <- c(dst1_resids = unname(partner_fit$coefficients$fixed[1]),
           dst1_freq = unname(partner_fit$coefficients$fixed[3]),
           dst2_resids_p = unname(partner_fit$coefficients$fixed[6]),
           dst2_freq_p = unname(partner_fit$coefficients$fixed[8]))

## the y and the z are the changes in the first two values.
state <- c(p = unname(modLL_dist1),n = unname(modLL_dist2),y = 0,z= 0)

modLL_dist2

cplosc <- function(time,state,pars) {
  gamp<-(dst1_resids - dst2_resids_p)
  gamn<-(dst2_resids_p - dst1_resids)
  with (
    as.list(c(state,pars)), {
      dy = dst1_freq*p + gamp*y + (dst1_freq*n  + dst2 )
      dz  = dst2_freq_p*n + gamn*z + ()
      return(list(c(y,1,dy,1)))
    }
  )
}

res <- ode(y = state,parms = paramms, func = cplosc,time = t)
View(as.data.frame(res))
library(ggplot2)
p <- ggplot(data = as.data.frame(res),aes(time)) + geom_point(aes(y = y),color="green") + geom_point(aes(y = n),color="blue") + geom_point(aes(y = z),color="red") + geom_point(aes(y = p),color="purple")
print(p)
### using theirs as guide
parameters <- c(dst1_freq = -8/3,
                b = -10,
                c = 28)
state <- c( X = 1,
            Y = 1,
            Z = 1)

lor <- function(time,state,parameters) {
  with(as.list(c(state,parameters)),{
    dx <- dst1_freq*X + Y*Z
    dy <- b*(Y-Z)
    dz <- -X*Y + c*Y -Z
    list(c(dx,dy,dz))
  })
}

t <- seq(0,100,by =.01)

res <- ode(y = state,parms = parameters,func = lor,times = t)



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

