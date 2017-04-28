
choose_tau_dim <- function (dat) {

    taus = c(1,2)
    dims = c(3,5,7)

    for( tau in taus) {
        for (dm in dims) {
            dat  <- runDerivativesEstimate(1,dm,tau,dat)
        }
    }
    return(dat)
}


#' runDerivativesEstimate
#'
#' @param deltaTime
#' @param theEmbed
#' @param theTau
#' @param dat_param
#'
#' @importFrom nlme lme lmeControl
#'
#' @return
#' @export
#'
#' @examples
runDerivativesEstimate = function (deltaTime,theEmbed,theTau,dat_param) {

    obsMatrix <- gllaEmbed(dat_param$resids, embed = theEmbed, tau = theTau)

    # Calculate Local Linear Approximation of derivatives

    wMatrix <-
      gllaWMatrix(
        embed = theEmbed,
        tau = theTau,
        deltaT = deltaTime,
        order = 2
      )
    obsMatrixLLA <- obsMatrix[, 2:dim(obsMatrix)[2]] %*% wMatrix

    ## conversion to df just makes this technique of adding rows easier
    dfobsMatLLA  <- as.data.frame(obsMatrixLLA)
    ## and now add an empty row at the top and the bottom.
    dfobsMatLLA[seq(2,nrow(obsMatrixLLA)+1),]  <- dfobsMatLLA[seq(1,nrow(obsMatrixLLA)),]
    dfobsMatLLA[1,]  <- NA ## make the first row NA
    ## now add correct padding for end of dataframe
    dfobsMatLLA[nrow(dfobsMatLLA) +(theEmbed -1)*theTau-1,]  <- NA
    ## the embed -2 tells us that the wrapping by embed degree changes the number of rows dif in length our dataframe isfrom the matLLA, this is teh value we need to pad by



    ## this is where the actual tau and embed labels would get attached
    ## adding in the derivative columns
    dat_param  <- dat_param %>%
    mutate(d_resids = dfobsMatLLA[,2],
           d2_resids = dfobsMatLLA[,3])

  # Fit the damped linear oscillator model to get estimate of which settings give best derivative estimates

    ## question, this is just for the one person right? doesn't involve the partner? it seems?
    ## do we still need the r^2 stuff we are keeping all the variations...
  # library(nlme)
  treg_self <-
    lme(
      d2_resids ~ resids + d_resids  - 1,
      random = list(~resids + d_resids - 1 | ID),
      data = dat_param,
      control = lmeControl(opt = "optim"),
      na.action = na.exclude
    )
  tLambda_self <- 2 * pi / sqrt(abs(treg_self$coefficients$fixed[1]))
  dat_param  <- dat_param %>%
      mutate(freq = tLambda_self)
  ## and now we have to rename the columns

  ## it goes tau then dim ok??
  names(dat_param)[names(dat_param) == 'd_resids'] <- paste('d_resids_',theTau,'_',theEmbed,sep='')
  names(dat_param)[names(dat_param) == 'd2_resids'] <- paste('d2_resids_',theTau,'_',theEmbed,sep='')
  names(dat_param)[names(dat_param) == 'freq'] <- paste('freq_',theTau,'_',theEmbed,sep='')
  return(dat_param)
}
