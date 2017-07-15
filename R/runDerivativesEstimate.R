#' process_by_group
#'
#' Essentially the main function for processing  all of our data. Calculates derivative estimates for each person in data, and then 
#' makes the flipped version of the data, and sets the correct column names.
#'
#'
#'
#' @param dat
#' @param taus
#' @param dims
#'
#' @return
#' @export
#'
#' @examples
#' process_by_group(equalized.dat,c(1),c(5,6),T)
#' process_by_group(equalized.dat,c(3),c(4,6))
#'
#'

process_by_group <- function(dat,taus,dims,extra_info=F) {
  person.tau.dim.est <- list()
  for( tau in taus) {
    for (dm in dims) {
      ids <- unique(dat$ID)
      mixed.output <- lapply(
        ids,
        function(x) {
          individual.dat <- subset(dat,dat$ID == x)
          runDerivativesEstimate(1,tau,dm,individual.dat,extra_info)}
      )
      ##create the res.list from the first elements in the nested list
      res.list <- lapply(mixed.output,`[[`,1) ## TODO look up what this backtick is doing
      res.df <- as.data.frame(do.call("rbind",res.list))
      if(extra_info) {
        ##establish a global variable with the outputs
        r.sqrd <- unlist(lapply(mixed.output,`[[`,2))
        assign(paste("r.sqrd",tau,dm,sep="."),r.sqrd,envir = .GlobalEnv)
      }
      #cross the data
      res.df <- crossPartners(res.df)
      # add the columns with the right nßame to the return list
      names <- sapply(colnames(res.df),function (x) {
        paste(x,tau,dm,sep=".")
      })
      colnames(res.df) <- names
      person.tau.dim.est <- c(person.tau.dim.est,list(res.df))
    }
  }
  return(person.tau.dim.est)
}


#' runDerivativesEstimate
#'
#' Uses the Stephen Boker GLLA code to calculate 1st and 2nd derivatives for the person data that has been passed in at the moment. 
#'
#'
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
runDerivativesEstimate = function (deltaTime,theTau,theEmbed,dat_param,est_info=F) {

  # print(dat_param$ID[1])

    obsMatrix <- gllaEmbed(dat_param$resids, embed = theEmbed, tau = theTau)

    # Calculate Local Linear Approximation of derivatives

    wMatrix <-
      gllaWMatrix(
        embed = theEmbed,
        tau = theTau,
        deltaT = deltaTime,
        order = 2
      )
    obsMatrixLLA.df <- as.data.frame(obsMatrix[, 2:dim(obsMatrix)[2]] %*% wMatrix)
    obsMatrixLLA.df$ID <- dat_param$ID[1]
    obsMatrixLLA.df$Dyad <- dat_param$Dyad[1]
    obsMatrixLLA.df$mod <- dat_param$mod[1]
    obsMatrixLLA.df$Dist0 <- dat_param$Dist0[1]
    obsMatrixLLA.df$Dist1 <- dat_param$Dist1[1]
    obsMatrixLLA.df$highMod <- dat_param$highMod[1]
    obsMatrixLLA.df$lowMod <- dat_param$lowMod[1]
    colnames(obsMatrixLLA.df) <- c("resids","d_resids","d2_resids","ID","Dyad","mod","Dist0","Dist1","highMod","lowMod")
    res.list <- list(obsMatrixLLA.df)
    ## optional information from estimate
    if (est_info == T) {

      treg_self <-
        lm(
            d2_resids ~ resids + d_resids  - 1,
            data = obsMatrixLLA.df,
            na.action = na.exclude
          )
        smmry.treg <- summary(treg_self)

        res.list <- c(res.list,smmry.treg$r.squared)
    }
    return(res.list)
}
