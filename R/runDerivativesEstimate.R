#' Title
#'
#' @param dat
#' @param taus
#' @param dims
#'
#' @return
#' @export
#'
#' @examples
process_by_group <- function(dat,taus,dims) {
  ids <- unique(dat$ID)
  res.list <- lapply(
    ids,
    function(x) {choose_tau_dim(dat[dat$ID == x,],taus,dims)}
  )
  ##must go over the combinations of the tau and dim here
  ## return the list of results
  res.list
}

#' Title
#'
#' @param dat
#' @param taus
#' @param dims
#'
#' @return
#' @export
#'
#' @examples
choose_tau_dim <- function (dat,taus,dims) {

  person.tau.dim.est <- list()
  for( tau in taus) {
    for (dm in dims) {
      res.df <- runDerivativesEstimate(1,dm,tau,dat)
      ## add the columns with the right name to the return list
      names <- sapply(colnames(res.df),function (x) {
        paste(x,tau,dm,sep=".")
      })
      colnames(res.df) <- names
      person.tau.dim.est <- c(person.tau.dim.est,list(res.df))
    }
  }
  person.tau.dim.est
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
    colnames(obsMatrixLLA.df) <- c("resids","d_resids","d2_resids","ID")
    return(obsMatrixLLA.df)
}
