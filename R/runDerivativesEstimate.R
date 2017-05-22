

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
  ## fastest way to do this is to preallocate the amount of space that we need, I think we can count on the fact that there will always be less estimated rows than the
  ret_df <- data.frame(matrix(0,ncol=length(taus)*length(dims)*3,nrow=length(dat$obs)))
  ret_df_names <- c()
  ## and then we can just fill in after
    # cat("freq,R^2,tau,dim\n")
  # we will move the pos to the right row position to insert the next individual's data
  col_pos <- 1
  row_pos <- 1
  ## think of a way to use the dplyr grouping to give use the individual data we need
  for( tau in taus) {
    for (dm in dims) {
      ind.est.data <- runDerivativesEstimate(1,dm,tau,dat)
      ## ind.est.data will have several columns, each same len, now place in the matrix
      size <- length(ind.est.data[,1])
      ret_df[row_pos:(row_pos+size-1),
             col_pos:(col_pos+2)] <- ind.est.data
      ret_df_names <- c(ret_df_names,
                        paste("resids",tau,dm,sep="_"),
                        paste("d_resids",tau,dm,sep = "_"),
                        paste("d2_resids",tau,dm,sep="_"))
      col_pos =col_pos+3
    }
  }
  #substitute in the colnames that we created
  colnames(ret_df) <- ret_df_names

  return(ret_df)
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

  print(dat_param$ID[1])

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
    return(as.data.frame(obsMatrixLLA))

  # library(nlme)
  # treg_self <-
  #   lme(
  #     d2_resids ~ resids + d_resids  - 1,
  #     random = list(~resids + d_resids - 1 | ID),
  #     data = dat_param,
  #     control = lmeControl(opt = "optim"),
  #     na.action = na.exclude
  #   )
  # if(treg_self$coefficients$fixed[1] > 0) {
  #   tLambda_self <- NA
  # } else {
  #   tLambda_self <- 2 * pi / sqrt(-(treg_self$coefficients$fixed[1]))
  # }
  # ## is this the right formula for the R^2?
  # ## the ind helps make sure that only non NA values get used in the var
  # ind <- !is.na(dat_param$d2_resids)
  # Rsq <- round(1 - (var(treg_self$residuals[,2]) / var(dat_param$d2_resids[ind])),4)
  # cat(tLambda_self,Rsq,theTau,theEmbed,"\n")
  # ## and now we have to rename the columns
  #
  # ## it goes tau then dim ok??
  # return(dat_param)
}
