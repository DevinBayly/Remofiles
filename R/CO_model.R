#' Title
#'
#' @param data
#' @param tau
#' @param dim
#'
#' @return
#' @export
#'
#' @examples
base_co <- function(data,tau,dim) {
  ## for the moment, the tau dim selection will be hard coded, but to be changed
  ## trim the data each time
  data <- data[complete.cases(data),]
  fit <- lme(d2_resids_1_3 ~ Dist1:resids + Dist2:resids + Dist1:d_resids_1_3 + Dist2:d_resids_1_3 + Dist1:resids_p + Dist2:resids_p + Dist1:d_resids_1_3_p + Dist2:d_resids_1_3_p - 1,
               random=list(~ resids + d_resids_1_3 + resids_p + d_resids_1_3_p  - 1 | Dyad), data=data, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
  summary(fit)
  return(fit)
}

#' Title
#'
#' @param data
#' @param tau
#' @param dim
#'
#' @return
#' @export
#'
#' @examples
moderator_co <- function(data,tau,dim) {
  data <- data[complete.cases(),]
  fit <- lme(d2_resids_1_3 ~ Dist2:resids + Dist1:resids + Dist2:d_resids_1_3 + Dist1:d_resids_1_3 + Dist2:resids_p + Dist1:resids_p + Dist2:d_resids_1_3_p + Dist1:d_resids_1_3_p + Dist2:mod + Dist1:mod + Dist2:resids:mod + Dist1:resids:mod + Dist2:d_resids_1_3:mod + Dist1:d_resids_1_3:mod + Dist2:resids_p:mod + Dist1:resids_p:mod + Dist2:d_resids_1_3_p:mod + Dist1:d_resids_1_3_p:mod - 1,  random=list(~ resids + d_resids_1_3 + resids_p + d_resids_1_3_p  - 1 | Dyad), data=data, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
  summary(fit)
  return(fit)
}

