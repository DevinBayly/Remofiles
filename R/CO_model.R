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
run_co_fitting <- function(data,tau,dim) {
  ## for the moment, the tau dim selection will be hard coded, but changed

  fit <- lme(d2_resids_1_3 ~ Dist1:resids + Dist2:resids + Dist1:d_resids_1_3 + Dist2:d_resids_1_3 + Dist1_p:resids_p + Dist2_p:resids_p + Dist1_p:d_resids_1_3_p + Dist2_p:d_resids_1_3_p - 1,
               random=list(~ resids + d_resids_1_3 + resids_p + d_resids_1_3_p  - 1 | Dyad), data=data, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
summary(fit)
}
