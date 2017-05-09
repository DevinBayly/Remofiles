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
  d2 <- paste("d2_resids",tau,dim,sep = "_")
  d <- paste("d_resids",tau,dim,sep = "_")
  dp <- paste("d_resids",tau,dim,"p",sep = "_")
  fitstring <-paste(d2,
                    paste("Dist1:resids + Dist0:resids + Dist1:",d," + Dist0:",d," + Dist1:resids_p +Dist0:resids_p + Dist1:",dp, " + Dist0:",dp," - 1",sep = "")
                    ,sep = " ~ ")
  fit_fmla <- as.formula(fitstring)
  randstring <- paste(" ~ resids + ",d," + resids_p + ",dp, " - 1 | Dyad",sep = "")
  rand_fmla <- as.formula(randstring)

  fit <- lme(fit_fmla,random=list(rand_fmla), data=crossed_data, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
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
  data <- data[complete.cases(data),]
  d2 <- paste("d2_resids",tau,dim,sep = "_")
  d <- paste("d_resids",tau,dim,sep = "_")
  dp <- paste("d_resids",tau,dim,"p",sep = "_")
  fitstring <-paste(d2," ~ Dist0:resids + Dist1:resids + Dist0:", d ,"  + Dist1:", d ,"  + Dist0:resids_p + Dist1:resids_p + Dist0:", dp ,"  + Dist1:", dp ,"  + Dist0:mod + Dist1:mod + Dist0:resids:mod + Dist1:resids:mod + Dist0:", d ," :mod + Dist1:", d ," :mod + Dist0:resids_p:mod + Dist1:resids_p:mod + Dist0:", dp ," :mod + Dist1:", dp ," :mod - 1")
  fitfmla <- as.formula(fitstring)
  randomstring <- paste("~ resids + ", d ,"  + resids_p + ", dp ,"   - 1 | Dyad")
  randfmla <- as.formula(randomstring)
  fit <- lme(fitfmla,  random=list(randfmla), data=data, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
  summary(fit)
  return(fit)
}

