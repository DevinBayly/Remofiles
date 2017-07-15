
#' getrsdls
#' 
#' This function used to actually use the residuals from a fit line around a particular person's data, but now we simply use the mean. Could go back to the previous version sometime.
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
getrsdls <- function (dat) {
    # rename observable to be the observable for clarification
    # example being getrsdls ("IBI","time",dat)

    dat_mean <- mean(dat$obs)
    return(dat$obs -dat_mean)
    # f  <- paste("obs","~","time")# makes it possible to havestring formula
    # fit  <- lm(f,data=dat)
    # return(summary(fit)$residuals)
}

#' addlevelMods
#'
#' This function adds the high and low moderators to the data as columns in the dataframe.
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
addlevelMods  <- function (dat){
    modLevels = quantile(dat$mod,c(.25,.75))
    outdat  <- dat %>%
        mutate(highMod = mod-modLevels[2] ,
               lowMod = mod -modLevels[1])
    return(outdat)

}

#' makeResidCol
#'
#' This function is used tot place teh residual column in the actual dataframe.
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
makeResidCol <- function (dat) {
    returnVect= c()
    pids = unique(dat$ID) 
    for (pid in pids) {
        dat %>%
            filter(ID==pid) -> pcdat
        tempres = getrsdls(pcdat)
        returnVect = c(returnVect,tempres)
    }
    dat[["resids"]] =returnVect
    outdat = addlevelMods(dat)
    return(outdat)
}

