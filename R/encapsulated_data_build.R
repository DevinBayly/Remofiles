#' build_data
#'
#' @param basedata
#' @param ID
#' @param Dyad
#' @param obs
#' @param mod
#' @param Dist
#' @param time_name
#'
#' @importFrom magrittr %>%
#' @import dplyr
#'
#' @return
#' @export
#'
#' @examples
build_data <- function(basedata,ID,Dyad,obs,mod,Dist,time_name) {
    ## naming of parameters has case selection to avoid prexisting variable
    ## names, appologies
    basedata  <- data.frame("ID" = basedata[[ID]],"Dyad" = basedata[[Dyad]],"obs" =basedata[[obs]],"time" = basedata[[time_name]],"mod" = basedata[[mod]],"Dist1" = basedata[[Dist]])
    ## filter out the exitsing NA's
    basedata  <- basedata %>%
        filter(!(is.na(obs)))
    factor_cols <- names(Filter(is.factor,basedata))
    for (col in names(Filter(is.factor,basedata))) {
      basedata[[col]] <- as.numeric(basedata[[col]])
      #check to see if there is a number greater than 1 for either dist
      if ((length(trimmed_data$Dist2 >1) + length(trimmed_data$Dist1) > 1) > 0) {
        stop("Distinguisher Conversion from factor failed, please address in code")
      }
    }
    basedata["Dist2"] = as.numeric(!basedata$Dist)
    ## plot for people and pause for them to move on with the process
    print(chec_data_trellis(basedata))
    ## change original data to the changed df
    return(basedata)

}


