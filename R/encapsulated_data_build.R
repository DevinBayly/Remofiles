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
        filter(!(is.na(obs)))%>%
      filter(!(is.na(mod)))
    factor_cols <- names(Filter(is.factor,basedata))
    for (col in names(Filter(is.factor,basedata))) {
      basedata[[col]] <- as.numeric(basedata[[col]])
      #check to see if there is a number greater than 1 for either dist
    }
    ## transform the dist numerics into indicators 1 or 0
    higher_dist <- max(unique(basedata$Dist1))
    lower_dist <- min(unique(basedata$Dist1))
    higher_ind <- basedata$Dist1 == higher_dist
    lower_ind <- basedata$Dist1 == lower_dist
    basedata$Dist1[higher_ind] <- 1
    basedata$Dist1[lower_ind] <- 0
    basedata[["Dist0"]] <- (as.numeric(!basedata$Dist1))
    ## plot for people and pause for them to move on with the process
    print(chec_data_trellis(basedata))
    ## change original data to the changed df
    return(basedata)

}


