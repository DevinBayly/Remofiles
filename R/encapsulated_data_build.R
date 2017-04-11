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
build_data <- function(file_name,ID,Dyad,obs,mod,Dist,time_name) {
   basedat <- read.csv(file_name)
    ## naming of parameters has case selection to avoid prexisting variable
    ## names, appologies
    basedata  <- data.frame("ID" = basedata[[ID]],"Dyad" = basedata[[Dyad]],"obs" =basedata[[obs]],"time" = basedata[[time_name]],"mod" = basedata[[mod]],"Dist" = basedata[[Dist]])
    ## filter out the exitsing NA's
    basedata  <- basedata %>%
        filter(!(is.na(obs)))
    ## plot for people and pause for them to move on with the process
    print(chec_data_trellis(basedata))
    ## change original data to the changed df
    return(basedata)

}
