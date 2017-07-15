## this function just houses a function that can be used to give a big overview plot of the observation overtime for each Dyad with the partners within.
#' chec_data_trellis
#'
#' This function is used to help visualize whether the data coming into the build_data function is poorly edited. The package doesn't have extensive checking steps in order to keep it is general as possible to the varieties of Dyadic Timeseries data that may exist.
#'
#' The idea is to use check the plots and return to the raw csv to edit out what is necessary.
#'
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @import lattice
#' @importFrom magrittr %>%
#' @import dplyr
#'
#' @examples
chec_data_trellis <- function (dat) {
     dyadlabs  <- factor(dat$Dyad,levels = unique(dat$Dyad),labels <- unique(dat$Dyad))



    dat %>%
        group_by(Dyad) %>%
        xyplot(obs~time|dyadlabs,data = .,groups=Dist1,type="l",main=.$Dyad,layout = c(5,5))-> plot_to_return## i really still don't get it but adding the dyad there makes me see what i want
    return (plot_to_return)

}
