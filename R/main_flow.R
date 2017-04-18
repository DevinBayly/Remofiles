#' Title
#'
#' @param basedata
#'
#' @return
#' @export
#'
#' @examples
process_data <- function(basedata) {
  resid_df  <- makeResidCol(basedata)
  outDf  <- choose_tau_dim(resid_df)
  crossed_dat  <- crossActorPartner(outDf)
  return (crossed_dat)
}
