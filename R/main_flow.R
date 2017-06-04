#' Title
#'
#' @param basedata
#'
#' @return
#' @export
#'
#' @examples
process_data <- function(basedata,taus,dims) {

  resid_df  <- makeResidCol(basedata)
  out.list  <- process_by_group(resid_df,taus,dims)
  # crossed_dat  <- crossActorPartner(outDf,length(taus)*length(dims)*2)
  # return (crossed_dat)
  return(out.list)
}
