#' Title
#'
#' @param basedata
#'
#' @return
#' @export
#'
#' @examples
process_data <- function(basedata,taus,dims,extra_info=F) {
  resid_df  <- makeResidCol(basedata)
  ##perform partner equalize
  equalized.data<-crossActorPartner(resid_df)
  out.list  <- process_by_group(equalized.data,taus,dims,extra_info)
  ## must iterate over the contents of the out.list and crosse each
  return(out.list)
}
