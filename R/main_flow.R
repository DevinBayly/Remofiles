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
  out.list  <- process_by_group(resid_df,taus,dims,extra_info)
  ## must iterate over the contents of the out.list and crosse each
  return(out.list)
}
