#' process_data
#'
#' this function handles the critical step of taking the data post build,
#' and processes it to a form that can be run through the actual coupled
#' oscillator functions. Takes tau and dim argument for derivative estimation step
#'
#' extra-info populates a global environment variable with the R^2 values for the derivative estimates, to assist in determining whether these estimates are of any use
#'
#'
#' includes

#'   creation of residual column --
#'   equalization of each partners data length--
#'   actual locallinear approximation (using Steven Boker's GLLA code --see references) for 1st and second derivatives --
#'   crossing of the actor and partner --
#'
#' @param basedata
#' @param taus
#' @param dims
#' @param extra_info
#'
#' @return dataframe with 1st, 2nd derivatives, residual column, high/low Moderator, ID, Dyad, time columns
#'
#'
#'
#'
#' @export
#'
#' @examples
#'
#' #tau 1 dim 6 extra info T
#' process_data(build.cleaned.data,c(1),c(6),T)
#' #tau 2,3 dim 5,6,7 extra info F
#' process_data(build.cleaned.data,c(2,3),c(5,6,7),F)
process_data <- function(basedata,taus,dims,extra_info=F) {
  resid_df  <- makeResidCol(basedata)
  ##perform partner equalize
  equalized.data<-makeEqual(resid_df)
  out.list  <- process_by_group(equalized.data,taus,dims,extra_info)
  ## must iterate over the contents of the out.list and crosse each
  return(out.list)
}
