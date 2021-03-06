% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr_actor_partner_cross.R
\name{equalizeTimes}
\alias{equalizeTimes}
\title{equalize times}
\usage{
equalizeTimes(dat, upper.time)
}
\arguments{
\item{dat}{}
}
\value{
a dataframe of correct length for the dyad
}
\description{
this function makes missing values for observations at a particular time step
 become NA. This makes it possible for equal length data in the ensuing steps of the coupled osc.
 Additional important columns are filled in also.
}
\examples{
equalizeTimes(personal.df,max.time)
}
