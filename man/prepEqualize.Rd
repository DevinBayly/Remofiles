% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr_actor_partner_cross.R
\name{prepEqualize}
\alias{prepEqualize}
\title{prepEqualize}
\usage{
prepEqualize(dat)
}
\arguments{
\item{dat}{}
}
\value{
dataframe where none of the timsteps inbetween the min and the max are missing from either partner. Basically means the lengths of the partners data matches.
}
\description{
this function helps to take data from the original dataframe, and break it into parts that can be equalized with the equalizeTimes function.
}
