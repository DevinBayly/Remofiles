
#' equalize times
#'
#' this function makes missing values for observations at a particular time step
#'  become NA. This makes it possible for equal length data in the ensuing steps of the coupled osc.
#'  Additional important columns are filled in also.
#'
#' @param upper.time
#' @param dat
#'
#' @return a dataframe of correct length for the dyad
#' @export
#'
#' @examples
#' equalizeTimes(personal.df,max.time)
equalizeTimes <- function (dat,upper.time) {
  time <-1
  i <- 1
  t.buffered.obs<-list(obs=c(),time=c(),resids=c())
  while(time <= upper.time){
    if (time < dat$time[i] || is.na(dat$time[i])) {
      t.buffered.obs$obs = c(t.buffered.obs$obs,NA)
      t.buffered.obs$resids = c(t.buffered.obs$resids,NA)
    }
    else if (time == dat$time[i]) {
      t.buffered.obs$obs=c(t.buffered.obs$obs, dat$obs[i])
      t.buffered.obs$resids=c(t.buffered.obs$resids, dat$resids[i])
      i = i + 1
    }
    time =time+1
  }
  t.buffered.obs$time = c(1:upper.time)
  res.df <- data.frame(t.buffered.obs)
  ##fill out constant columns
  res.df$ID = dat$ID[1]
  res.df$mod = dat$mod[1]
  res.df$highMod = dat$highMod[1]
  res.df$lowMod = dat$lowMod[1]
  res.df$Dist1 = dat$Dist1[1]
  res.df$Dist0 = dat$Dist0[1]
  return(res.df)
}


#' prepEqualize
#'
#' this function helps to take data from the original dataframe, and break it into parts that can be equalized with the equalizeTimes function.
#'
#'
#' @param dat
#'
#' @return dataframe where none of the timsteps inbetween the min and the max are missing from either partner. Basically means the lengths of the partners data matches.
#' @export
#'
#' @examples
prepEqualize <- function (dat) {
    ids <- unique(dat$ID)
    firstPerson <- dat %>%
      filter(ID ==ids[1])
    secondPerson <- dat %>%
      filter(ID == ids[2])
    maxtime  <- max(firstPerson$time,secondPerson$time)
    if(is.na(firstPerson$ID[1])){
      firstPerson.df <- data.frame() ## make empty dataframe
    } else {
    firstPerson.df <- equalizeTimes(firstPerson,maxtime)
    }
    if(is.na(secondPerson$ID[1])){
      secondPerson.df <- data.frame() ## make empty dataframe
    } else {
    secondPerson.df <- equalizeTimes(secondPerson,maxtime)
    }
    return(rbind(firstPerson.df,secondPerson.df))
}

#' makeEqual
#'
#' this function takes a full dataframe, processes in groups by dyad, equalizes each of the partners data based on min and max times, and returns a new dataframe for the whole experiment population which is balanced.
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
#' makeEqual(my.unbalanced.data.frame)
makeEqual <- function (dat) {
    dat %>%
        group_by(Dyad) %>%
        do(prepEqualize(.)) -> all.equalized.partners
    ## now we go on to combine the columns into a new df
    return(all.equalized.partners)
}
#' crossPartners
#' 
#' This function does the much anticipated actual flipping of the data. Effectively turns a long datastructure in a wide where a specific person's id
#' has their partner's data also within that row. Very helpful format for
#' performing fitting and other types of analysis.
#'
#' In the example below the argument is equalized and estimated data, because
#' that illustrates that this package uses this function following the 
#' derivative estimation step.
#'
#'
#'
#' @param data
#'
#' @return a new dataframe taht has a wide format with a person and partenrs data on the same row
#' @export
#'
#' @examples
#' crossPartners(equalized.estimated.data)
crossPartners <- function (data) {
  ##create copy flipped
  data %>%
    group_by(Dyad) %>%
    arrange(Dyad,desc(ID)) -> partner.data
  ##rename
  colnames(partner.data) <- paste(colnames(partner.data),"p",sep="_")
  ##mush back together
  res.df <- data.frame(data,partner.data)
  return (res.df)
}
