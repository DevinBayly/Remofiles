#### this is the basic what should be working set of tools. Problem being that the data isn't reliably structured...

### make these more like eventual dataframes
#' Title
#'
#' @param dat
#' @param upperTimeBound
#' @param fill_cols
#'
#' @return
#' @export
#'
#' @examples
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


#' Title
#'
#' @param dat
#' @param fill_cols
#'
#' @return
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

#' Title
#'
#' @param dat
#' @param fill_cols
#'
#' @return
#' @export
#'
#' @examples
crossActorPartner <- function (dat) {
    dat %>%
        group_by(Dyad) %>%
        do(prepEqualize(.)) -> all.equalized.partners
    ## now we go on to combine the columns into a new df
    return(all.equalized.partners)
}
### this is the dataframe column selections
#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
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
