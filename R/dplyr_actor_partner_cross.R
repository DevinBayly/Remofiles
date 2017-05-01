#### this is the basic what should be working set of tools. Problem being that the data isn't reliably structured...

### make these more like eventual dataframes
equalizeTimes <- function (dat,upperTimeBound) {
    tind=1
    while (tind != upperTimeBound) {
        t1 = dat$time[tind]
        if(is.na(t1)) {## at this point the df doesn't have numbers given, so we just supply supplemental rows until the end
            dat[seq(tind+1,nrow(dat)+1),]  <- dat[seq(tind,nrow(dat)),]
 ## this is the NA row creation step
            dat[tind,]= c(dat$ID[tind],# id
                          dat$Dyad[tind],#dyad
                          NA,#obs
                          tind, #time
                          NA,#mod
                          dat$Dist1[tind],#dist
                          rep(NA,15),
                          dat$Dist2[tind])# following columns derivs and freqs based on obs
        } else if (t1 > tind) {## t1 got ahead, shouldn't have time repeats, but I guess that can be taken into account
            dat[seq(tind+1,nrow(dat)+1),]  <- dat[seq(tind,nrow(dat)),]
            ## this means that the rows must be in this order.
            dat[tind,]= c(dat$ID[tind],# id
                          dat$Dyad[tind],#dyad
                          NA,#obs
                          tind, #time
                          NA,#mod
                          dat$Dist1[tind],#dist
                          rep(NA,15),
                          dat$Dist2[tind])# following columns derivs and freqs based on obs
        }
        tind = tind +1
    }
    return(dat)
}


prepEqualize <- function (dat) {
    ids  <- unique(dat$ID)
    firstPerson  <- dat %>%
        filter(ID == ids[1])
    secondPerson  <- dat %>%
        filter(ID == ids[2])
    maxtime  <- max(firstPerson$time,secondPerson$time)
    firstPersonupd  <- equalizeTimes(firstPerson,maxtime)
    secondPersonupd  <- equalizeTimes(secondPerson,maxtime)
    return(rbind(firstPersonupd,secondPersonupd))
}

crossActorPartner <- function (dat) {
    dat %>%
        group_by(Dyad) %>%
        do(prepEqualize(.)) -> equalizeddat## this is actually the step in which we are making the columns equal

    ## now we go on to combine the columns into a new df

    equalizeddat %>%
        group_by(Dyad) %>%
        arrange(Dyad,ID) -> selfdat ## basically organizes first by Dyad, and then ascending person (this is assuming that the data has partners organized with
    selfdf  <- as.data.frame(selfdat)
    equalizeddat %>%
        group_by(Dyad) %>%
        arrange(Dyad,desc(ID)) -> partnerdat
    partnerdf  <- as.data.frame(partnerdat)
    ## changes column names of partnerdf to have the p next to them
    colnames(partnerdf)  <- paste(colnames(partnerdf),"p",sep="_")

    crossdat  <- data.frame(selfdf,partnerdf)## seems pretty fast, I think I'll keep it
    return(crossdat)
}
### this is the dataframe column selections
