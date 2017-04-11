# this file serves to create a new dataframe that has all the exitsing material but adds in a column that holds the residuals after each ID's experimental factor (ibi,dial,etc) is centralized.


## question, currentnly this example has been following the process of using IBI and time. IBI seems like an observable, and just a standard measure, how would we tie the residuals in?

getrsdls <- function (dat) {
    # rename observable to be the observable for clarification
    # example being getrsdls ("IBI","time",dat)

    f  <- paste("obs","~","time")# makes it possible to havestring formula
    fit  <- lm(f,data=dat)
    return(summary(fit)$residuals)
}

addlevelMods  <- function (dat){
    ## im still leaning towards using a within ID sort of thing here
    modLevels = quantile(dat$mod,c(.25,.75))
    outdat  <- dat %>%
        mutate(highModResid = mod-modLevels[2] ,
               lowModResid = mod -modLevels[1])
    return(outdat)

}

makeResidCol <- function (dat) {
    ## imporntant to remember that by this point we shouldhave a dat that has a specific column called ID
    ## question, in theory we don't need to have an observable param right?
    ## should just be that we use the column named obs?
    returnVect= c()
    pids = unique(dat$ID)# making it into a vector appears to be very important
    # or not...
    # the error seems to still be here
    for (pid in pids) {# need some third way to generically let the user specify the colname with the ids
        dat %>%
            filter(ID==pid) -> pcdat
        tempres = getrsdls(pcdat)
        returnVect = c(returnVect,tempres)
    }
    ## maybe this function should be the sort of main, where all the
  #different methods get called. that way we don't have to chain so much
    dat[["resids"]] =returnVect
    outdat = addlevelMods(dat)
    return(outdat)
}

