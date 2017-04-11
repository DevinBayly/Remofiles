## the purpose of this file is to give the user a single edit point that the
##rest of the files take advantage of for declaring things like
##data,id,moderator.ect

##data <- "dataname.csv"
##observation <- "name_of_observed_variable"
##moderator <- "name_of_moderator_variable"
##Dyad <- "name_of_distinguishing_variable"
##time  <- "the measure val for the obs
##ID <- "name_of_id_variable"

data = read.csv("conv2secMods05.csv")
## we expect that dplyr is loaded before running this
data %>%
    mutate(ID = Person,
           Dyad = Couple,
           time = time,
           observation = IBI, ## this and the moderator are subject to change
           moderator = sexsat)

