

## Functions for creating actor-partner data format - Takes individual data (Version 1 is cross-sectional and Version 2 is repeated measures) from dyads and turns it into actor-partner format

# Need to use a person ID that has first person in dyad numbered 1-n and second person in dyad = ID + 500
# Need dyad ID numbered same as the first person in the dyad
# Both members in each dyad need to have the same number of rows (rows of missing data are ok)
# Need to enter appropriate values into the function call - see example call below
#  data = name of an R data frame containing original data, n = highest value of dyad ID, by = name of the variable indicating peron ID, and for the repeated measures version time = name of the variable indicating temporal order
# function will create a data file called "dataAPcross" or "dataAPtime"

# Version 1 : example call for the ActorPartnerDataCross function
# ActorPartnerDataCross(data, n, by)

# ActorPartnerDataCross(data, 46, data$Person)

ActorPartnerDataCross <- function(data, n, by){
library(plyr)
data.A <- data
data.A$by <- by
data.P1 <- subset(data.A, by <=n)
data.P2 <- subset(data.A, by > n)
data.P1$by <- data.P1$by + 500
data.P2$by <- data.P2$by - 500
data.P <- merge(data.P1, data.P2, all=TRUE)
colnames(data.P) <- paste("p", colnames(data.P), sep="_")
data.P <- rename(data.P, c(p_by="by"))
dataAPcross <- merge(data.A, data.P, by=c("by"), all=TRUE)
dataAPcross <<- subset(dataAPcross, select = -c (by))
}

## tests that AP variable summaries are the same, but the correlation is not= 1
summary(dataAPcross$t1ambiv4)
summary(dataAPcross$p_t1ambiv4)
cor(dataAPcross$t1ambiv4, data.APcross$p_t1ambiv4, use="pairwise.complete.obs")

# should also do a visual check
fix(dataAPcross)


# Version 2: example call for the ActorPartnerDataTime function
# ActorPartnerDataTime(data, 110, data$Person, data$Segment)

ActorPartnerDataTime <- function(data, n, by, time){
library(plyr)
data.A <- data
data.A$by <- by
data.A$time <- time
data.P1 <- subset(data.A, by <=n)
data.P2 <- subset(data.A, by > n)
data.P1$by <- data.P1$by + 500
data.P2$by <- data.P2$by - 500
data.P <- merge(data.P1, data.P2, all=TRUE)
colnames(data.P) <- paste("p", colnames(data.P), sep="_")
data.P <- rename(data.P, c(p_by="by", p_time="time"))
dataAPtime <- join(data.A, data.P)
dataAPtime <<- subset(dataAPtime, select = -c (by))
}

## tests that AP variable summaries are the same, but the correlation is not= 1
summary(dataAPtime$posptouch)
summary(dataAPtime$p_posptouch)
cor(dataAPtime$posptouch, dataAPtime$p_posptouch, use="pairwise.complete.obs")

# should also do a visual check
fix(dataAPtime)


