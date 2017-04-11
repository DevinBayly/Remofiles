library(nlme)
library(psych)


#### First, a few leftovers from last week.

####### Creating composite scale scores

data <- read.csv("survey.csv")
scale1 <- subset(data, select=c(strat1:strat6))
alpha(scale1)
data$scale1 <- rowMeans(scale1, na.rm=T)
write.csv(data, "surveyUpdate.csv", row.names=F)

#### Unpacking functions and using indexing into objects

explore <- read.csv("explore.csv")
explore <- explore[complete.cases(explore), ]

# stem plot for the intercepts based on the OLS estimates
int <- by(explore, explore$PERSON, function(x) coefficients(lm(systolic ~ SEGMENT, data=x))[[1]])
int
## how did that work? Let's unpack it. 
lm(systolic ~ SEGMENT, data=explore) # The fancy part was all to do one person at a time. To unpack it we can just use all the data at once. Since it prints the intercept, it obviously knows what it is
coefficients(lm(systolic ~ SEGMENT, data=explore))
coefficients(lm(systolic ~ SEGMENT, data=explore))[[1]]
int # it has pulled the estimated intercept for each person's regression model
summary(int)
stem(int, scale=2)

# r-square for OLS models
rsq <- by(explore, explore$PERSON, function(x) summary(lm(systolic ~ SEGMENT, data=x))$r.squared)
rsq # this is a similar trick to the indexing above, but a bit different - let's see what it did
s <- summary(lm(systolic ~ SEGMENT, data=explore))
str(s)
s$coefficients
s$coefficients[1]# this is basically what we did before, just done a little differently
s$coefficients[2,2] 
s$r.squared
summary(rsq)
stem(rsq, scale=2)

#######################################################

### example of decomposing an interaction with one continuous and one categorical variable
# in the example, the variables are ethnicity (ethnic: 1=not Asian American, 2=Asian American) and negative emotion expression (negab) predicting blood pressure changes (mp) from a resting baseline to a conversation.

data <- read.csv("interaction.csv")
str(data)

# need to make the categorical variable into a factor and create a second version with the opposite reference level (we will need it soon)
data$ethnicNA <- factor(data$ethnic, levels=c(1,2), labels=c("NotAsian", "Asian"))
data$ethnicA <- relevel(data$ethnicNA, ref="Asian")

# first need to test whether there is a significant interaction
# Note: This is a dyadic model, so we have a random intercept, but don't worry about that for now

fit1 <- lme(mp ~ negab*ethnicA, random = ~ 1 | dyad, data=data, na.action=na.omit)
summary(fit1)
anova(fit1)

# having found a significant interaction we will want to graph it. The first thing we need is to create high and low versions of the continuous variable (negab). To do this we create new variables centered around high (75th percentile) and low (25th percentile) values of negab. 
# Note that any meaningful values can be used as the centering point - for example, plus and minus 1 SD would also make sense.

# we first use the summary function to find the 75th and 25th values for negab
summary(data$negab)
hist(data$negab)

# this shows that 25% = 13 and 75% = 23.75 

# next we create the new centered variables, "negabL" for low and "negabH" for high 
# new variables = the original variable minus the desired centering value (13 or 23.75 in this case)
# if there was more than one continuous variable involved then you would do this for each of them
# Note: a very common mistake is that if the desired centering value is negative then the new variable becomes the original variable PLUS the centering value. For example, if the 25th percentile was -13, then we would have newvariable = original variable - (-13) = original variable + 13

data$negabL  <-  data$negab - 13
data$negabH <- data$negab - 24
fix(data) # to check that nothing went wrong

# we next run the model 4 times using appropriate combinations of our new variables, once for each of the 4 estimation points we need for a graph. These points are: 1) Asian, high negab, 2) Asian, low negab, 3) not-Asian high negab, and 4) not-Asian low negab
# the intercept for each run will be the estimated value for that combination of ethncity and negab
# the parameter estimate for negab(L or H) in each run will be the estimate of the slope of mp on negab for the reference category of ethnic
# the parameter estimate for the ethnicity variable will be the effect of ethnicity at that level of negab (L or H)
# if you did it right, the estimates for the interaction term should not change from one run to the next

# first run gives intercept for Asians at low negab, the slope for negab in the Asian group, and the effect of ethnicity at low negab
fitAL <- lme(mp ~ negabL*ethnicA, random = ~ 1 | dyad, data=data, na.action=na.omit)
summary(fitAL) ## We don't need to look at this, but it can be a good reality check
IAL <- summary(fitAL)$tTable[1, 1] # stores the intercept estimate for this fit so we can use it for a  graph
SA <- summary(fitAL)$tTable[2, 1] # stores the slope for negab in the Asian group
SAp <- round(summary(fitAL)$tTable[2, 5], digits=3) # stores p value for the slope of negab in Asian group

# second run gives intercept for Asians at high negab, the slope for negab in the Asian group, and the effect of ethnicity at high negab
fitAH <- lme(mp ~ negabH*ethnicA, random = ~ 1 | dyad, data=data, na.action=na.omit)
summary(fitAH)
IAH <- summary(fitAH)$tTable[1, 1]

# third run gives intercept for non-Asians at low negab, the slope for negab in the non-Asian group, and the effect of ethnicity at low negab
fitNAL <- lme(mp ~ negabL*ethnicNA, random = ~ 1 | dyad, data=data, na.action=na.omit)
summary(fitNAL)
INAL <- summary(fitNAL)$tTable[1, 1] # stores the intercept estimate for this fit so we can use it for a  graph
SNA <- summary(fitNAL)$tTable[2, 1] # stores the slope for negab in the non-Asian group
SNAp <- round(summary(fitNAL)$tTable[2, 5], digits=3) # stores p value for the slope of negab in non-Asian group

# fourth run gives intercept for non-Asians at high negab, the slope for negab in the non-Asian group, and the effect of ethnicity at high negab
fitNAH <- lme(mp ~ negabH*ethnicNA, random = ~ 1 | dyad, data=data, na.action=na.omit)
summary(fitNAH)
INAH <- summary(fitNAH)$tTable[1, 1] 

# once you have the estimates you can graph them. First create objects with the values to graph
Asian <- c(IAL, IAH) # these are the intercept estimates for Asians at Low and High negab
NonAsian <- c(INAL, INAH) # these are the intercept estimates for Non-Asians at Low and High negab
NegAb <- c(13, 24) # these are the centering values for negab to be used on the X-axis

# Create the legend information you want 
legend1 <- sprintf('%s b=%6.2f p=%6.2f', "Asian", SA , SAp)
legend2 <- sprintf('%s b=%6.2f p=%6.2f', "Not Asian", SNA, SNAp)

# in the next statement you put in the first vector (Asian) to be graphed against NegAb and set the xlim and ylim to values that will give a good picture
plot(Asian~ NegAb, type="o", col="blue", xlim=c(10, 30), ylim=c(10, 24), axes=FALSE, ann=FALSE)
axis(1, at=NegAb, lab=c("Low=13", "High=24")) # put in the X labels you want
axis(2)
lines(NonAsian~NegAb, type="o", pch=22, lty=2, col="red") # put in the second vector (NonAsian) to be graphed against NegAb
title(main="Changes in Blood Pressure by Ethnicity") # put in the title you want
title(xlab="Negative Expression") # put in the x-label you want
title(ylab="Change in Blood Pressure") # put in the y-label you want
legend(13, 14, c(legend1, legend2), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2) # put in the legend


####################################

## Regions of significance, with thanks to Kristopher Preacher; http://www.quantpsy.org/interact/

## He has tools for interactions in multilevel models. We are going to apply the KISS principle and just treat our example as a multiple regression; This means the results will be a bit different than above, where we appropriately modeld the nesting in dyads.


## the following produces all the results we need to use the online tool
fit1 <- lm(mp ~ negab*ethnicA, data=data, na.action=na.omit)
summary(fit1)
vcov(fit1)
summary(data$negab)

