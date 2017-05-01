#is this model based around anything that should have already been run?
#####
### Exploring CO model and weight

# formula for estimating frequency from the displacement parameter estimate
freq <- (2*pi)/sqrt((-1)*fit1$coefficients$fixed[[1]])


#### observation

library(nlme)

# empty model - only difference for different frequencies is the average frequency
fit1 <- lme(d2_observation ~ observation + d_observation + coupling - 1,
			random=list(~ observation + d_observation + coupling - 1 | Dyad), data=data_obsCO7, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
summary(fit1)

# separating parameters by sex - simplifying random matrix improves fit, is faster, and converges more reliably therefore use fit4 version
# Note: this is essentially the "two-intercept model", except there are no intercepts
fit2 <- lme(d2_observation ~ sexf:observation + sexf:d_observation + sexf:coupling - 1,
			random=list(~ sexf:observation + sexf:d_observation + sexf:coupling - 1 | Dyad), data=data_obsCO25, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
summary(fit2)
fit3 <- update(fit2, random=list(Dyad=pdDiag(~sexf:observation + sexf:d_observation + sexf:coupling - 1)))
anova(fit2, fit3)

# results show different parameters-by-sex for different frequencies (need to triple check which estimates are the male and which are female - I think the 0s are the women if using sexf)
fit4 <- lme(d2_observation ~ sexf:observation + sexf:d_observation + sexf:coupling - 1, random=list(Dyad=pdDiag(~sexf:observation + sexf:d_observation + sexf:coupling - 1)), data=data_obsCO7, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
summary(fit4)

# this is the start of the moderator experiments
# get most interesting results with CO25
fit5 <- lme(d2_observation ~ sexf:observation + sexf:d_observation + sexf:coupling
					 + sexf:bmiave:observation + sexf:bmiave:d_observation + sexf:bmiave:coupling
					 + sexf:bmidif:observation + sexf:bmidif:d_observation + sexf:bmidif:coupling - 1,
			random=list(Dyad=pdDiag(~sexf:observation + sexf:d_observation + sexf:coupling - 1)), data=data_obsCO25, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
summary(fit5)

# get most interesting results for CO25 different moderator
fit6 <- lme(d2_observation ~ sexf:observation + sexf:d_observation + sexf:coupling
					 + sexf:ratepave:observation + sexf:ratepave:d_observation + sexf:ratepave:coupling
					 + sexf:ratepdif:observation + sexf:ratepdif:d_observation + sexf:ratepdif:coupling - 1,
			random=list(Dyad=pdDiag(~sexf:observation + sexf:d_observation + sexf:coupling - 1)), data=data_obsCO25, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
summary(fit6)




