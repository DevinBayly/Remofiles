library(nlme)

data_dialCO <- read.csv("data_dialCO.csv")

full <- lme(d2dial ~ male:dial + female:dial + male:ddial + female:ddial + male:coupling + female:coupling- 1,
            random=list(~ male:dial + female:dial + male:ddial + female:ddial + male:coupling + female:coupling - 1 | Dyad), data=data_dialCO, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
summary(full)


reduced <- lme(d2dial ~ male:dial + female:dial + male:ddial + female:ddial + male:coupling + female:coupling- 1,
               random=list(~ dial + ddial + coupling  - 1 | Dyad), data=data_dialCO, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
summary(reduced)

anova(full, reduced)



##

reduced <- lme(d2_resids_1_3 ~ male:resids + female:resids + male:ddial + female:ddial + male:pdial + female:pdial + male:pddial + female:pddial - 1,
               random=list(~ dial + ddial + pdial + pddial  - 1 | Dyad), data=crossed_data, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))
summary(reduced)


reducedMod <- lme(d2dial ~ male:dial + female:dial + male:ddial + female:ddial + male:pdial + female:pdial + male:pddial + female:pddial + male:partsat1 + female:partsat1 + male:dial:partsat1 + female:dial:partsat1 + male:ddial:partsat1 + female:ddial:partsat1 + male:pdial:partsat1 + female:pdial:partsat1 + male:pddial:partsat1 + female:pddial:partsat1 - 1,  random=list(~ dial + ddial + pdial + pddial  - 1 | Dyad), data=data_dialCO, method="ML",na.action=na.omit,control=lmeControl(opt="optim"))

summary(reducedMod)

anova(reduced, reducedMod)
