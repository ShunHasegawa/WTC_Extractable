## ----Stat_WTC_Extr_Phosphate

range(extr$po)
bxplts(value = "po", data = extr)
# use raw data

# The initial model is
Iml <- lmer(po ~ temp * time + (1|chamber) + (1|id), data = extr)
Anova(Iml)

# The final model is
Fml <- stepLmer(Iml)
Anova(Fml)
AnvF_P <- Anova(Fml, test.statistic = "F")
AnvF_P

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(resid(Fml))
qqline(resid(Fml))

# remove one outlier at the bottom
qqval <- qqnorm(resid(Fml))
Rm.Ol <- extr[-which(qqval$x == min(qqval$x)) ,]
bxplts(value = "po", data = Rm.Ol)
m1 <- lmer(po ~ temp * time + (1|chamber) + (1|id), data = Rm.Ol)
Anova(m1)
Anova(m1, test.statistic = "F")
plot(m1)
qqnorm(resid(m1))
qqline(resid(m1))
# improved slightly, so use this for the time being
Fml <- m1
AnvF_P <- Anova(Fml, test.statistic = "F")
AnvF_P

############
# contrast #
############
# note. contrast doesn't accept lmer so use lme
lmeMod <- lme(po ~ temp * time, random = ~1|chamber/location, data = Rm.Ol) 
Anova(lmeMod)

cntrst<- contrast(lmeMod,
                  a=list(time=levels(extr$time), temp = "amb"),
                  b=list(time=levels(extr$time),temp = "elev"))

WTC_ExtractableP_cntstDF <- cntrstTbl(cntrstRes = cntrst, data = extr)

## ----Stat_WTC_Extr_Phosphate_Smmry
# The initial model is:
Iml@call
Anova(Iml)

# The final model is:
Fml@call
Anova(Fml)
AnvF_P

WTC_ExtractableP_cntstDF