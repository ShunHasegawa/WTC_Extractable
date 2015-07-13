## ----Stat_WTC_Extr_Phosphate

bxplts(value = "po", data = Extr_DF)

# use raw data

# The initial model is
Iml_po <- lmer(po ~ temp * time + (1|chamber), data = Extr_DF)
Anova(Iml_po)

# The final model is
Fml_po <- stepLmer(Iml_po)
Anova(Fml_po)
AnvF_po <- Anova(Fml_po, test.statistic = "F")
AnvF_po

# model diagnosis
plot(Fml_po)
qqnorm(resid(Fml_po))
qqline(resid(Fml_po))

############
# contrast #
############
# note. contrast doesn't accept lmer so use lme
lmeMod <- lme(po ~ temp * time, random = ~1|chamber, data = Extr_DF) 

cntrst<- contrast(lmeMod,
                  a=list(time=levels(Extr_DF$time), temp = "amb"),
                  b=list(time=levels(Extr_DF$time),temp = "elev"))

WTC_ExtractableP_cntstDF <- cntrstTbl(cntrstRes = cntrst, data = extr, variable = "po")

############################
# ANCOVA fit soil variable #
############################

# plot soil variables 
xyplot(po ~ moist|temp, groups = chamber, type = c("r", "p"), data = Extr_DF)
xyplot(po ~ moist|chamber, type = c("r", "p"), data = Extr_DF)
xyplot(po ~ moist|temp, groups = time, type = c("r", "p"), data = Extr_DF)
xyplot(po ~ moist|time, type = c("r", "p"), data = Extr_DF)

scatterplotMatrix(~ po + moist + Temp5_Mean, data = Extr_DF,
                  diag = "boxplot", groups = Extr_DF$temp, by.group = TRUE)

scatterplotMatrix(~ log(po) + log(moist) + Temp5_Mean, data = Extr_DF,
                  diag = "boxplot", groups = Extr_DF$temp, by.group = TRUE)

m1 <- lmer(po ~ temp * moist + (1|time) + (1|chamber), data = Extr_DF)
Anova(m1)
# Interaction is indicated, but moisture range is quite different. what if I use
# the samge range of moisture for both treatment
ddply(Extr_DF, .(temp), summarise, range(moist))
m2 <- update(m1, subset = moist < 0.14)
Anova(m2)
# no interaction so remove

Iml_ancv_po <- lmer(po ~ temp + moist + (1|time) + (1|chamber), data = Extr_DF)
Anova(Iml_ancv_po)

Fml_ancv_po <- Iml_ancv_po

AnvF_ancv_po <- Anova(Iml_ancv_po, test.statistic = "F")
AnvF_ancv_po

plot(Fml_ancv_po)
qqnorm(resid(Fml_ancv_po))
qqline(resid(Fml_ancv_po))

# visualise
visreg(Fml_ancv_po, xvar = "moist", by = "temp", overlay = TRUE)

## ----Stat_WTC_Extr_Phosphate_Smmry
# The initial model is:
Iml_po@call
Anova(Iml_po)

# The final model is:
Fml_po@call
Anova(Fml_po)
AnvF_po

WTC_ExtractableP_cntstDF

# ANCOVA
Iml_ancv_po@call
Anova(Iml_ancv_po)

Fml_ancv_po@call
AnvF_ancv_po
