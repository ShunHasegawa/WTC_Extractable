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
xyplot(po ~ moist|temp, groups = chamber, type = c("r", "p"), data = Extr_DF2)
xyplot(po ~ moist|chamber, type = c("r", "p"), data = Extr_DF2)
xyplot(po ~ moist|temp, groups = time, type = c("r", "p"), data = Extr_DF2)
xyplot(po ~ moist|time, type = c("r", "p"), data = Extr_DF2)

scatterplotMatrix(~ po + moist + Temp5_Mean, data = Extr_DF2,
                  diag = "boxplot", groups = Extr_DF2$temp, by.group = TRUE)

scatterplotMatrix(~ log(po) + log(moist) + Temp5_Mean, data = Extr_DF2,
                  diag = "boxplot", groups = Extr_DF2$temp, by.group = TRUE)

Iml_ancv_po <- lmer(po ~ temp * (moist + Temp5_Mean) + (1|chamber), data = Extr_DF2)
Anova(Iml_ancv_po)

Fml_ancv_po <- stepLmer(Iml_ancv_po, alpha.fixed = .1)

plot(Fml_ancv_po)
qqnorm(resid(Fml_ancv_po))
qqline(resid(Fml_ancv_po))

ol <- which(qqnorm(resid(Fml_ancv_po))$y == min(qqnorm(resid(Fml_ancv_po))$y))
mm <- update(Iml_ancv_po, subset = -ol)
plot(mm)
qqnorm(resid(mm))
qqline(resid(mm))

Iml_ancv_po <- mm
Fml_ancv_po <- stepLmer(Iml_ancv_po, alpha.fixed = .1)
AnvF_ancv_po <- Anova(Fml_ancv_po, test.statistic = "F")
AnvF_ancv_po

# visualise
# visreg can't be used for the above model as one value is removed
newpodf <- Extr_DF2[-ol, ]
ml_po <- lmer(newpo ~ moist + Temp5_Mean + (1|chamber), data = newpodf)
par(mfrow = c(1, 2))
TransVirsreg(visreg(ml_po, xvar = "moist", plot = FALSE), 
             trans = I, point = list(col = newpodf$temp, cex = 1))
TransVirsreg(visreg(ml_po, xvar = "Temp5_Mean", plot = FALSE), 
             trans = I, point = list(col = newpodf$temp, cex = 1))

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

par(mfrow = c(1, 2))
TransVirsreg(visreg(ml_po, xvar = "moist", plot = FALSE), 
             trans = I, point = list(col = newpodf$temp, cex = 1))
TransVirsreg(visreg(ml_po, xvar = "Temp5_Mean", plot = FALSE), 
             trans = I, point = list(col = newpodf$temp, cex = 1))
