## ----Stat_WTC_Extr_Nitrate

range(extr$no)

bxplts(value = "no", ofst = 0.1, data = extr)
bxcxplts(value = "no", data = extr, sval = 0.001, fval = .8)

# log seemes better

# different random factor structure
m1 <- lme(log(no + .1) ~ temp * time, random = ~1|chamber/side, data = extr)
m2 <- lme(log(no + .1) ~ temp * time, random = ~1|chamber, data = extr)
m3 <- lme(log(no + .1) ~ temp * time, random = ~1|id, data = extr)
anova(m1, m2, m3)
# m2 is slightly better

# autocorrelation
atcr.cmpr(m2, rndmFac= "chamber")$models
# no need for auto correlation

# The initial model is:
Iml <- atcr.cmpr(m2, rndmFac= "chamber")[[1]]
Anova(Iml)

# model simplification
MdlSmpl(Iml)
# interaction of temp x time is removed; temp is maginal but may be removable

Fml <- MdlSmpl(Iml)$model.reml

# The final model is:
Fml$call

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))

## ----Stat_WTC_Extr_Nitrate_Smmry
# The initial model is:
Iml$call
Anova(Iml)

# The final model is:
Fml$call
Anova(Fml)