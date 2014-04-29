###########
# Nitrate #
###########
range(extr$no)

bxplts(value = "no", ofst = 0.1, data = extr)
 # log seemes better

# different random factor structure
m1 <- lme(log(no + .1) ~ temp * time, random = ~1|chamber/side, data = extr)
m2 <- lme(log(no + .1) ~ temp * time, random = ~1|chamber, data = extr)
m3 <- lme(log(no + .1) ~ temp * time, random = ~1|id, data = extr)
anova(m1, m2, m3)
  # m2 is slightly better

# autocorrelation
atcr.cmpr(m2, rndmFac= "chamber")
 # no need for auto correlation

# model simplification
MdlSmpl(m2)
  # interaction of temp x time is removed; temp is maginal but may be removable

# remove temp
msmpl <- MdlSmpl(m2)$model.ml # extract the simpifed model with the method "ML"
msmpl2 <- update(msmpl, ~. - temp)
anova(msmpl, msmpl2)
  # temp is removed, so now check if we can simplify the model even more
  MdlSmpl(msmpl2)
    # unable to remove any more

Fml <- MdlSmpl(msmpl2)$model.reml

# The final model is:
me(log(no + .1) ~ time, random = ~1|chamber, data = extr)

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))


