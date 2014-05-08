range(extr$nh)
bxplts(value = "nh", ofst= 0.6, data = extr)
# log seems better

# different random factor structure
m1 <- lme(log(nh + .6) ~ temp * time, random = ~1|chamber/side, data = extr)
m2 <- lme(log(nh + .6) ~ temp * time, random = ~1|chamber, data = extr)
m3 <- lme(log(nh + .6) ~ temp * time, random = ~1|id, data = extr)
anova(m1, m2, m3)
# m3 is slightly better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")
# no need for correlation

Anova(m3)

# model simplification
MdlSmpl(m3)
# interaction of tmep x time is remove

Fml <- MdlSmpl(m3)$model.reml

# the final model is:
lme(log(nh + .6) ~ temp + time, random = ~1|id, data = extr)

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
# not very great, but not too terrible...(?), then how about power(1/3)?

### power(1/3) transformation ###
bxplts(value = "nh", ofst= 0.29, data = extr)
m1 <- lme((nh + .29)^(1/3) ~ temp * time, random = ~1|chamber/side, data = extr)
plot(m1)
qqnorm(m1, ~ resid(.)|chamber)
qqnorm(residuals.lm(m1))
qqline(residuals.lm(m1))
# not really different than log transformation so stay with log
