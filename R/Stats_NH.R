range(extr$nh)
bxplts(value = "nh", ofst= 0.6, data = extr)
bxcxplts(value = "nh", data = extr, sval = 0.3, fval = 6)
# adding constant value of 3 may improve

bxplts(value = "nh", ofst= 3, data = extr)
# use box-cox lambda

# different random factor structure
m1 <- lme((nh + 3)^(-1.1515) ~ temp * time, random = ~1|chamber/side, data = extr)
m2 <- lme((nh + 3)^(-1.1515) ~ temp * time, random = ~1|chamber, data = extr)
m3 <- lme((nh + 3)^(-1.1515) ~ temp * time, random = ~1|id, data = extr)
anova(m1, m2, m3)
# m3 is slightly better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")$models
# no need for correlation

# The initial model is:
Iml <- atcr.cmpr(m3, rndmFac= "id")[[1]]

Anova(Iml)

# model simplification
MdlSmpl(Iml)
# interaction of tmep x time is remove
Fml <- MdlSmpl(Iml)$model.reml

# The final model is:
Fml$call

Anova(Fml)

summary(Fml)

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
# not very great, but not too terrible...(?)

### power(1/3) transformation ###
bxplts(value = "nh", ofst= 0.29, data = extr)
m1 <- lme((nh + .29)^(1/3) ~ temp * time, random = ~1|chamber/side, data = extr)
plot(m1)
qqnorm(m1, ~ resid(.)|chamber)
qqnorm(residuals.lm(m1))
qqline(residuals.lm(m1))
# not really different than log transformation so stay with log
