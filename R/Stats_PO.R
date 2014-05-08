range(extr$po)
bxplts(value = "po", data = extr)
# use box-cox lambda

# different random factor structure
m1 <- lme(po^2 ~ temp * time, random = ~1|chamber/side, data = extr)
m2 <- lme(po^2 ~ temp * time, random = ~1|chamber, data = extr)
m3 <- lme(po^2 ~ temp * time, random = ~1|id, data = extr)
anova(m1, m2, m3)
# m3 seems better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")$models
# no need for correlation

# The initial model is:
Iml <- atcr.cmpr(m3, rndmFac= "id")[[1]]
Anova(Iml)

# model simplification
MdlSmpl(Iml)
# no factor is removed

Fml <- MdlSmpl(Iml)$model.reml

# The final model is:
Fml$call

Anova(Fml)

summary(Fml)

# contrast and look at each month
cntrst<- contrast(Fml,
                  a=list(time=levels(extr$time), temp = "amb"),
                  b=list(time=levels(extr$time),temp = "elev"))

WTC_ExtractableP_cntstDF <- cntrstTbl(cntrstRes = cntrst, data = extr)

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
# not really great..... how about removing outlier



