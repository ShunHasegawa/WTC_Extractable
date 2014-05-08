range(extr$po)
bxplts(value = "po", data = extr)
# no transformation seems better

# different random factor structure
m1 <- lme(po ~ temp * time, random = ~1|chamber/side, data = extr)
m2 <- lme(po ~ temp * time, random = ~1|chamber, data = extr)
m3 <- lme(po ~ temp * time, random = ~1|id, data = extr)
anova(m1, m2, m3)
# m3 seems better

# autocorrelation
atcr.cmpr(m3, rndmFac= "id")
# no need for correlation

Anova(m3)

# model simplification
MdlSmpl(m3)
# no factor is removed

Fml <- m3

# the final model is:
lme(po ~ temp * time, random = ~1|id, data = extr)

Anova(Fml)

summary(Fml)

plot(allEffects(Fml))

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


### remove outlier ###
bxplts(value = "po", data = extr)

P_RmOl <- subset(extr, po > min(po))

bxplts(value = "po", data = P_RmOl)

m1 <- lme(po ~ temp * time, random = ~1|chamber/side, data = P_RmOl)
# model diagnosis
plot(m1)
qqnorm(m1, ~ resid(.)|chamber)
qqnorm(residuals.lm(m1))
qqline(residuals.lm(m1))
# slightly improved, but wouldn hardly make a difference in the final resuts
Anova(m1)
