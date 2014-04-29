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

Anova(m2)

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

############
# Ammonium #
############
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

#############
# Phosphate #
#############
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
