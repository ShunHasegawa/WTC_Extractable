## ----Stat_WTC_Extr_Ammonium

bxplts(value = "nh", data = Extr_DF)

# log
Iml_nh <- lmer(nh^(1/3) ~ temp * time + (1|chamber), data = Extr_DF)
Anova(Iml_nh)

Fml_nh <- stepLmer(Iml_nh)
AnvF_nh <- Anova(Fml_nh, test.statistic = "F")
AnvF_nh

# model diagnosis
plot(Fml_nh)
qqnorm(resid(Fml_nh))
qqline(resid(Fml_nh))

# what if I remove the outlier
ol <- which(qqnorm(resid(Fml_nh))$y == min(qqnorm(resid(Fml_nh))$y))
m <- update(Iml_nh, subset = -ol)
plot(m)
qqnorm(resid(m))
qqline(resid(m))
Anova(m, test.statistic = "F")
m2 <- stepLmer(m)
Anova(m2, test.statistic = "F")
# same as the above so just stay with the first one


############################
# ANCOVA fit soil variable #
############################

# plot soil variables 
xyplot(nh ~ moist|temp, groups = chamber, type = c("r", "p"), data = Extr_DF)
xyplot(nh ~ moist|chamber, type = c("r", "p"), data = Extr_DF)
xyplot(nh ~ moist|temp, groups = time, type = c("r", "p"), data = Extr_DF)
xyplot(nh ~ moist|time, type = c("r", "p"), data = Extr_DF)

scatterplotMatrix(~ I(nh^(1/3)) + moist + Temp5_Mean, data = Extr_DF, diag = "boxplot", 
                  groups = Extr_DF$temp, by.group = TRUE)
scatterplotMatrix(~ log(nh) + log(moist) + Temp5_Mean, data = Extr_DF, diag = "boxplot", 
                  groups = Extr_DF$temp, by.group = TRUE)


m1 <- lmer(nh^(1/3) ~ temp * log(moist) + (1|time) + (1|chamber), 
                    data = Extr_DF)
Anova(m1) 
# Interaction is indicated, but moisture range is quite different. what if I use
# the samge range of moisture for both treatment
ddply(Extr_DF, .(temp), summarise, range(moist))
m2 <- update(m1, subset = moist < 0.14)
Anova(m2)
# no interaction so remove this.

Iml_ancv_nh <- lmer(nh^(1/3) ~ temp + log(moist) + (1|time) + (1|chamber), 
                    data = Extr_DF)
m2 <- update(Iml_ancv_nh, ~. - (1|time))
m3 <- update(Iml_ancv_nh, ~. - (1|chamber))
anova(Iml_ancv_nh, m2, m3)

Anova(Iml_ancv_nh)

Fml_ancv_nh <- Iml_ancv_nh
AnvF_ancv_nh <- Anova(Fml_ancv_nh, test.statistic = "F")
AnvF_ancv_nh
plot(Fml_ancv_nh)
qqnorm(resid(Fml_ancv_nh))
qqline(resid(Fml_ancv_nh))

# visualise
visreg(Fml_ancv_nh, xvar = "moist", by = "temp", overlay = TRUE)

## ----Stat_WTC_Extr_Ammonium_Smmry
# The initial model is:
Iml_nh@call
Anova(Iml_nh)

# The final model is:
Fml_nh@call

# Chi test
Anova(Fml_nh)

# F test
AnvF_nh

# ANCOVA
Iml_ancv_nh@call
Anova(Iml_ancv_nh)

Fml_ancv_nh@call

# Chi
Anova(Fml_ancv_nh)

# F test
AnvF_ancv_nh
