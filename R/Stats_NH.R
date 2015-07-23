## ----Stat_WTC_Extr_Ammonium

bxplts(value = "nh", data = Extr_DF)

# log
m1 <- lmer(nh^(1/3) ~ temp * time + (1|chamber), data = Extr_DF)
Anova(m1)
# no interaction so remove time1

Iml_nh <- update(m1, data = Extr_DF2)
Anova(Iml_nh)

Fml_nh <- update(Iml_nh, ~. - temp:time)
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
xyplot(nh ~ moist|temp, groups = chamber, type = c("r", "p"), data = Extr_DF2)
xyplot(nh ~ moist|chamber, type = c("r", "p"), data = Extr_DF2)
xyplot(nh ~ moist|temp, groups = time, type = c("r", "p"), data = Extr_DF2)
xyplot(nh ~ moist|time, type = c("r", "p"), data = Extr_DF2)

scatterplotMatrix(~ I(nh^(1/3)) + log(moist) + Temp5_Mean, data = Extr_DF2, diag = "boxplot", 
                  groups = Extr_DF2$temp, by.group = TRUE)

Iml_ancv_nh <- lmer(nh^(1/3) ~ temp * (moist + Temp5_Mean) + (1|chamber), data = Extr_DF2)
Fml_ancv_nh <- stepLmer(m1, alpha.fixed = .1)
Anova(Fml_ancv_nh, test.statistic = "F")

AnvF_ancv_nh <- Anova(Fml_ancv_nh, test.statistic = "F")
AnvF_ancv_nh
plot(Fml_ancv_nh)
qqnorm(resid(Fml_ancv_nh))
qqline(resid(Fml_ancv_nh))

# visualise
TransVirsreg(visreg(Fml_ancv_nh, xvar = "Temp5_Mean", by = "temp"), 
             trans = function(x) x^3, overlay = TRUE, 
             line = list(col = c(1, 2)))

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

par(mfrow = c(1, 1))
TransVirsreg(visreg(Fml_ancv_nh, xvar = "Temp5_Mean", by = "temp"), 
             ddf = Extr_DF2, trans = function(x) x^3, overlay = TRUE, 
             line = list(col = c(1, 2)))
