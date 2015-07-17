## ----Stat_WTC_Extr_Nitrate

range(Extr_DF$no)

bxplts(value = "no", data = Extr_DF)

# power(1/3)
m1 <- lmer(no^(1/3) ~ temp * time + (1|chamber), data = Extr_DF)
Anova(m1)

# no interaction. so remove time1 as it was before plants being installed
Iml_no <- update(m1, data = Extr_DF2)
Anova(Iml_no)

Fml_no <- update(Iml_no, ~. -temp:time)
AnvF_no <- Anova(Fml_no, test.statistic = "F")
AnvF_no

# model diagnosis
plot(Fml_no)
qqnorm(resid(Fml_no))
qqline(resid(Fml_no))

############################
# ANCOVA fit soil variable #
############################
# plot soil variables 
xyplot(no ~ moist|temp, groups = chamber, type = c("r", "p"), data = Extr_DF2)
xyplot(no ~ moist|chamber, type = c("r", "p"), data = Extr_DF2)
xyplot(no ~ moist|temp, groups = time, type = c("r", "p"), data = Extr_DF2)
xyplot(no ~ moist|time, type = c("r", "p"), data = Extr_DF2)

scatterplotMatrix(~ I(no^(1/3)) + moist + Temp5_Mean, data = Extr_DF2, diag = "boxplot", 
                  groups = Extr_DF2$temp, by.group = TRUE)
scatterplotMatrix(~ log(no) + log(moist) + Temp5_Mean, data = Extr_DF2, diag = "boxplot", 
                  groups = Extr_DF2$temp, by.group = TRUE)
scatterplotMatrix(~ no + log(moist) + Temp5_Mean, data = Extr_DF2, diag = "boxplot", 
                  groups = Extr_DF2$temp, by.group = TRUE)

m1 <- lmer(no^(1/3) ~ temp * (logmoist + Temp5_Mean)  + (1|chamber),  data = Extr_DF2)
m2 <- lmer(no^(1/3) ~ temp * (moist + Temp5_Mean) +  (1|chamber),  data = Extr_DF2)
ldply(list(m1,m2), r.squared)

m3 <- stepLmer(m2, alpha.fixed = .1)
Anova(m3, test.statistic = "F")

Iml_ancv_no <- lmer(no^(1/3) ~ temp * (moist + Temp5_Mean)  + (1|chamber),  data = Extr_DF2)
Fml_ancv_no <- lmer(no^(1/3) ~ moist + Temp5_Mean  + (1|chamber),  data = Extr_DF2)
AnvF_ancv_no <- Anova(Fml_ancv_no, test.statistic = "F")
AnvF_ancv_no

# model diagnosis
plot(Fml_ancv_no)
qqnorm(resid(Fml_ancv_no))
qqline(resid(Fml_ancv_no))

# visualise
par(mfrow = c(1, 2))
visreg(Fml_ancv_no, xvar = "moist", point = list(col = Extr_DF$temp))
visreg(Fml_ancv_no, xvar = "Temp5_Mean", point = list(col = Extr_DF$temp))

# Note: poinst plotted on visreg is not raw data but corrected value. when 
# plotted against Temp, median of moisture is used to get predicted values. Raw 
# data is corrected for this median moisture. Those values can be obtained
# manuary as follows
par(mfrow = c(1, 3))
# adjusted values by visreg
a <- visreg(Fml_ancv_no, xvar = "Temp5_Mean", point = list(col = Extr_DF2$temp))
# raw data
plot(no^(1/3) ~ Temp5_Mean, col = temp, data = Extr_DF2, pch = 19)
lines(visregFit ~ Temp5_Mean, data = a$fit)
# manual correction
summary(Fml_ancv_no)
Extr_DF2$CorrectNO <- (median(Extr_DF2$moist) - Extr_DF2$moist) * (-13.38498) + (Extr_DF2$no^(1/3))
plot(CorrectNO ~ Temp5_Mean, data = Extr_DF2, col = temp, pch = 19)

## ----Stat_WTC_Extr_Nitrate_Smmry
# The initial model is:
Iml_no@call
Anova(Iml_no)

# The final model is:
Fml_no@call

# Chi
Anova(Fml_no)

# F test
AnvF_no

# ANCOVA
Iml_ancv_no@call
Anova(Iml_ancv_no)

Fml_ancv_no@call
# Chi
Anova(Fml_ancv_no)

# F test
AnvF_ancv_no

par(mfrow = c(1, 2))
visreg(Fml_ancv_no, xvar = "moist", point = list(col = Extr_DF2$temp))
visreg(Fml_ancv_no, xvar = "Temp5_Mean", point = list(col = Extr_DF2$temp))
