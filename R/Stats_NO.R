## ----Stat_WTC_Extr_Nitrate

range(Extr_DF$no)

bxplts(value = "no", ofst = 0.1, data = Extr_DF)

# power(1/3)
Iml_no <- lmer(no^(1/3) ~ temp * time + (1|chamber), data = Extr_DF)
Anova(Iml_no)

Fml_no <- stepLmer(Iml_no)
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
xyplot(no ~ moist|temp, groups = chamber, type = c("r", "p"), data = Extr_DF)
xyplot(no ~ moist|chamber, type = c("r", "p"), data = Extr_DF)
xyplot(no ~ moist|temp, groups = time, type = c("r", "p"), data = Extr_DF)
xyplot(no ~ moist|time, type = c("r", "p"), data = Extr_DF)

scatterplotMatrix(~ I(no^(1/3)) + moist + Temp5_Mean, data = Extr_DF, diag = "boxplot", 
                  groups = Extr_DF$temp, by.group = TRUE)
scatterplotMatrix(~ log(no) + log(moist) + Temp5_Mean, data = Extr_DF, diag = "boxplot", 
                  groups = Extr_DF$temp, by.group = TRUE)


m1 <- lmer(log(no) ~ temp * log(moist) + (1|time) + (1|chamber), 
                    data = Extr_DF)
Anova(m1)
visreg(m1, xvar = "moist", by = "temp", overlay = TRUE)
# interaction is indicated, but moisture range is quite different. what if I use
# the samge range of moisture for both treatment
ddply(Extr_DF, .(temp), summarise, range(moist))
m2 <- update(m1, subset = moist < 0.14)
Anova(m2)
visreg(m2, xvar = "moist", by = "temp", overlay = TRUE)
# no indifcation of interaction; so remove interaction

Iml_ancv_no <- lmer(log(no) ~ temp + log(moist) + (1|time) + (1|chamber),  data = Extr_DF)
m2 <- update(Iml_ancv_no, ~. - (1|time))
m3 <- update(Iml_ancv_no, ~. - (1|chamber))
anova(Iml_ancv_no, m2, m3)
Anova(Iml_ancv_no)

Fml_ancv_no <- update(Iml_ancv_no, ~. - temp)
anova(Iml_ancv_no, Fml_ancv_no)
AnvF_ancv_no <- Anova(Fml_ancv_no, test.statistic = "F")
AnvF_ancv_no

# model diagnosis
plot(Fml_ancv_no)
qqnorm(resid(Fml_ancv_no))
qqline(resid(Fml_ancv_no))

# visualise
visreg(Fml_ancv_no, xvar = "moist", overlay = TRUE, point = list(col = Extr_DF$temp))

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

visreg(Fml_ancv_no, xvar = "moist", overlay = TRUE, point = list(col = Extr_DF$temp))
