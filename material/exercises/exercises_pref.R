#This exercise uses the `/data/prefdata.csv` dataset. it contains measurements of leaf mass per area (LMA),
#and distance from the top of the tree (dfromtop) on 35 trees of two species. 
#We want to know whether LMA decreases with dfromtop as expected, and whether this decrease 
# in LMA with distance from top differs by species.

#################################################################################
# 1. read the data and inspect the first few rows, and the species variable.
library(tidyverse)
prefdata <- read_csv("C:/Users/Gebruiker/Dropbox/tridata/courses-R-TNO/data/prefdata.csv")

####################################################################################
#  fit a linear regresion model by species (ignoring individual-level variation) 


lm1 <- lm(LMA ~ species + dfromtop + species:dfromtop , data = prefdata)
summary(lm1)



# 3. plot the linear predictions (hint load the library visreg and use the function `visreg(lm1, "dfromtop", by = "species", overlay =TRUE)`) 
# Plot predictions
library(visreg)
visreg(lm1, "dfromtop", by="species", overlay=TRUE)


# 4. Look at the anova table for the fitted linear regression model from the
# example above to confirm that LMA does not significantly change with dfromtop. 
anova(lm1)

# 5. To see whether the relationship betwee LMA and dfromtop potentially varies from tree to tree, 
# fit a linear regression separately for each tree using the lmlist function in the `lme4` package 
# and then plot the outcome. 



lmlist1 <- lmList(LMA ~ dfromtop | ID , data = prefdata)

liscoef <- coef(lmlist1)

#6.  To specify random effects with `lmer` we add it to the formula in the right-hand side. For example, a random intercept for an  `ID` (that is, the intercept will vary randomly among `IDs` ) is coded as(1|ID). If we also allow the slope of the relationship to vary, we specify it as (`dfromtop|ID`) the slope and intercept of the relationship between `LMA` and `dfromtop` will vary randomly between tree `IDs`.
#+ Fit a random intercept model (`pref_m1`) only with species::dfromtop interaction 
#+ Fit a random intercept and slope model (`pref_m2`) 
#+ Use the `AIC` (Akaike Information criterion) to compare (`pref_m1`) and (`pref_m2`) and conclude that we don't need a random slope.    
#+ Conclude dat `LMA` indeed decreases with `dfromtop` 

# Random intercept only
lmer1 <- lmer(LMA ~ species + dfromtop + species:dfromtop + (1|ID), data=prefdata)
# Random intercept and slope
lmer2 <- lmer(LMA ~ species + dfromtop + species:dfromtop + (dfromtop|ID), data=prefdata)
# Compare models using AIC
# model 1 is more efficient (lower AIC due to fewer degrees of freedom)
AIC(lmer1, lmer2)

# Evaluate the importance of the interaction between 'species' and 'dfromtop'
lmer1 <- lmer(LMA ~ species + dfromtop + species:dfromtop + (1|ID), data=prefdata)
lmer3 <- lmer(LMA ~ species + dfromtop + (1|ID), data=prefdata)

anova(lmer1, lmer3) # P > 0.05 so interaction probably not important
## Data: prefdata
## Models:
## lmer3: LMA ~ species + dfromtop + (1 | ID)
## lmer1: LMA ~ species + dfromtop + species:dfromtop + (1 | ID)
## Df AIC BIC logLik deviance Chisq Chi Df Pr(>Chisq)
## lmer3 5 2263.7 2281.2 -1126.8 2253.7
## lmer1 6 2263.2 2284.3 -1125.6 2251.2 2.4417 1 0.1182
AIC(lmer1, lmer3) # there is not much difference, suggesting that the interaction is probably not


## df AIC
## lmer1 6 2251.997
## lmer3 5 2253.002
# Evaluate the importance of 'species' and 'dfromtop' as main effects
lmer3a <- lmer(LMA ~ dfromtop + (1|ID), data=pref, REML=F)
lmer3b <- lmer(LMA ~ species + (1|ID), data=pref, REML=F)
anova(lmer3, lmer3a)
## Data: pref
## Models:
## lmer3a: LMA ~ dfromtop + (1 | ID)
## lmer3: LMA ~ species + dfromtop + (1 | ID)
## Df AIC BIC logLik deviance Chisq Chi Df Pr(>Chisq)
## lmer3a 4 2321.7 2335.8 -1156.9 2313.7
## lmer3 5 2263.7 2281.2 -1126.8 2253.7 60.085 1 9.085e-15 ***
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
anova(lmer3, lmer3b) # model fit significantly reduced by dropping either main effect
## Data: pref
## Models:
## lmer3b: LMA ~ species + (1 | ID)
## lmer3: LMA ~ species + dfromtop + (1 | ID)
## Df AIC BIC logLik deviance Chisq Chi Df Pr(>Chisq)
## lmer3b 4 2326.3 2340.4 -1159.2 2318.3
## lmer3 5 2263.7 2281.2 -1126.8 2253.7 64.678 1 8.82e-16 ***
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
AIC(lmer3, lmer3a, lmer3b) # model fit greatly reduced by dropping either main effect
## df AIC
## lmer3 5 2253.002
## lmer3a 4 2321.736
## lmer3b 4 2326.329
# Use Anova to compute p-values
library(car)
Anova(lmer3)
## Analysis of Deviance Table (Type II Wald chisquare tests)

##
## Response: LMA
## Chisq Df Pr(>Chisq)
## species 152.913 1 < 2.2e-16 ***
## dfromtop 85.178 1 < 2.2e-16 ***
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

