if (require("lme4")) {
  # example regressions are from lme4 documentation
  lmm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  tidy(lmm1)
  tidy(lmm1, effects = "fixed")
  tidy(lmm1, effects = "fixed", conf.int=TRUE)
  tidy(lmm1, effects = "fixed", conf.int=TRUE, conf.method="profile")
  tidy(lmm1, effects = "ran_modes", conf.int=TRUE)
  head(augment(lmm1, sleepstudy))
  glance(lmm1)
  
  glmm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                 data = cbpp, family = binomial)
  tidy(glmm1)
  tidy(glmm1, effects = "fixed")
  head(augment(glmm1, cbpp))
  glance(glmm1)
  
  startvec <- c(Asym = 200, xmid = 725, scal = 350)
  
  nm1 <- nlmer(circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym|Tree,
               Orange, start = startvec)
  tidy(nm1)
  
  tidy(nm1, effects = "fixed")
  
  head(augment(nm1, Orange))
  glance(nm1)
  
}