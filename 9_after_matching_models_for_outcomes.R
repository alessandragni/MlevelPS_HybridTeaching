source('Utils_functions/dotpl.R')
source('Utils_functions/import_libraries.R')

#### MODELS AFTER MATCHING (WITHOUT REPLACEMENT) ####

#### MODELLO PER ECTS_cat - MLM - AC ####
clmm_mod <- clmm(formula("ECTS_cat ~ admission_score + career_admission_age + 
                gender + previous_studies + origins + highschool_grade + hybrid_teaching +
                         (1 + hybrid_teaching|stud_career_degree_name)"), 
                 control = clmm.control(innerCtrl = "noWarn"), 
                 data=m.M.cfu)
summary(clmm_mod)
r1 <- ranef(clmm_mod, condVar=TRUE)

d11 = VarCorr(clmm_mod)$stud_career_degree_name[1]
d22 = VarCorr(clmm_mod)$stud_career_degree_name[4]
d12 = VarCorr(clmm_mod)$stud_career_degree_name[3]
rho = attr(VarCorr(clmm_mod)$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(m.M.cfu$hybrid_teaching^2) + 2*d12*mean(m.M.cfu$hybrid_teaching) 
sigma2_b

jpeg(file="Images/dotplot_ECTS_matchM.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp = dotpl(r1, scales = list(x = list(relation = 'free')))
dp$stud_career_degree_name$main <- "CLMM - PS AC Matching from MLM"
print(dp)
dev.off()   

#### MODELLO PER ECTS_cat - GLMM - WC ####
clmm_mod <- clmm(formula("ECTS_cat ~ admission_score + career_admission_age + 
                gender + previous_studies + origins + highschool_grade + hybrid_teaching +
                         (1 + hybrid_teaching|stud_career_degree_name)"), 
                 control = clmm.control(innerCtrl = "noWarn"),
                 data=m.Rby.cfu)
summary(clmm_mod)
r1 <- ranef(clmm_mod, condVar=TRUE)

d11 = VarCorr(clmm_mod)$stud_career_degree_name[1]
d22 = VarCorr(clmm_mod)$stud_career_degree_name[4]
d12 = VarCorr(clmm_mod)$stud_career_degree_name[3]
rho = attr(VarCorr(clmm_mod)$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(m.Rby.cfu$hybrid_teaching^2) + 2*d12*mean(m.Rby.cfu$hybrid_teaching) 
sigma2_b

jpeg(file="Images/dotplot_ECTS_matchR.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp = dotpl(r1, scales = list(x = list(relation = 'free')))
dp$stud_career_degree_name$main <- "CLMM - PS WC Matching from GLMM"
print(dp)
dev.off() 


#### MODELLO PER ECTS - MLM - AC ####

glmmTMB_zicpoisson <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                                gender + previous_studies + origins + highschool_grade + 
                                hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                              zi =~admission_score + career_admission_age + 
                                gender + previous_studies + origins + highschool_grade + 
                                hybrid_teaching,
                              family=poisson, data=m.M.cfu)
glmmTMB_zicnbinom1 <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                                gender + previous_studies + origins + highschool_grade + 
                                hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                              zi =~admission_score + career_admission_age + 
                                gender + previous_studies + origins + highschool_grade + 
                                hybrid_teaching,
                              family=nbinom1, data=m.M.cfu)

glmmTMB_zipoisson <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                               gender + previous_studies + origins + highschool_grade + 
                               hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                             zi =~1 + (1 |stud_career_degree_name),
                             family=poisson, data=m.M.cfu)
glmmTMB_zinbinom1 <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                               gender + previous_studies + origins + highschool_grade + 
                               hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                             zi =~1 + (1 |stud_career_degree_name),
                             family=nbinom1, data=m.M.cfu)

glmmTMB_trpoisson <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                               gender + previous_studies + origins + highschool_grade + 
                               hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                             zi =~1 + (1 |stud_career_degree_name),
                             family=truncated_poisson, data=m.M.cfu)
glmmTMB_trnbinom1 <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                               gender + previous_studies + origins + highschool_grade + 
                               hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                             zi =~1 + (1 |stud_career_degree_name),
                             family=truncated_nbinom1, data=m.M.cfu)

AIC(glmmTMB_zicpoisson, glmmTMB_zicnbinom1, 
    glmmTMB_zipoisson, glmmTMB_zinbinom1,
    glmmTMB_trpoisson, glmmTMB_trnbinom1)

summary(glmmTMB_zicnbinom1)
# plot of the random intercept
jpeg(file="Images/dotplot_ECTS_infl_matchM.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp = lme4:::dotplot.ranef.mer(ranef(glmmTMB_zicnbinom1)$cond)
dp$stud_career_degree_name$main <- "ZINBMM - PS AC Matching from MLM"
print(dp)
dev.off()   

d11 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[1]
d22 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[4]
d12 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[3]
rho = attr(VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(m.M.cfu$hybrid_teaching^2) + 2*d12*mean(m.M.cfu$hybrid_teaching) 
sigma2_b



#### MODELLO PER ECTS - GLMM - WC ####

glmmTMB_zicpoisson <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                          gender + previous_studies + origins + highschool_grade + 
                          hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                        zi =~admission_score + career_admission_age + 
                          gender + previous_studies + origins + highschool_grade + 
                          hybrid_teaching,
                        family=poisson, data=m.Rby.cfu)
glmmTMB_zicnbinom1 <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                           gender + previous_studies + origins + highschool_grade + 
                           hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                         zi =~admission_score + career_admission_age + 
                           gender + previous_studies + origins + highschool_grade + 
                           hybrid_teaching,
                         family=nbinom1, data=m.Rby.cfu)

glmmTMB_zipoisson <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                               gender + previous_studies + origins + highschool_grade + 
                               hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                             zi =~1 + (1 |stud_career_degree_name),
                             family=poisson, data=m.Rby.cfu)
glmmTMB_zinbinom1 <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                               gender + previous_studies + origins + highschool_grade + 
                               hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                             zi =~1 + (1 |stud_career_degree_name),
                             family=nbinom1, data=m.Rby.cfu)

glmmTMB_trpoisson <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                              gender + previous_studies + origins + highschool_grade + 
                              hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                            zi =~1 + (1 |stud_career_degree_name),
                            family=truncated_poisson, data=m.Rby.cfu)
glmmTMB_trnbinom1 <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                              gender + previous_studies + origins + highschool_grade + 
                              hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                            zi =~1 + (1 |stud_career_degree_name),
                            family=truncated_nbinom1, data=m.Rby.cfu)

AIC(glmmTMB_zicpoisson, glmmTMB_zicnbinom1, 
      glmmTMB_zipoisson, glmmTMB_zinbinom1,
      glmmTMB_trpoisson, glmmTMB_trnbinom1)

summary(glmmTMB_zicnbinom1)

# plot of the random intercept
jpeg(file="Images/dotplot_ECTS_infl_matchR.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp = lme4:::dotplot.ranef.mer(ranef(glmmTMB_zicnbinom1)$cond)
dp$stud_career_degree_name$main <- "ZINBMM - PS WC Matching from GLMM"
print(dp)
dev.off()   

d11 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[1]
d22 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[4]
d12 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[3]
rho = attr(VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(m.Rby.cfu$hybrid_teaching^2) + 2*d12*mean(m.Rby.cfu$hybrid_teaching) 
sigma2_b




#### MODELLO PER LA GPA - MLM - AC ####
lmer_mod = lmer("GPA ~ admission_score + career_admission_age + 
  gender + previous_studies + origins + highschool_grade + 
  hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name)", data=m.M.grade)
summary(lmer_mod)
AIC(lmer_mod)

d11 = VarCorr(lmer_mod)[1]$stud_career_degree_name[1]
d22 = VarCorr(lmer_mod)[1]$stud_career_degree_name[4]
d12 = VarCorr(lmer_mod)[1]$stud_career_degree_name[3]
rho = attr(VarCorr(lmer_mod)[1]$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(m.M.grade$hybrid_teaching^2) + 2*d12*mean(m.M.grade$hybrid_teaching) 
sigma2_b

print(vc <- VarCorr(lmer_mod), comp = c("Variance", "Std.Dev."))
sigma2_eps <- attr(vc, "sc")^2
sigma2_eps

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

# plot of the random intercept
jpeg(file="Images/dotplot_GPA_matchM.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp <- dotplot(ranef(lmer_mod), scales = list(x = list(relation = 'free')))
dp$stud_career_degree_name$main <- "LMM - PS AC Matching from MLM"
print(dp)
dev.off()   

# for the pvalues
m1 <- lme(GPA ~ admission_score + career_admission_age + 
            gender + previous_studies + origins + highschool_grade + hybrid_teaching,
          random=~1 + hybrid_teaching|stud_career_degree_name,data=m.M.grade)
summary(m1)


#### MODELLO PER LA GPA - GLMM - WC ####
lmer_mod = lmer("GPA ~ admission_score + career_admission_age + 
  gender + previous_studies + origins + highschool_grade + 
  hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name)", data=m.Rby.grade)
summary(lmer_mod)
AIC(lmer_mod)

d11 = VarCorr(lmer_mod)[1]$stud_career_degree_name[1]
d22 = VarCorr(lmer_mod)[1]$stud_career_degree_name[4]
d12 = VarCorr(lmer_mod)[1]$stud_career_degree_name[3]
rho = attr(VarCorr(lmer_mod)[1]$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(m.Rby.grade$hybrid_teaching^2) + 2*d12*mean(m.Rby.grade$hybrid_teaching) 
sigma2_b

print(vc <- VarCorr(lmer_mod), comp = c("Variance", "Std.Dev."))
sigma2_eps <- attr(vc, "sc")^2
sigma2_eps

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

# plot of the random intercept
jpeg(file="Images/dotplot_GPA_matchR.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp <- dotplot(ranef(lmer_mod), scales = list(x = list(relation = 'free')))
dp$stud_career_degree_name$main <- "LMM - PS WC Matching from GLMM"
print(dp)
dev.off()   

# for the pvalues
m1 <- lme(GPA ~ admission_score + career_admission_age + 
            gender + previous_studies + origins + highschool_grade + hybrid_teaching,
          random=~1 + hybrid_teaching|stud_career_degree_name,data=m.Rby.grade)
summary(m1)








#### MODELLO CON ZERO PER LA GPA - MLM - AC ####
glmmTMB_tweedie <- glmmTMB(GPA ~ admission_score + career_admission_age + 
                               gender + previous_studies + origins + highschool_grade + 
                               hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                             zi =~admission_score + career_admission_age + 
                             gender + previous_studies + origins + highschool_grade + 
                             hybrid_teaching + (1 |stud_career_degree_name),
                             family=tweedie, data=m.M.cfu)
summary(glmmTMB_tweedie)


d11 = VarCorr(glmmTMB_tweedie)[[1]]$stud_career_degree_name[1]
d22 = VarCorr(glmmTMB_tweedie)[[1]]$stud_career_degree_name[4]
d12 = VarCorr(glmmTMB_tweedie)[[1]]$stud_career_degree_name[3]
rho = attr(VarCorr(glmmTMB_tweedie)[[1]]$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(m.M.cfu$hybrid_teaching^2) + 2*d12*mean(m.M.cfu$hybrid_teaching) 
sigma2_b


# plot of the random intercept
jpeg(file="Images/dotplot_GPA_zeros_matchM.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp <-  lme4:::dotplot.ranef.mer(ranef(glmmTMB_zicnbinom1)$cond)
dp$stud_career_degree_name$main <- "? - PS AC Matching from MLM"
print(dp)
dev.off()   

#### MODELLO CON ZERO PER LA GPA - GLMM - WC ####
glmmTMB_tweedie <- glmmTMB(GPA ~ admission_score + career_admission_age + 
                             gender + previous_studies + origins + highschool_grade + 
                             hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                           zi =~admission_score + career_admission_age + 
                             gender + previous_studies + origins + highschool_grade + 
                             hybrid_teaching + (1 |stud_career_degree_name),
                           family=tweedie, data=m.Rby.cfu)
summary(glmmTMB_tweedie)


d11 = VarCorr(glmmTMB_tweedie)[[1]]$stud_career_degree_name[1]
d22 = VarCorr(glmmTMB_tweedie)[[1]]$stud_career_degree_name[4]
d12 = VarCorr(glmmTMB_tweedie)[[1]]$stud_career_degree_name[3]
rho = attr(VarCorr(glmmTMB_tweedie)[[1]]$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(m.Rby.cfu$hybrid_teaching^2) + 2*d12*mean(m.Rby.cfu$hybrid_teaching) 
sigma2_b


# plot of the random intercept
jpeg(file="Images/dotplot_GPA_zeros_matchR.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp <-  lme4:::dotplot.ranef.mer(ranef(glmmTMB_tweedie)$cond)
dp$stud_career_degree_name$main <- "? - PS WC Matching from GLMM"
print(dp)
dev.off()   



