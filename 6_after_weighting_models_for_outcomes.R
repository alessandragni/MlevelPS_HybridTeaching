source('Utils_functions/import_libraries.R')
source('Utils_functions/dotpl.R')

##### MODELS #####

#### MODEL FOR ECTS_cat - M ####
clmm_modM <- clmm(formula("ECTS_cat ~ admission_score + career_admission_age + 
                gender + previous_studies + origins + highschool_grade + hybrid_teaching +
                         (1 + hybrid_teaching|stud_career_degree_name)"), 
                  control = clmm.control(innerCtrl = "noWarn"),
                  weights = data$psMw,
                  data=data)
summary(clmm_modM)
r1 <- ranef(clmm_modM, condVar=TRUE)

jpeg(file="Images/dotplot_ECTS_weighting_M.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp = dotpl(r1, scales = list(x = list(relation = 'free')))
dp$stud_career_degree_name$main <- "CLMM - PS Weighting from MLM"
print(dp)
dev.off()   

d11 = VarCorr(clmm_modM)$stud_career_degree_name[1]
d22 = VarCorr(clmm_modM)$stud_career_degree_name[4]
d12 = VarCorr(clmm_modM)$stud_career_degree_name[3]
rho = attr(VarCorr(clmm_modM)$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(data$hybrid_teaching^2) + 2*d12*mean(data$hybrid_teaching) 
sigma2_b

#### MODEL FOR ECTS_cat - R ####
clmm_modR <- clmm(formula("ECTS_cat ~ admission_score + career_admission_age + 
                gender + previous_studies + origins + highschool_grade + hybrid_teaching +
                         (1 + hybrid_teaching|stud_career_degree_name)"), 
                 control = clmm.control(innerCtrl = "noWarn"),
                 weights = data$psRw,
                 data=data)
summary(clmm_modR)
r1 <- ranef(clmm_modR, condVar=TRUE)

d11 = VarCorr(clmm_modR)$stud_career_degree_name[1]
d22 = VarCorr(clmm_modR)$stud_career_degree_name[4]
d12 = VarCorr(clmm_modR)$stud_career_degree_name[3]
rho = attr(VarCorr(clmm_modR)$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(data$hybrid_teaching^2) + 2*d12*mean(data$hybrid_teaching) 
sigma2_b

jpeg(file="Images/dotplot_ECTS_weighting_R.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp = dotpl(r1, scales = list(x = list(relation = 'free')))
dp$stud_career_degree_name$main <- "CLMM - PS Weighting from GLMM"
print(dp)
dev.off()   



#### MODEL FOR ECTS - M ####

glmmTMB_zicnbinom1 <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                                gender + previous_studies + origins + highschool_grade + 
                                hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                              zi =~admission_score + career_admission_age + 
                                gender + previous_studies + origins + highschool_grade + 
                                hybrid_teaching + (1 |stud_career_degree_name),
                              weights = data$psMw,
                              family=nbinom1, data=data)



# plot of the random intercept

jpeg(file="Images/dotplot_ECTS_infl_weighting_M.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp = lme4:::dotplot.ranef.mer(ranef(glmmTMB_zicnbinom1)$cond)
dp$stud_career_degree_name$main <- "ZINBMM - PS Weighting from MLM"
print(dp)
dev.off() 

# d11 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[1]
# d22 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[4]
# d12 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[3]
# rho = attr(VarCorr(glmmTMB_zinbinom1)[[1]]$stud_career_degree_name, "correlation")[2]
# sigma2_b <- d11 + d22*mean(data$hybrid_teaching^2) + 2*d12*mean(data$hybrid_teaching) 
# sigma2_b


#### MODEL FOR ECTS - R ####


glmmTMB_zicnbinom1 <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                                gender + previous_studies + origins + highschool_grade + 
                                hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                              zi =~admission_score + career_admission_age + 
                                gender + previous_studies + origins + highschool_grade + 
                                hybrid_teaching + (1 |stud_career_degree_name),
                              weights = data$psRw,
                              family=nbinom1, data=data)

# plot of the random intercept

jpeg(file="Images/dotplot_ECTS_infl_weighting_R.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp = lme4:::dotplot.ranef.mer(ranef(glmmTMB_zicnbinom1)$cond)
dp$stud_career_degree_name$main <- "ZINBMM - PS Weighting from GLMM"
print(dp)
dev.off() 

# d11 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[1]
# d22 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[4]
# d12 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[3]
# rho = attr(VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name, "correlation")[2]
# sigma2_b <- d11 + d22*mean(data$hybrid_teaching^2) + 2*d12*mean(data$hybrid_teaching) 
# sigma2_b



#### MODEL FOR LA GPA - M ####
lmer_mod_M = lmer("GPA ~ admission_score + career_admission_age + 
  gender + previous_studies + origins + highschool_grade + 
  hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name)", 
                weights = data_grade$psMw, control=lmerControl(optimizer="bobyqa"),
                data=data_grade)
summary(lmer_mod_M)
AIC(lmer_mod_M)

d11 = VarCorr(lmer_mod_M)[1]$stud_career_degree_name[1]
d22 = VarCorr(lmer_mod_M)[1]$stud_career_degree_name[4]
d12 = VarCorr(lmer_mod_M)[1]$stud_career_degree_name[3]
rho = attr(VarCorr(lmer_mod_M)[1]$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(data_grade$hybrid_teaching^2) + 2*d12*mean(data_grade$hybrid_teaching) 
sigma2_b

print(vc <- VarCorr(lmer_mod_M), comp = c("Variance", "Std.Dev."))
sigma2_eps <- attr(vc, "sc")^2
sigma2_eps

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

coefs <- data.frame(coef(summary(lmer_mod_M)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
round(coefs,3)


# plot of the random intercept
jpeg(file="Images/dotplot_GPA_weighting_M.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp <- dotplot(ranef(lmer_mod_M), scales = list(x = list(relation = 'free')))
dp$stud_career_degree_name$main <- "LMM - PS Weighting from MLM"
print(dp)
dev.off()   

 
#### MODEL FOR GPA - R ####
lmer_mod_R = lmer("GPA ~ admission_score + career_admission_age + 
  gender + previous_studies + origins + highschool_grade + 
  hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name)", 
                weights = data_grade$psRw, 
                data=data_grade)
summary(lmer_mod_R)
AIC(lmer_mod_R)

d11 = VarCorr(lmer_mod_R)[1]$stud_career_degree_name[1]
d22 = VarCorr(lmer_mod_R)[1]$stud_career_degree_name[4]
d12 = VarCorr(lmer_mod_R)[1]$stud_career_degree_name[3]
rho = attr(VarCorr(lmer_mod_R)[1]$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(data_grade$hybrid_teaching^2) + 2*d12*mean(data_grade$hybrid_teaching) 
sigma2_b

print(vc <- VarCorr(lmer_mod_R), comp = c("Variance", "Std.Dev."))
sigma2_eps <- attr(vc, "sc")^2
sigma2_eps

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

coefs <- data.frame(coef(summary(lmer_mod_R)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
round(coefs,3)


# plot of the random intercept
jpeg(file="Images/dotplot_GPA_weighting_R.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp <- dotplot(ranef(lmer_mod_R), scales = list(x = list(relation = 'free')))
dp$stud_career_degree_name$main <- "LMM - PS Weighting from GLMM"
print(dp)
dev.off()   
