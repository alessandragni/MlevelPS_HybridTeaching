
source('Utils_functions/import_libraries.R')
source('Utils_functions/dotpl.R')

### Models per outcome before everything ###

### MODELLO PER ECTS_cat
clmm_mod <- clmm(formula("ECTS_cat ~ admission_score + career_admission_age + 
                gender + previous_studies + origins + highschool_grade + hybrid_teaching +
                         (1 + hybrid_teaching|stud_career_degree_name)"), 
                 data=data)
summary(clmm_mod)
r1 <- ranef(clmm_mod, condVar=TRUE)
VarCorr(clmm_mod)

# plot of the random intercept
jpeg(file="Images/dotplot_ECTS_prelim.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp = dotpl(r1, scales = list(x = list(relation = 'free')))
dp$stud_career_degree_name$main <- "CLMM - Preliminary Analysis"
print(dp)
dev.off()  

d11 = VarCorr(clmm_mod)$stud_career_degree_name[1]
d22 = VarCorr(clmm_mod)$stud_career_degree_name[4]
d12 = VarCorr(clmm_mod)$stud_career_degree_name[3]
rho = attr(VarCorr(clmm_mod)$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(data$hybrid_teaching^2) + 2*d12*mean(data$hybrid_teaching) 
sigma2_b

### MODELLO PER ECTS

library("glmmTMB")

glmmTMB_zicnbinom1 <- glmmTMB(ECTS ~ admission_score + career_admission_age + 
                                gender + previous_studies + origins + highschool_grade + 
                                hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name),
                              zi =~admission_score + career_admission_age + 
                                gender + previous_studies + origins + highschool_grade + 
                                hybrid_teaching + (1 |stud_career_degree_name),
                              family=nbinom1, data=data)

# plot of the random intercept
jpeg(file="Images/dotplot_ECTS_infl_prelim.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp = lme4:::dotplot.ranef.mer(ranef(glmmTMB_zicnbinom1)$cond)
dp$stud_career_degree_name$main <- "ZINBMM - Preliminary Analysis"
print(dp)
dev.off()  

# d11 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[1]
# d22 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[4]
# d12 = VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name[3]
# rho = attr(VarCorr(glmmTMB_zicnbinom1)[[1]]$stud_career_degree_name, "correlation")[2]
# sigma2_b <- d11 + d22*mean(data$hybrid_teaching^2) + 2*d12*mean(data$hybrid_teaching) 
# sigma2_b


#_______________________________________________________________________________
### MODELLO PER LA GPA

lmer_mod = lmer("GPA ~ admission_score + career_admission_age + 
  gender + previous_studies + origins + highschool_grade + 
  hybrid_teaching + (1 + hybrid_teaching|stud_career_degree_name)", data=data_grade)
summary(lmer_mod)

# plot of the random intercept
jpeg(file="Images/dotplot_GPA_prelim.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp <- dotplot(ranef(lmer_mod), scales = list(x = list(relation = 'free')))
dp$stud_career_degree_name$main <- "LMM - Preliminary Analysis"
print(dp)
dev.off()   

m1 <- lme(GPA ~ admission_score + career_admission_age + 
            gender + previous_studies + origins + highschool_grade + hybrid_teaching,
          random=~1 + hybrid_teaching|stud_career_degree_name,data=data_grade)
summary(m1)


d11 = VarCorr(lmer_mod)[1]$stud_career_degree_name[1]
d22 = VarCorr(lmer_mod)[1]$stud_career_degree_name[4]
d12 = VarCorr(lmer_mod)[1]$stud_career_degree_name[3]
rho = attr(VarCorr(lmer_mod)[1]$stud_career_degree_name, "correlation")[2]
sigma2_b <- d11 + d22*mean(data_grade$hybrid_teaching^2) + 2*d12*mean(data_grade$hybrid_teaching) 
sigma2_b

print(vc <- VarCorr(lmer_mod), comp = c("Variance", "Std.Dev."))
sigma2_eps <- as.numeric(m1$sigma^2)
sigma2_eps

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE


#_______________________________________________________________________________
### MODELLO PER LA GPA INCLUDENDO GLI ZERI

data$logGPA <- ifelse(data$GPA > 0, log(data$GPA), 0)

x11()
hist(data$logGPA, breaks=100)

# https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/
library("GLMMadaptive")
fm1 <- mixed_model(fixed = logGPA ~ admission_score + career_admission_age + 
                     gender + previous_studies + origins + highschool_grade + hybrid_teaching, 
                   random = ~ 1 + hybrid_teaching|stud_career_degree_name, 
                   data = data, 
                   family = hurdle.lognormal(), 
                   #family = hurdle.poisson(), 
                   iter_EM = 0,
                   zi_fixed = ~ 1)

summary(fm1)
fixef(fm1)
marginal_coefs(fm1)
dotplot(ranef(fm1))

