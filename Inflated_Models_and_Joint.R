source('Utils_functions/import_libraries.R')
source('Utils_functions/dotpl.R')

data_temp = data
data_temp$logGPA <- ifelse(data_temp$GPA > 0, log(data_temp$GPA), 0)

hist(data_temp$logGPA, breaks=100)

# https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/
library("GLMMadaptive")
fm1 <- mixed_model(fixed = logGPA ~ admission_score + career_admission_age + 
                     gender + previous_studies + origins + highschool_grade + hybrid_teaching, 
                   random = ~ 1 + hybrid_teaching|stud_career_degree_name, 
                   data = data_temp, 
                   family = hurdle.lognormal(), 
                   #family = hurdle.poisson(), 
                   iter_EM = 0,
                   zi_fixed = ~ 1)

summary(fm1)
fixef(fm1)
marginal_coefs(fm1)
dotplot(ranef(fm1))



library("glmmTMB")

glmmTMB_mod_poi <- glmmTMB(formula("ECTS ~ admission_score + career_admission_age + 
                            gender + previous_studies + origins + highschool_grade + hybrid_teaching +
                            (1 + hybrid_teaching|stud_career_degree_name)"), 
                    zi=~.,
                    family=poisson(link = "log"),
                    data=data)
summary(glmmTMB_mod_poi)


glmmTMB_mod_binom <- glmmTMB(formula("ECTS ~ admission_score + career_admission_age + 
                            gender + previous_studies + origins + highschool_grade + hybrid_teaching +
                            (1 + hybrid_teaching|stud_career_degree_name)"), 
                       zi=~.,
                       family=nbinom2,
                       data=data)
summary(glmmTMB_mod_binom)


glmmTMB_mod_hurd <- glmmTMB(formula("ECTS ~ admission_score + career_admission_age + 
                            gender + previous_studies + origins + highschool_grade + hybrid_teaching +
                            (1 + hybrid_teaching|stud_career_degree_name)"), 
                       zi=~.,
                       family=truncated_poisson,
                       data=data)
summary(glmmTMB_mod_hurd)

anova(glmmTMB_mod_poi, glmmTMB_mod_binom, glmmTMB_mod_hurd)
