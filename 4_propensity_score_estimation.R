
source('Utils_functions/import_libraries.R')

covariateNames <- c(
  "admission_score",
  "career_admission_age",
  "gender",
  "previous_studies",
  "origins",
  "highschool_grade")


##### Formulae definition #####

ps.formulaM <- paste("hybrid_teaching ~ ", paste(covariateNames, collapse=" + "))
ps.formulaM

ps.formulaR <- paste(paste("hybrid_teaching ~ ", paste(covariateNames, collapse=" + ")),
                     " + (1 | stud_career_degree_name)")
ps.formulaR   

# Dataframe with unit ID, treatment indicator, propensity scores and weights creation
prs_df <- data.frame(data$career_anonymous_id, 
                     data$hybrid_teaching,
                     data$stud_career_degree_name) 
prs_df <- prs_df %>% dplyr::rename(ID = data.career_anonymous_id,
                                   treatment = data.hybrid_teaching)

#### FIRST FOR data ####

##### PS estimation : Marginal Model (M) #####

# model fitting
ps.modelM <- glm(formula = formula(ps.formulaM), data=data, family=binomial)
summary(ps.modelM)
AIC(ps.modelM)
#stargazer(ps.modelM, type = "latex", title = "PS Marginal Model summary", summary = FALSE)

# propensity scores and ps-inverse weights computation
data$psM <- fitted(ps.modelM) # propensity scores 

data$psMw <- with(data, ifelse(hybrid_teaching==1, 1/psM, 1/(1-psM)))

# distributions
with(data, by(psM, hybrid_teaching, summary))
with(data, by(psMw, hybrid_teaching, summary))

# add to prs_df
prs_df$psM = data$psM
prs_df$psMw = data$psMw

##### PS estimation : Random Effects Model (R) #####
ps.modelR <- glmer(formula = ps.formulaR, family = binomial(link="logit"), 
                   data = data) 
summary(ps.modelR)
# stargazer(ps.modelR, type = "latex", title = "PS Mixed-effects Model summary", summary = FALSE)

dotplot(ranef(ps.modelR)) 

# plot of the random intercept
jpeg(file="Images/dotplot_R.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp <- dotplot(ranef(ps.modelR), scales = list(x = list(relation = 'free')))
dp$stud_career_degree_name$main <- "GLMM - PS Estimation - PoliMi"
print(dp)
dev.off()  

VPC = VarCorr(ps.modelR)[[1]][1]/(VarCorr(ps.modelR)[[1]][1] +pi^2/3) 
VPC


# propensity scores and ps-inverse weights computation
data$psR = fitted(ps.modelR) 
data$psRw <- with(data, ifelse(hybrid_teaching==1, 1/psR, 1/(1-psR)))

# distributions
with(data, by(psR, hybrid_teaching, summary))
with(data, by(psRw, hybrid_teaching, summary))

# add to prs_df
prs_df$psR = data$psR
prs_df$psRw = data$psRw


##### Some plots for the PS #####
##### FOR M #####
labs <- c("Hybrid", "Face-to-face")
PS_M = prs_df %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = psM)) + 
  geom_histogram(aes(y = ..density..)) + ylim(0, 8) +
  facet_wrap(~treatment) +
  xlab("Propensity score - MLM - PoliMi") 
ggsave("Images/PropensityScore_ModelM.jpeg", plot = PS_M, width = 8, height = 6)

# or
library(ggpattern)
PS_M = prs_df %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = psM, fill = treatment))+
  geom_histogram(aes(y=..density..), color='gray50',
                 alpha=0.2, position = "identity") + ylim(0, 8) +
  geom_density(alpha=0.2) + theme(legend.position = "top") +
  geom_density_pattern(pattern_color = "white",
                       pattern_fill = "grey",
                       alpha=0.01,
                       aes(pattern = treatment)) +
  scale_color_grey() + scale_fill_grey() +
  labs(x = 'Propensity Score', subtitle = 'Delivered teaching type') 
ggsave("Images/PropensityScore_ModelM.jpeg", plot = PS_M, width = 8, height = 5)


##### FOR R #####
PS_R = prs_df %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = psR)) + 
  geom_histogram(aes(y = ..density..)) + ylim(0, 8) +
  facet_wrap(~treatment) +
  xlab("Propensity score - GLMM - PoliMi") 
ggsave("Images/PropensityScore_ModelR.jpeg", plot = PS_R, width = 8, height = 6)

# or
PS_R = prs_df %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = psR, fill = treatment))+
  geom_histogram(aes(y=..density..), color='gray50',
                 alpha=0.2, position = "identity") + ylim(0, 8) +
  geom_density(alpha=0.2) + theme(legend.position = "top") +
  geom_density_pattern(pattern_color = "white",
                       pattern_fill = "grey",
                       alpha=0.01,
                       aes(pattern = treatment)) +
  scale_color_grey() + scale_fill_grey() +
  labs(x = 'Propensity Score', subtitle = 'Delivered teaching type') 
ggsave("Images/PropensityScore_ModelR.jpeg", plot = PS_R, width = 8, height = 5)


##### NOW FOR data_grade #####

# Dataframe with unit ID, treatment indicator, propensity scores and weights creation
prs_df_grade <- data.frame(data_grade$career_anonymous_id, 
                           data_grade$hybrid_teaching,
                           data_grade$stud_career_degree_name) 
prs_df_grade <- prs_df_grade %>% dplyr::rename(ID = data_grade.career_anonymous_id,
                                               treatment = data_grade.hybrid_teaching)

##### PS estimation : Marginal Model (M) #####

# model fitting
ps.modelM <- glm(formula = formula(ps.formulaM), data=data_grade, family=binomial)
summary(ps.modelM)
AIC(ps.modelM)
stargazer(ps.modelM, type = "latex", title = "PS Marginal Model summary", summary = FALSE)

# propensity scores and ps-inverse weights computation
data_grade$psM <- fitted(ps.modelM) # propensity scores 

data_grade$psMw <- with(data_grade, ifelse(hybrid_teaching==1, 1/psM, 1/(1-psM)))

# distributions
with(data_grade, by(psM, hybrid_teaching, summary))
with(data_grade, by(psMw, hybrid_teaching, summary))

# add to prs_df_grade
prs_df_grade$psM = data_grade$psM
prs_df_grade$psMw = data_grade$psMw


##### PS estimation : Random Effects Model (R) #####

ps.modelR <- glmer(formula = formula(ps.formulaR), 
                   family = binomial(link="logit"), 
                   data = data_grade) 
summary(ps.modelR)
# stargazer(ps.modelR, type = "latex", title = "PS Mixed-effects Model summary", summary = FALSE)

VPC = VarCorr(ps.modelR)[[1]][1]/(VarCorr(ps.modelR)[[1]][1] +pi^2/3) 
VPC

dotplot(ranef(ps.modelR)) 

# plot of the random intercept
jpeg(file="Images/dotplot_R_gpa.jpeg", width = 150, height = 150, units = 'mm', res = 250)
dp <- dotplot(ranef(ps.modelR), scales = list(x = list(relation = 'free')))
dp$stud_career_degree_name$main <- "GLMM - PS Estimation - PoliMi_GPA"
print(dp)
dev.off()   

# propensity scores and ps-inverse weights computation
data_grade$psR = fitted(ps.modelR) 
data_grade$psRw <- with(data_grade, ifelse(hybrid_teaching==1, 1/psR, 1/(1-psR)))

# distributions
with(data_grade, by(psR, hybrid_teaching, summary))
with(data_grade, by(psRw, hybrid_teaching, summary))

# add to prs_df_grade
prs_df_grade$psR = data_grade$psR
prs_df_grade$psRw = data_grade$psRw


##### Some plots for the PS #####
##### FOR M #####
labs <- c("Hybrid", "Face-to-face")
PS_M = prs_df_grade %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = psM)) + 
  geom_histogram(aes(y = ..density..)) + ylim(0, 8) +
  facet_wrap(~treatment) +
  xlab("Propensity score - MLM - PoliMi_GPA") 
ggsave("Images/PropensityScore_ModelM_gpa.jpeg", plot = PS_M, width = 8, height = 6)

PS_M = prs_df_grade %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = psM, fill = treatment))+
  geom_histogram(aes(y=..density..), color='gray50',
                 alpha=0.2, position = "identity") + ylim(0, 8) +
  geom_density(alpha=0.2) + theme(legend.position = "top") +
  geom_density_pattern(pattern_color = "white",
                       pattern_fill = "grey",
                       alpha=0.01,
                       aes(pattern = treatment)) +
  scale_color_grey() + scale_fill_grey() +
  labs(x = 'Propensity Score', subtitle = 'Delivered teaching type') 
ggsave("Images/PropensityScore_ModelM_gpa.jpeg", plot = PS_M, width = 8, height = 5)


##### FOR R #####
PS_R = prs_df_grade %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = psR)) + 
  geom_histogram(aes(y = ..density..)) + ylim(0, 8) +
  facet_wrap(~treatment) +
  xlab("Propensity score - GLMM - PoliMi_GPA") 
ggsave("Images/PropensityScore_ModelR_gpa.jpeg", plot = PS_R, width = 8, height = 5)

PS_R =prs_df_grade %>%
  mutate(treatment = ifelse(treatment == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = psR, fill = treatment))+
  geom_histogram(aes(y=..density..), color='gray50',
                 alpha=0.2, position = "identity") + ylim(0, 8) +
  geom_density(alpha=0.2) + theme(legend.position = "top") +
  geom_density_pattern(pattern_color = "white",
                       pattern_fill = "grey",
                       alpha=0.01,
                       aes(pattern = treatment)) +
  scale_color_grey() + scale_fill_grey() +
  labs(x = 'Propensity Score', subtitle = 'Delivered teaching type') 
ggsave("Images/PropensityScore_ModelR_gpa.jpeg", plot = PS_R, width = 8, height = 5)




