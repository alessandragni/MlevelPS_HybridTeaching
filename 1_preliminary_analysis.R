
#temp <- df_1721_pr[sample(nrow(df_1721_pr), nrow(df) * 0.1), c(covariateNames, outputVar, 'hybrid_teaching')]
#write.csv(temp, "data_complete.csv", row.names=FALSE, quote=FALSE) 

source('Utils_functions/import_libraries.R')
source('Utils_functions/plot_means.R')

# 10% of the rows from the original dataframe were selected
data <- read_csv("data_complete.csv")
data

#### Pre-analysis on preprocessed non-matched data: Difference-in-means ####
##### Difference-in-means #####

# means
data %>%
  group_by(hybrid_teaching) %>%
  dplyr::summarise(n_students = n(),
                   mean_cfu = mean(ECTS),
                   mean_grade = mean(GPA, na.rm=TRUE),
                   mean_adm_sc = mean(admission_score),
                   mean_adm_age = mean(career_admission_age),
                   mean_school_grade = mean(highschool_grade))

# standard deviations
data %>%
  group_by(hybrid_teaching) %>%
  dplyr::summarise(n_students = n(),
                   sd_cfu = sd(ECTS),
                   sd_grade = sd(GPA, na.rm=TRUE),
                   sd_adm_sc = sd(admission_score),
                   sd_adm_age = sd(career_admission_age),
                   sd_school_grade = sd(highschool_grade))


data %>%
  group_by(hybrid_teaching, origins) %>%
  dplyr::summarise(n_students = n()) %>%
  dplyr::mutate(prop = n_students/sum(n_students))

data %>%
  group_by(hybrid_teaching, gender) %>%
  dplyr::summarise(n_students = n()) %>%
  dplyr::mutate(prop = n_students/sum(n_students))

data %>%
  group_by(hybrid_teaching, previous_studies) %>%
  dplyr::summarise(n_students = n()) %>%
  dplyr::mutate(prop = n_students/sum(n_students))


##### Plots #####

ggplot(data,aes(factor(hybrid_teaching),ECTS))+
  geom_boxplot(alpha=0.5)+
  facet_wrap(~factor(stud_career_degree_name))+
  labs(x = "Delivered teaching type", y="ECTS") + 
  scale_x_discrete(labels = c("0" = "Face-to-face", "1" = "Hybrid"))
ggsave("Images/per_EP_per_teaching_ECTS.jpeg", width = 8, height = 6)

ggplot(data,aes(factor(hybrid_teaching),GPA))+
  geom_boxplot(alpha=0.5)+
  facet_wrap(~factor(stud_career_degree_name))+
  labs(x = "Delivered teaching type", y="GPA") + 
  scale_x_discrete(labels = c("0" = "Face-to-face", "1" = "Hybrid"))
ggsave("Images/per_EP_per_teaching_GPA.jpeg", width = 8, height = 6)

ggplot(data,aes(factor(hybrid_teaching),admission_score))+
  geom_boxplot(alpha=0.5)+
  facet_wrap(~factor(stud_career_degree_name))+
  labs(x = "Delivered teaching type", y="admission_score") + 
  scale_x_discrete(labels = c("0" = "Face-to-face", "1" = "Hybrid"))
ggsave("Images/per_EP_per_teaching_admission_score.jpeg", width = 8, height = 6)

ggplot(data,aes(factor(hybrid_teaching),career_admission_age))+
  geom_boxplot(alpha=0.5)+
  facet_wrap(~factor(stud_career_degree_name))+
  labs(x = "Delivered teaching type", y="career_admission_age") + 
  scale_x_discrete(labels = c("0" = "Face-to-face", "1" = "Hybrid"))
ggsave("Images/per_EP_per_teaching_career_admission_age.jpeg", width = 8, height = 6)

ggplot(data,aes(factor(hybrid_teaching),highschool_grade))+
  geom_boxplot(alpha=0.5)+
  facet_wrap(~factor(stud_career_degree_name))+
  labs(x = "Delivered teaching type",y="highschool_grade") + 
  scale_x_discrete(labels = c("0" = "Face-to-face", "1" = "Hybrid"))
ggsave("Images/per_EP_per_teaching_highschool_grade.jpeg", width = 8, height = 6)

ggplot(data, aes(x = factor(hybrid_teaching), y = factor(origins), fill = factor(origins), colour = factor(origins))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha = 0.5)  +
  facet_wrap(~factor(stud_career_degree_name)) +
  labs(x="Delivered teaching type", y="origins") + 
  scale_fill_discrete(name="origins") + scale_colour_discrete(name="origins") + 
  scale_x_discrete(labels = c("0" = "Face-to-face", "1" = "Hybrid"))
ggsave("Images/per_EP_per_teaching_origins.jpeg", width = 9, height = 6)

ggplot(data, aes(x = factor(hybrid_teaching), y = factor(gender), fill = factor(gender), colour = factor(gender))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha = 0.5)  +
  facet_wrap(~factor(stud_career_degree_name)) +
  labs(x="Delivered teaching type", y="gender")+ 
  scale_fill_discrete(name="gender") + scale_colour_discrete(name="gender") + 
  scale_x_discrete(labels = c("0" = "Face-to-face", "1" = "Hybrid"))
ggsave("Images/per_EP_per_teaching_gender.jpeg", width = 9, height = 6)

ggplot(data, aes(x = factor(hybrid_teaching), y = factor(previous_studies), fill = factor(previous_studies), colour = factor(previous_studies))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), alpha = 0.5) +
  facet_wrap(~factor(stud_career_degree_name)) +
  labs(x="Delivered teaching type", y="previous_studies") + 
  scale_fill_discrete(name="previous_studies") + scale_colour_discrete(name="previous_studies") + 
  scale_x_discrete(labels = c("0" = "Face-to-face", "1" = "Hybrid"))
ggsave("Images/per_EP_per_teaching_previous_studies.jpeg", width = 9, height = 6)


##### Some plots #####

plot_means(data = data[!is.na(data$GPA), ], 
           measurevar = "GPA", var = sym("GPA"))
plot_means(data = data, measurevar = "ECTS", var = sym("ECTS"))
plot_means(data = data, measurevar = "admission_score", var = sym("admission_score"))
plot_means(data = data, measurevar = "career_admission_age", var = sym("career_admission_age"))
plot_means(data = data, measurevar = "highschool_grade", var = sym("highschool_grade"))



