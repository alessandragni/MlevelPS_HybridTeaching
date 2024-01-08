
source('Utils_functions/import_libraries.R')
source('Utils_functions/marginal_clustered_estimators.R')

###### ACD or ATE COMPUTATION ######

R = 500


##### Create a table for the estimator (sd) #####
df <- matrix(NA, nrow=2, ncol = 4) 
df <- cbind(c("M",  "R"), df)
colnames(df) <- c("PS Model", rep(c("Marginal", "Cluster-weighted"), 2))
df

a_header <- construct_header(
  # the data.frame or matrix that should be plotted  
  df,
  # the labels of the groups that we want to insert
  grp_names = c("", "GPA", "ECTS"), 
  # the number of columns each group spans
  span = c(1, 2, 2), 
  # the alignment of each group, can be a single character (lcr) or a vector
  align = "c"
)


##### Marginal estimator #####

# first two columns are for GPA (data_grade)
set.seed(2)
b = boot(data = data_grade, statistic = compute_marg_est_boot, R = R, 
         strata = factor(data_grade$stud_career_degree_name), 
         output = 'GPA', psw = 'psMw')
df[1,2] = paste(b$t0, '(',round(sd(b$t),3),')')


set.seed(2)
b = boot(data = data_grade, statistic = compute_marg_est_boot, R = R, 
         strata = factor(data_grade$stud_career_degree_name), 
         output = 'GPA', psw = 'psRw')
df[2,2] = paste(b$t0, '(',round(sd(b$t),3),')')


# second two columns are for ECTS (data)
set.seed(2) 
b = boot(data = data, statistic = compute_marg_est_boot, R = R, 
         strata = factor(data$stud_career_degree_name), 
         output = 'ECTS', psw = 'psMw')
df[1,4] = paste(b$t0, '(',round(sd(b$t),3),')')

set.seed(2)
b = boot(data = data, statistic = compute_marg_est_boot, R = R, 
         strata = factor(data$stud_career_degree_name), 
         output = 'ECTS', psw = 'psRw')
df[2,4] = paste(b$t0, '(',round(sd(b$t),3),')')

df

##### Clustered estimator #####

# first two columns are for GPA (data_grade)
set.seed(2)
b = boot(data = data_grade, statistic = compute_clust_est_boot, R = R, 
         strata = factor(data_grade$stud_career_degree_name), 
         output = sym('GPA'), psw = sym('psMw'))
df[1,3] = paste(b$t0, '(',round(sd(b$t),3),')')

set.seed(2)
b = boot(data = data_grade, statistic = compute_clust_est_boot, R = R, 
         strata = factor(data_grade$stud_career_degree_name), 
         output = sym('GPA'), psw = sym('psRw'))
df[2,3] = paste(b$t0, '(',round(sd(b$t),3),')')


# second two columns are for ECTS (data)
set.seed(2) 
b = boot(data = data, statistic = compute_clust_est_boot, R = R, 
         strata = factor(data$stud_career_degree_name), 
         output = sym('ECTS'), psw = sym('psMw'))
df[1,5] = paste(b$t0, '(',round(sd(b$t),3),')')


set.seed(2)
b = boot(data = data, statistic = compute_clust_est_boot, R = R, 
         strata = factor(data$stud_career_degree_name), 
         output = sym('ECTS'), psw = sym('psRw'))
df[2,5] = paste(b$t0, '(',round(sd(b$t),3),')')

df

# PRINT THE TABLE 
# print(xtable(df), add.to.row = a_header, include.rownames = F, hline.after = F)


#### FOR THE PLOTS ####
df.M.GPA = compute_clust_est(data = data_grade, output = sym('GPA'), psw = sym('psMw'))
df.R.GPA = compute_clust_est(data = data_grade, output = sym('GPA'), psw = sym('psRw'))

df.M.CFU = compute_clust_est(data = data, output = sym('ECTS'), psw = sym('psMw'))
df.R.CFU = compute_clust_est(data = data, output = sym('ECTS'), psw = sym('psRw'))

df_h = cbind(df.M.GPA,
             df.R.GPA[,2:3],
             df.M.CFU[,2:3],
             df.R.CFU[,2:3]) 
colnames(df_h) = c('stud_career_degree_name',
                   'M - GPA - pi_h',
                   'M - GPA - w_h',
                   'R - GPA - pi_h',
                   'R - GPA - w_h',
                   'M - CFU - pi_h',
                   'M - CFU - w_h',
                   'R - CFU - pi_h',
                   'R - CFU - w_h')
df_h


# PLOTS

plGPA_pi_h = ggplot(df_h) +
  geom_point(aes(x = !!sym('M - GPA - pi_h'), y = stud_career_degree_name, shape = "M", color = "M"), size = 7, alpha = 0.6) +
  geom_point(aes(x = !!sym('R - GPA - pi_h'), y = stud_career_degree_name, shape = "R", color = "R"), size = 7, alpha = 0.6) +
  labs(x = TeX(r'($\pi_h$)'),
       y = TeX(r'(Degree Course $h$)'),
       title = 'GPA') +
  scale_shape_manual(values = c("M" = 16, "R" = 18), labels = c("MLM", "GLMM"), name = "PS Model") +
  scale_color_manual(values = c("M" = "blue", "R" = "red"), labels = c("MLM", "GLMM"), name = "PS Model") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text = element_text(size = 16),  # Set the size of axis text
        axis.title = element_text(size = 18),  # Set the size of axis titles)
        legend.text = element_text(size = 17),  # Set the size of legend text
        legend.title = element_text(size = 17),  # Set the size of legend title
        plot.title = element_text(size = 18))   # Set the size of plot title
print(plGPA_pi_h)
ggsave("Images/EP_GPA_pi_h.jpeg", plot = plGPA_pi_h, width = 8, height = 8)


plGPA_w_h = ggplot(df_h) +
  geom_point(aes(x = !!sym('M - GPA - w_h'), y = stud_career_degree_name, shape = "M", color = "M"), size = 7, alpha = 0.6) +
  geom_point(aes(x = !!sym('R - GPA - w_h'), y = stud_career_degree_name, shape = "R", color = "R"), size = 7, alpha = 0.6) +
  labs(x = TeX(r'($w_h$)'),
       y = TeX(r'(Degree Course $h$)'),
       title = 'GPA') +
  scale_shape_manual(values = c("M" = 16, "R" = 18), labels = c("MLM", "GLMM"), name = "PS Model") +
  scale_color_manual(values = c("M" = "blue", "R" = "red"), labels = c("MLM", "GLMM"), name = "PS Model") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text = element_text(size = 16),  # Set the size of axis text
        axis.title = element_text(size = 18),  # Set the size of axis titles)
        legend.text = element_text(size = 17),  # Set the size of legend text
        legend.title = element_text(size = 17),  # Set the size of legend title
        plot.title = element_text(size = 18))   # Set the size of plot title
print(plGPA_w_h)
ggsave("Images/EP_GPA_w_h.jpeg", plot = plGPA_w_h, width = 8, height = 8)



plCFU_pi_h = ggplot(df_h) +
  geom_point(aes(x = !!sym('M - CFU - pi_h'), y = stud_career_degree_name, shape = "M", color = "M"), size = 7, alpha = 0.6) +
  geom_point(aes(x = !!sym('R - CFU - pi_h'), y = stud_career_degree_name, shape = "R", color = "R"), size = 7, alpha = 0.6) +
  labs(x = TeX(r'($\pi_h$)'),
       y = TeX(r'(Degree Course $h$)'),
       title = 'ECTS') +
  scale_shape_manual(values = c("M" = 16, "R" = 18), labels = c("MLM", "GLMM"), name = "PS Model") +
  scale_color_manual(values = c("M" = "blue", "R" = "red"), labels = c("MLM", "GLMM"), name = "PS Model") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text = element_text(size = 16),  # Set the size of axis text
        axis.title = element_text(size = 18),  # Set the size of axis titles)
        legend.text = element_text(size = 17),  # Set the size of legend text
        legend.title = element_text(size = 17),  # Set the size of legend title
        plot.title = element_text(size = 18))   # Set the size of plot title
print(plCFU_pi_h)
ggsave("Images/EP_CFU_pi_h.jpeg", plot = plCFU_pi_h, width = 8, height = 8)


plCFU_w_h <- ggplot(df_h) +
  geom_point(aes(x = !!sym('M - CFU - w_h'), y = stud_career_degree_name, shape = "M", color = "M"), size = 7, alpha = 0.6) +
  geom_point(aes(x = !!sym('R - CFU - w_h'), y = stud_career_degree_name, shape = "R", color = "R"), size = 7, alpha = 0.6) +
  labs(x = TeX(r'($w_h$)'),
       y = TeX(r'(Degree Course $h$)'),
       title = 'ECTS') +
  scale_shape_manual(values = c("M" = 16, "R" = 18), labels = c("MLM", "GLMM"), name = "PS Model") +
  scale_color_manual(values = c("M" = "blue", "R" = "red"), labels = c("MLM", "GLMM"), name = "PS Model") +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text = element_text(size = 16),  # Set the size of axis text
        axis.title = element_text(size = 18),  # Set the size of axis titles)
        legend.text = element_text(size = 17),  # Set the size of legend text
        legend.title = element_text(size = 17),  # Set the size of legend title
        plot.title = element_text(size = 18))   # Set the size of plot title
print(plCFU_w_h)
ggsave("Images/EP_CFU_w_h.jpeg", plot = plCFU_w_h, width = 8, height = 8)

