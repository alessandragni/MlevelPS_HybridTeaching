
source('Utils_functions/import_libraries.R')
source('Utils_functions/univariate_box_cox.R')

xvars<-c("admission_score","career_admission_age",
         "highschool_grade","gender",
         "previous_studies","origins")
data$admission_score = as.numeric(data$admission_score)
data$highschool_grade = as.numeric(data$highschool_grade)

# Create a dataset for making analysis for GPA
data_grade = data[which(data$GPA!=0),]

# SMDs on data
table1<- CreateTableOne(vars=xvars, strata=c("hybrid_teaching") , data=data, test=FALSE)
print(table1,smd=TRUE)

SMD_plot_Polimi_ECTS <- data.frame(variable   = rownames(ExtractSmd(table1)),
                                   Original  = as.numeric(ExtractSmd(table1)))

# SMDs on data_grade
table1<- CreateTableOne(vars=xvars, strata=c("hybrid_teaching") , data=data_grade, test=FALSE)
print(table1,smd=TRUE)

SMD_plot_Polimi_GPA <- data.frame(variable   = rownames(ExtractSmd(table1)),
                                  Original  = as.numeric(ExtractSmd(table1)))

# Create ECTS_cat
g = 4
table(cut2(data$ECTS, g=g)) # g = quantile groups
data$ECTS_cat = as.factor(as.numeric(cut2(data$ECTS, g=g)))

ggplot(data, aes(x = ECTS)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(x = "ECTS", y = "Frequency", title = "Histogram of ECTS")
ggsave("Images/ECTS_histogram.jpeg", width = 6, height = 5)

ggplot(data, aes(x = ECTS_cat)) +
  geom_bar(fill = "grey", color = "black") +
  labs(x = "ECTS_cat", y = "Count", title = "Bar Plot of ECTS_cat") + 
  scale_x_discrete(labels = c("1" = "[0,9)", "2" = "[9,21)", "3" = "[21,29)", "4" = "[29,50]"))
ggsave("Images/ECTS_cat_bar_plot.jpeg", width = 6, height = 5)


data_grade$GPA = univariate_box_cox(data_grade$GPA)
# --> can be assumed to be gaussian distributed


