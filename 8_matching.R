
source('Utils_functions/import_libraries.R')

analysis = FALSE
comparison_matchit = FALSE

#### WITHOUT REPLACEMENT ####

##### For Grade, Match (M) #####
PS.M.grade <- Match(Y=data_grade$GPA, 
                    Tr=data_grade$hybrid_teaching, 
                    X=data_grade$psM, 
                    estimand = "ATE",
                    M=1,
                    caliper=0.2, 
                    replace=FALSE,
                    ties=TRUE,
                    distance.tolerance = 0)
summary(PS.M.grade)

# Obtain the matched data set
m.M.grade = data_grade[unlist(PS.M.grade[c("index.treated","index.control")]), ]

weightedtableM_gr <- CreateTableOne(vars=covariateNames, strata ="hybrid_teaching", 
                                    data=m.M.grade, test = FALSE)
print(weightedtableM_gr, smd = TRUE)
SMD_plot_Polimi_GPA$Matched_MLM = as.numeric(ExtractSmd(weightedtableM_gr))


# some analysis
if(analysis){
  MatchBalance(formula(ps.formulaM), 
               data=data_grade, 
               match.out=PS.M.grade, nboots=500)
  
  balance.table = bal.tab(PS.M.grade, formula = formula(ps.formulaM), 
                          quick = FALSE, data = data_grade)
  
  love.plot(balance.table, 
            threshold =.1,
            line = TRUE,
            stars = "std")
}


##### For CFU, Match (M) #####
PS.M.cfu <- Match(Y=data$ECTS, 
                  Tr=data$hybrid_teaching, 
                  X=data$psM, 
                  estimand="ATE",
                  caliper=0.2, 
                  replace=FALSE,
                  ties=TRUE,
                  M=1,
                  distance.tolerance = 0)
summary(PS.M.cfu)

# Obtain the matched data set
m.M.cfu = data[unlist(PS.M.cfu[c("index.treated","index.control")]), ]

weightedtableM <- CreateTableOne(vars=covariateNames, strata ="hybrid_teaching", 
                                 data=m.M.cfu, test = FALSE)
print(weightedtableM, smd = TRUE)
SMD_plot_Polimi_ECTS$Matched_MLM = as.numeric(ExtractSmd(weightedtableM))



# some analysis
if(analysis){
  MatchBalance(formula(ps.formulaM), 
               data=data, 
               match.out=PS.M.cfu, nboots=500)
  
  balance.table = bal.tab(PS.M.cfu, formula = formula(ps.formulaM), 
                          quick = FALSE, data = data)
  
  love.plot(balance.table, 
            threshold =.1,
            line = TRUE,
            stars = "std")
}

##### For Grade, Matchby (R) #####
PS.Rby.grade <- Matchby(Y=data_grade$GPA, 
                        Tr=data_grade$hybrid_teaching, 
                        X=data_grade$psR, 
                        by=data_grade$stud_career_degree_name,
                        estimand = "ATE",
                        caliper=0.2, 
                        replace=FALSE,
                        ties=TRUE,
                        M=1,
                        distance.tolerance = 0)
summary(PS.Rby.grade)

# Obtain the matched data set
m.Rby.grade = data_grade[unlist(PS.Rby.grade[c("index.treated","index.control")]), ]

weightedtableR_gr <- CreateTableOne(vars=covariateNames, strata ="hybrid_teaching", 
                                    data=m.Rby.grade, test = FALSE)
print(weightedtableR_gr, smd = TRUE)
SMD_plot_Polimi_GPA$Matched_GLMM = as.numeric(ExtractSmd(weightedtableR_gr))



# some analysis
if(analysis){
  MatchBalance(formula(ps.formulaR), 
               data=data_grade, 
               match.out=PS.Rby.grade, nboots=500)
  
  balance.table = bal.tab(PS.Rby.grade, formula = formula(ps.formulaR), 
                          quick = FALSE, data = data_grade)
  
  love.plot(balance.table, 
            threshold =.1,
            line = TRUE,
            stars = "std")
}



##### For CFU, Matchby (R) #####
PS.Rby.cfu <- Matchby(Y=data$ECTS, 
                      Tr=data$hybrid_teaching, 
                      X=data$psR, 
                      by=data$stud_career_degree_name,
                      estimand = "ATE",
                      caliper=0.2, 
                      replace=FALSE,
                      ties=TRUE,
                      M=1,
                      distance.tolerance = 0)
summary(PS.Rby.cfu)

# Obtain the matched data set
m.Rby.cfu = data[unlist(PS.Rby.cfu[c("index.treated","index.control")]), ]

matchedtab1<-CreateTableOne(vars=covariateNames, strata ="hybrid_teaching", 
                            data=m.Rby.cfu, test = FALSE)
print(matchedtab1, smd = TRUE)
SMD_plot_Polimi_ECTS$Matched_GLMM = as.numeric(ExtractSmd(matchedtab1))



# some analysis
if(analysis){
  MatchBalance(formula(ps.formulaR), 
               data=data, 
               match.out=PS.Rby.cfu, nboots=500)
}  



#### WITH REPLACEMENT ####

comparison_matchit = FALSE

##### For Grade, Match (M) #####
PS.M.grade_repl <- Match(Y=data_grade$GPA, 
                         Tr=data_grade$hybrid_teaching, 
                         X=data_grade$psM, 
                         estimand = "ATE",
                         M=1,
                         caliper=0.2, 
                         replace=TRUE, 
                         ties=TRUE,
                         distance.tolerance = 0)
summary(PS.M.grade_repl)


# Obtain the matched data set
m.M.grade_repl = data_grade[unlist(PS.M.grade_repl[c("index.treated","index.control")]), ]

weightedtableM_gr <- CreateTableOne(vars=covariateNames, strata ="hybrid_teaching", 
                                    data=m.M.grade_repl, test = FALSE)
print(weightedtableM_gr, smd = TRUE)
SMD_plot_Polimi_GPA$Matched_MLM_repl = as.numeric(ExtractSmd(weightedtableM_gr))


##### For CFU, Match (M) #####
PS.M.cfu_repl <- Match(Y=data$ECTS, 
                  Tr=data$hybrid_teaching, 
                  X=data$psM, 
                  estimand="ATE",
                  caliper=0.2, 
                  replace=TRUE,
                  ties=TRUE,
                  M=1,
                  distance.tolerance = 0)
summary(PS.M.cfu_repl)

# Obtain the matched data set
m.M.cfu_repl = data[unlist(PS.M.cfu_repl[c("index.treated","index.control")]), ]

weightedtableM <- CreateTableOne(vars=covariateNames, strata ="hybrid_teaching", 
                                 data=m.M.cfu_repl, test = FALSE)
print(weightedtableM, smd = TRUE)
SMD_plot_Polimi_ECTS$Matched_MLM_repl = as.numeric(ExtractSmd(weightedtableM))


##### For Grade, Matchby (R) #####
PS.Rby.grade_repl <- Matchby(Y=data_grade$GPA, 
                        Tr=data_grade$hybrid_teaching, 
                        X=data_grade$psR, 
                        by=data_grade$stud_career_degree_name,
                        estimand = "ATE",
                        caliper=0.2, 
                        replace=TRUE,
                        ties=TRUE,
                        M=1,
                        distance.tolerance = 0)
summary(PS.Rby.grade_repl)

# Obtain the matched data set
m.Rby.grade_repl = data_grade[unlist(PS.Rby.grade_repl[c("index.treated","index.control")]), ]
weightedtableR_gr <- CreateTableOne(vars=covariateNames, strata ="hybrid_teaching", 
                                    data=m.Rby.grade_repl, test = FALSE)
print(weightedtableR_gr, smd = TRUE)
SMD_plot_Polimi_GPA$Matched_GLMM_repl= as.numeric(ExtractSmd(weightedtableR_gr))




##### For CFU, Matchby (R) #####
PS.Rby.cfu_repl <- Matchby(Y=data$ECTS, 
                      Tr=data$hybrid_teaching, 
                      X=data$psR, 
                      by=data$stud_career_degree_name,
                      estimand = "ATE",
                      caliper=0.2, 
                      replace=TRUE,
                      ties=TRUE,
                      M=1,
                      distance.tolerance = 0)
summary(PS.Rby.cfu_repl)

# Obtain the matched data set
m.Rby.cfu_repl = data[unlist(PS.Rby.cfu_repl[c("index.treated","index.control")]), ]

matchedtab1<-CreateTableOne(vars=covariateNames, strata ="hybrid_teaching", 
                            data=m.Rby.cfu_repl, test = FALSE)
print(matchedtab1, smd = TRUE)
SMD_plot_Polimi_ECTS$Matched_GLMM_repl = as.numeric(ExtractSmd(matchedtab1))



#### CREATE PLOT #####
# Create long-format data for ggplot2
library(reshape2)
dataPlotMelt_ECTS <- melt(data      = SMD_plot_Polimi_ECTS,
                          id.vars       = c("variable"),
                          variable.name = "Method",
                          value.name    = "SMD")

## Order variable names by magnitude of SMD
varNames <- as.character(dataPlotMelt_ECTS$variable)[order(SMD_plot_Polimi_ECTS$Original)]

## Order factor levels in the same order
dataPlotMelt_ECTS$variable <- factor(dataPlotMelt_ECTS$variable,
                                     levels = varNames)

## Plot using ggplot2
pECTS = ggplot(data = dataPlotMelt_ECTS,
               mapping = aes(x = variable, y = SMD, group = Method, color = Method, 
                             linetype = Method, shape = Method)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() +
  theme_bw() +
  theme(legend.key = element_blank(), text = element_text(size=16),
        legend.key.width = unit(5, "line")) +
  guides(
    linetype = guide_legend(override.aes = list(size = 5))  # Adjust size for linetype legend
  )
pECTS = pECTS + theme(legend.position = c(.7, .2)) 
ggsave("Images/loveplot_ECTS.jpeg", plot = pECTS, width = 8, height = 8)


#_______________________________________________________________________________

# Create long-format data for ggplot2
dataPlotMelt_GPA <- melt(data      = SMD_plot_Polimi_GPA,
                         id.vars       = c("variable"),
                         variable.name = "Method",
                         value.name    = "SMD")

## Order variable names by magnitude of SMD
varNames <- as.character(dataPlotMelt_GPA$variable)[order(SMD_plot_Polimi_GPA$Original)]

## Order factor levels in the same order
dataPlotMelt_ECTS$variable <- factor(dataPlotMelt_GPA$variable,
                                     levels = varNames)

## Plot using ggplot2
pGPA = ggplot(data = dataPlotMelt_GPA,
              mapping = aes(x = variable, y = SMD, group = Method, color = Method, 
                            linetype = Method, shape = Method)) +
  geom_line(size=1) +
  geom_point(size=5) +
  geom_hline(yintercept = 0.1, color = "black", size = 0.1) +
  coord_flip() +
  theme_bw() +
  theme(legend.key = element_blank(), text = element_text(size=16),
        legend.key.width = unit(5, "line")) +
  guides(
    linetype = guide_legend(override.aes = list(size = 5))  # Adjust size for linetype legend
  )
pGPA = pGPA + theme(legend.position = c(.7, .2)) 
ggsave("Images/loveplot_GPA.jpeg", plot = pGPA, width = 8, height = 8)




