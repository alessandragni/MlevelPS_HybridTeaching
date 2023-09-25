
source('Utils_functions/import_libraries.R')

# apply weights to data
weighteddataM<-svydesign(ids = ~ 1, data = data, weights = ~ psMw)
weightedtableM <-svyCreateTableOne(vars = covariateNames, strata = "hybrid_teaching", 
                                   data = weighteddataM, test = FALSE)
SMD_plot_Polimi_ECTS$Weighted_MLM = as.numeric(ExtractSmd(weightedtableM))

weighteddataR<-svydesign(ids = ~ 1, data = data, weights = ~ psRw)
weightedtableR <-svyCreateTableOne(vars = covariateNames, strata = "hybrid_teaching", 
                                   data = weighteddataR, test = FALSE)
SMD_plot_Polimi_ECTS$Weighted_GLMM = as.numeric(ExtractSmd(weightedtableR))

weighteddataM_gr<-svydesign(ids = ~ 1, data = data_grade, weights = ~ psMw)
weightedtableM_gr <-svyCreateTableOne(vars = covariateNames, strata = "hybrid_teaching", 
                                      data = weighteddataM_gr, test = FALSE)
SMD_plot_Polimi_GPA$Weighted_MLM = as.numeric(ExtractSmd(weightedtableM_gr))

weighteddataR_gr<-svydesign(ids = ~ 1, data = data_grade, weights = ~ psRw)
weightedtableR_gr <-svyCreateTableOne(vars = covariateNames, strata = "hybrid_teaching", 
                                      data = weighteddataR_gr, test = FALSE)
SMD_plot_Polimi_GPA$Weighted_GLMM = as.numeric(ExtractSmd(weightedtableR_gr))




# Weighted tables

weightedtable <-svyCreateTableOne(vars = covariateNames, strata = "hybrid_teaching", 
                                  data = weighteddataM, test = FALSE)
print(weightedtable, smd = TRUE) ## Show table with SMD


weightedtable <-svyCreateTableOne(vars = covariateNames, strata = "hybrid_teaching", 
                                  data = weighteddataR, test = FALSE)
print(weightedtable, smd = TRUE) ## Show table with SMD


weightedtable <-svyCreateTableOne(vars = covariateNames, strata = "hybrid_teaching", 
                                  data = weighteddataM_gr, test = FALSE)
print(weightedtable, smd = TRUE) ## Show table with SMD


weightedtable <-svyCreateTableOne(vars = covariateNames, strata = "hybrid_teaching", 
                                  data = weighteddataR_gr, test = FALSE)
print(weightedtable, smd = TRUE) ## Show table with SMD






