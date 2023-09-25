##### Functions definition for estimators for the ACD or ATE #####
# ATE estimator as calculated in Section 3.3 of 
# "Propensity score weighting with multilevel data" by Li, Zaslavsky and Landrum

# Marginal estimator
# for bootstrap
compute_marg_est_boot = function(data, i, output, psw){
  data2 <- data[i, ] 
  Y_hk_1= data2[data2$hybrid_teaching==1, output]
  Y_hk_0= data2[data2$hybrid_teaching==0, output]
  
  w_hk_1 = data2[data2$hybrid_teaching==1, psw]
  w_hk_0 = data2[data2$hybrid_teaching==0, psw]
  
  w1 = sum(w_hk_1)
  w0 = sum(w_hk_0)
  
  ATE_Est = (sum(Y_hk_1 * w_hk_1)/w1) - (sum(Y_hk_0 * w_hk_0)/w0)
  return(round(ATE_Est,3))
}

# Cluster-weighted estimator 
# for plot and for returning a df
compute_clust_est = function(data, output, psw){
  # CFU
  data_bl1 = data[data$hybrid_teaching==1, ]
  data_bl0 = data[data$hybrid_teaching==0, ]
  
  tmp1 = data_bl1 %>%
    dplyr::group_by(stud_career_degree_name) %>%
    dplyr::summarize(w1 = weighted.mean(!!output, !!psw),
                     n = n())
  
  tmp0 = data_bl0 %>%
    dplyr::group_by(stud_career_degree_name) %>%
    dplyr::summarize(w0 = weighted.mean(!!output, !!psw),
                     n = n())
  
  tmpA = merge(x = tmp1, y = tmp0, by = "stud_career_degree_name", all = TRUE) %>% 
    mutate(pi_h = w1 - w0)
  
  tmpB = data %>% 
    group_by(stud_career_degree_name) %>% 
    dplyr::summarize(w_h = sum(!!psw))
  
  tmpC = merge(x = tmpA, y = tmpB, by = "stud_career_degree_name", all = TRUE)
  
  pi_cl = as.numeric(tmpC %>% dplyr::summarize(pi_cl = weighted.mean(pi_h, w_h)))
  
  pl1 = ggplot(tmpC) +
    geom_point(aes(x = pi_h, y = stud_career_degree_name), color = "cadetblue4")+ 
    labs(x = TeX(r'($\pi_h$)'),
         y = TeX(r'(Degree Course $h$)'))
  ggsave(paste("Images/ATE_pi_h",output,".jpeg"), plot = pl1, width = 10, height = 8)
  print(pl1)
  
  pl2 = ggplot(tmpC) +
    geom_point(aes(x = w_h, y = stud_career_degree_name), color = "orange")+ 
    labs(x = TeX(r'($w_h$)'),
         y = TeX(r'(Degree Course $h$)'))
  ggsave(paste("Images/ATE_w_h",output,".jpeg"), plot = pl2, width = 10, height = 8)
  print(pl2)
  
  return(tmpC[,c('stud_career_degree_name', 'pi_h', 'w_h')]) 
}

# for bootstrap
compute_clust_est_boot = function(data, i, output, psw){
  data2 <- data[i, ] 
  data_bl1 = data2[data2$hybrid_teaching==1, ]
  data_bl0 = data2[data2$hybrid_teaching==0, ]
  
  tmp1 = data_bl1 %>%
    dplyr::group_by(stud_career_degree_name) %>%
    dplyr::summarize(w1 = weighted.mean(!!output, !!psw),
                     n = n())
  
  tmp0 = data_bl0 %>%
    dplyr::group_by(stud_career_degree_name) %>%
    dplyr::summarize(w0 = weighted.mean(!!output, !!psw),
                     n = n())
  
  tmpA = merge(x = tmp1, y = tmp0, by = "stud_career_degree_name", all = TRUE) %>% 
    mutate(pi_h = w1 - w0)
  
  tmpB = data %>% 
    group_by(stud_career_degree_name) %>% 
    dplyr::summarize(w_h = sum(!!psw))
  
  tmpC = merge(x = tmpA, y = tmpB, by = "stud_career_degree_name", all = TRUE)
  
  pi_cl = as.numeric(tmpC %>% dplyr::summarize(pi_cl = weighted.mean(pi_h, w_h)))
  
  return(round(pi_cl,3))
}


construct_header <- function(df, grp_names, span, align = "c", draw_line = T) {
  if (length(align) == 1) align <- rep(align, length(grp_names))
  if (!all.equal(length(grp_names), length(span), length(align)))
    stop("grp_names and span have to have the same length!")
  
  if (ncol(df) < sum(span)) stop("Span has to be less or equal to the number of columns of df") 
  
  header <- mapply(function(s, a, grp) sprintf("\\multicolumn{%i}{%s}{%s}", s, a, grp),
                   span, align, grp_names)
  header <- paste(header, collapse = " & ")
  header <- paste0(header, " \\\\")
  
  if (draw_line) {
    # where do we span the lines:
    min_vals <- c(1, 1 + cumsum(span)[1:(length(span) - 1)])
    max_vals <- cumsum(span)
    line <- ifelse(grp_names == "", "", 
                   sprintf("\\cmidrule(lr){%i-%i}", min_vals, max_vals))
    line <- paste(line[line != ""], collapse = " ")
    
    header <- paste0(header, "  ", line, "\n  ")
  }
  
  addtorow <- list(pos = list(-1, -1, nrow(df)),
                   command = c("\\hline\n  ", header, "\\hline\n  "))
  return(addtorow)
}

