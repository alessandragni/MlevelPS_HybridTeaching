plot_means = function(data, measurevar, var){
  plot_means_df <- summarySE(data, measurevar = measurevar, 
                             groupvars = c("hybrid_teaching", "stud_career_degree_name", na.rm=TRUE))
  p = ggplot(plot_means_df, aes(x = !!var, y = stud_career_degree_name, color = as.factor(hybrid_teaching)))+
    geom_errorbar(aes(xmin = !!var - ci, xmax = !!var + ci))+ # 95% confidence interval length
    scale_color_discrete(name = "Delivered teaching type", 
                         labels = c("Face-to-face", "Hybrid")) +
    xlab(measurevar)+
    ylab("Degree Course")+
    geom_point(size = 2)+ theme(
      axis.title.x = element_text(size=15),
      axis.title.y = element_text(size=15),
      legend.title = element_text(size=14),
      legend.text = element_text(size=12), 
      axis.text.x = element_text(size=14),
      axis.text.y = element_text(size=14),
      legend.position = "top") 
  ggsave(paste("Images/means_", measurevar, ".jpeg", sep=""), plot = p, width = 8, height = 5)
  return(p)
  
}
