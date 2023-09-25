# Univariate Box-Cox transformation for GPA
univariate_box_cox = function(x){
  # We compute the optimal lambda of the univariate Box-Cox transformation
  # (command powerTransform()) #notice that the data has to be positive. 
  # If they are negative, I have to translate them before using power transform
  lambda.x <- powerTransform(x) 
  # lambda<1: observations <1 are "spread", observations >1 are "shrinked"
  print(lambda.x)
  # Transformed sample with the optimal lambda (command bcPower())
  bc.x <- bcPower(x, lambda.x$lambda)      
  # it transforms the data of the first argument through the lambda given as second argument
  return (bc.x)
}