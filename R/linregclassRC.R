

#' Title
#'
#' @field coefficients matrix. 
#' @field fittedvalues matrix. 
#' @field residuals matrix. 
#' @field df numeric. 
#' @field residualvariance numeric. 
#' @field varianceregcoe matrix. 
#' @field tvalues matrix. 
#' @field pvalue matrix. 
#' @field formula character. 
#' @field data character. 
#' @field star character. 
#'
#' @export linreg 
#' @exportClass linreg
#' @importFrom methods new setRefClass
#' @import ggplot2
linreg <- setRefClass("linreg", fields = list( coefficients="matrix",
                                               fittedvalues="matrix",
                                               residuals="matrix",
                                               df="numeric",
                                               residualvariance="numeric",
                                               varianceregcoe="matrix",
                                               tvalues="matrix",
                                               pvalue="matrix",
                                               formula="character",
                                               data="character",
                                               star = "character"
)


)


linreg$methods(initialize = function( formula,data){
  X<- model.matrix(formula,data)
  y<- all.vars(formula)[1]
  Y<- data[,y]
  n=dim(X)[1]
  p=dim(X)[2]
  .self$formula <- deparse(formula)
  .self$data <- deparse(substitute(data))
  
  .self$coefficients <- solve(t(X) %*% X) %*% t(X) %*% Y
  .self$fittedvalues <- X %*% coefficients
  .self$residuals <- Y - fittedvalues
  .self$df <- n-p
  .self$ residualvariance<- as.numeric(t(residuals)%*% residuals/df)
  varmat<- residualvariance * solve(t(X) %*% X)
  
  .self$varianceregcoe <- matrix(diag(varmat), nrow=nrow(coefficients))
  rownames(varianceregcoe)<<- rownames(varmat)
  .self$tvalues<- coefficients/sqrt(varianceregcoe)
  .self$pvalue<- pt(coefficients,df)
  
},
#show= function(){
#cat("call:\n")
#cat("linreg(formula= ", formula, ", data =" ,data,")  \n  \n")
#cat(colnames(matrix(coefficients)),"\n",matrix(coefficients),"\n"  )
#cat(coefficients,"\n")
print = function(){
  # cat("call:\n")
  cat("linreg(formula = ",formula,", data = ",data,")",sep="")
  cat("\n\n",rownames(coefficients), "\n")
  cat( coefficients, "\n")
  
  
},
resid= function(){
  return(as.vector(residuals))
},
pred= function(){
  return(fittedvalues)
},
coef= function(){
  coef<- as.vector(coefficients)
  
  return(coef)
}, 
plot = function(){
  plot1 <- ggplot2::ggplot(mapping = ggplot2::aes(x = fittedvalues ,y=residuals))+
    ggplot2::geom_point(shape =1 , size =3)+
    ggplot2::stat_summary(fun="median", colour= "red" , geom = "line")+
    ggplot2::labs(title = "Residuals vs Fitted")
  plot1
  plot2 <- ggplot2::ggplot(mapping = ggplot2::aes(x = fittedvalues ,y=(sqrt(abs(residuals/sqrt(residualvariance))))))+
    ggplot2::geom_point(shape =1 , size =3)+
    ggplot2::stat_summary(fun="mean", colour= "red" , geom = "line")+
    ggplot2::labs(title = "Scale-Location")
  
  plots <- list(plot1 , plot2)
  plots
},
summary= function(){
  for(i in 1:length(coefficients)){
    star[i] <<- "***"
  }
  
  summarytable <- data.frame("Coefficients" = coefficients, "Standard error" = sqrt(varianceregcoe), "T Values" = tvalues, "P Values"= pvalue, "stars" = star)
  names(summarytable)[5] <- " "
  print.data.frame(summarytable)
  cat(paste("\nResidual standard error: ", sqrt(residualvariance), " on ", df, " degrees of freedom", sep = ""))
}
)
# data("iris")
test <- linreg(Petal.Length~Species, iris)
# ttt <- linreg(Sepal.Length~Species, iris)
test$print()
# test$residual()
# test$fittedvalues
# test$pred()
# test$coef()
# test$coefficients
# test$pvalue
# 
# test$summary()
# data(iris)
#mod_object <- lm(Petal.Length~Species, data = iris)
#mod_object$summary(lm(Petal.Length~Species, data = iris))
#summary(mod_object)
#test$X
