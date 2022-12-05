#summary.object
#' summary.mars
#'
#' @param object of class mars, which is obtained from calling mars()
#' @param digits the number of significant digits i.e. SIG FIG
#'
#' @return
#' @export
#'
#' @examples
#'  mars_object<-mars(y ~.,data=mars::marstestdata)
#' summary(mars_object)
#' @author Tauseef Kashtwari, Promit Chowdhury, Ibraheem Azad
summary.mars <- function(object,...)
{
  "convert coef to dataframe"
  data_frame<-as.data.frame(object$coefficients)
  rownames(data_frame)[1]<- "(Intercept)" #interecept will be located on the rows
  colnames(data_frame)[1]<- "(Estimate coefficients)" #coeffiecients are given on the cols

  print(data_frame)
  summary(object$residuals) #residual summary

  for(i in 2:length(object$Bfuncs))
  {
    cat("Coefficient ",names(object$coefficients)[i],":\n") #run names function to get the coefficient values of each variable
    for(j in 1:nrow(object$Bfuncs[[i]]))
      {
      cat("Hinge function value:",object$x_names[object$Bfuncs[[i]][j,2]],"split at value t:",object$Bfuncs[[i]][j,3],"\n")
    }
  }
}
