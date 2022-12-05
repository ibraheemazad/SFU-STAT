#' print.mars
#'
#' @description PRINT METHOD MARS OBJECT
#' @usage
#' @return values of the coefficeints for MARS
#'
#' @details Prints intercept and coeffcient of mars object
#'
#' @examples
print.mars <- function(marsobject,...) #marsobject
{
  #-------coeffcients separately
  print(marsobject$call)
  print(coefficients(marsobject))
  print("NOW GONNA TRY TO PRINT THE MODEL\n")


  #---------------------MODEL PRINTED BELOW
  model = "Y = B0"
  for(i in 2:length(marsobject$coefficients))
  {
      #use paste
      model = paste(model, names(marsobject$coefficients)[i], sep="+") #take ith object separated by + e.g. Y = B0+B1X+B2X
  }

  print("The model is: ", model)

  for(k in 1:length(marsobject$coefficients)){
    cat(marsobject$coefficients[k], "is the coefficient of",names(marsobject$coefficients)[k],"\n" ) #coef followed by name
  }


}
