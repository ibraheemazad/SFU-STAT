"see lab 10 and testpredict - below is from lab 10 (object is MARS object)"
#' Predict function for mars
#'
#' @param object
#' @param newdata
#'
#' @return
#' @export
#'
predict.mars <- function(object,newdata) #renamed so that predict.mars for test
{
  #NEWDATA=NULL uses TRAINING DATA
  if(missing(newdata) || is.null(newdata)){
    B <- as.matrix(object$B)
  }

  #validation data
  else
  {
    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf,"terms")
    X <- model.matrix(mt,mf)[,-1] #remove intercept

    "need to write make_B"
    B <- make_B(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop(B %*% beta)
}

#' Make_B
#'
#' @param B
#' @param Bfuncs
#'
#' @return output_x
#' @export
#'
make_B <- function(X,Bfuncs) #use B
{
  len <- length(Bfuncs) #added this forgot it
  output <- init_B(nrow(X), len-1)
  for(i in 2:length(Bfuncs))
    "changed index error - 5pm"
  {
    temp_value <- 1
    for(j in 1:nrow(Bfuncs[[i]]))
    {
      temp_value <- temp_value*h(X[,Bfuncs[[i]][j,"v"]], Bfuncs[[i]][j,"s"], Bfuncs[[i]][j,"t"])
    }
  output[,i] <- temp_value
  }
  return (as.matrix(output))
}
