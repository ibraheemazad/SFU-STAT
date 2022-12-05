#' @description Predict with an mars model for new data, returns the predicted basis function.
#'
#'
#' @param object MARS object.For example, object mars(waiting~eruption, data  = datasets:faithful)
#' @param newdata Make predictions using a data frame.
#'
#' @return
#' @export
#' @seealso [make_B]
#' @examples
#' predict
#' @usage
#' predict.mars(object = (mars object), newdata = data)
#' @details The function returns predicted values based on MARS conditions.
#' @author Tauseef Kashtwari, Promit Chowdhury, Ibraheem Azad
predict.mars <- function(object,newdata) {
  if(missing(newdata) || is.null(newdata)) {
    B <- as.matrix(object$B)
  }
  else {
    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)[,-1] # remove intercept
    B <- make_B(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop(B %*% beta)
}

#----------------------Constructor---------------------#
make_B <- function(X, Bfuncs) {
  size <- length(Bfuncs); out <- init_B(nrow(X), size-1)
  for (i in 2:size) {
    sub <- 1
    for (j in 1:nrow(Bfuncs[[i]])) {
      sub <- sub*h(X[,Bfuncs[[i]][j,"v"]], Bfuncs[[i]][j,"s"], Bfuncs[[i]][j,"t"])
    }
    out[,i] <- sub
  }
  return(as.matrix(out))
}
