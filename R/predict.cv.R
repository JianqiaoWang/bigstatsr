
#' Predict method
#'
#' Predict method for class `big_sp` after cross-validation
#'
#' @param object Object of class `big_sp_list`.
#' @inheritParams bigstatsr-package
#' @param ... Not used.
#' @param proba Whether to return probabilities?
#' @param base.row Vector of base predictions, corresponding to `ind.row`.
#'
#' @return predicted response
#'
#' @export

predict.cv <- function(object, X,
                       ind.row = rows_along(X),
                       ind.col = attr(object, "ind.col"),
                       covar.row = NULL,
                       proba = (attr(object, "family") == "binomial"),
                       base.row = NULL,
                       ncores = 1
                       ) {

  beta <- object$beta

  if (is.null(covar.row)) {

    ind.nozero <- which(beta != 0)
    scores <- big_prodVec(X, beta[ind.nozero], ind.row = ind.row,
                          ind.col = ind.col[ind.nozero], ncores = ncores)
  } else {

    assert_lengths(ind.row, rows_along(covar.row))

    ind.X <- seq_along(ind.col)
    ind.nozero <- which(beta[ind.X] != 0)
    scores <- big_prodVec(X, beta[ind.nozero], ind.row = ind.row,
                          ind.col = ind.col[ind.nozero], ncores = ncores) +
      drop(covar.row %*% beta[-ind.X])
  }

  names(scores) <- ind.row
  intercept = object$intercept

  if(attr(object, "family") == "gaussian"){
    pred = linear(intercept + scores)
  }

  if(attr(object, "family") == "binomial"){
    pred = expit(intercept + scores)
  }

  return(pred)
}


expit = function(x){
  exp(x)/(1 + exp(x))
}

linear = function(x){
  return(x)
}
