





#' Title
#'
#' @param y Vector of the outcome variable or dependent variable
#' @param X Matrix of the independent variables.
#' @param model.type Specifies whether the function calculates MLE parameter estimates and associated standard errors for
#' logistic (link logit) or linear models
#'
#' @return Returns a matrix of parameter estimates and associated standard errors
#' @export
#'
#' @examples
#' model.linear <- ll.models(y=y, X=X, model.type='linear')
#' model.logistic <- ll.models(y=y, X=X, model.type='logistic')
ll.models <- function(y, X, model.type) {

  if (model.type == 'linear') {
    linmod.llik <- function(theta, y, X) {
      N <- nrow(X) # number of observations
      k <- ncol(X) # number of parameters
      # subset parameter vector
      beta.hat <- theta[1:k]
      gamma.hat <- theta[k + 1]
      # ensure sigma2 to be positive
      sigma2.hat <- exp(gamma.hat)
      # calculate residuals
      e <- y - X %*% beta.hat
      # write down log-likelihood
      llik <- -1/2 * N * log(2 * pi) - 1/2 * N * log(sigma2.hat) - (t(e) %*% e) / (2 * sigma2.hat)
      # return log-likelihood
      return(llik)
    }
    inits <- c(0, rep(0, ncol(X))) # define starting values
    # results assignment moved outside of the function definition
    results <- optim(inits, linmod.llik, y = y, X = X,
                     control = list(fnscale = -1),
                     method = "BFGS",
                     hessian = TRUE)
  } else if (model.type == 'logistic') {
    ll.logit <- function(theta, y, X) {
      # theta consists merely of beta (dim is ncol(X))
      beta <- theta[1:ncol(X)]
      # linear predictor; make sure that X is stored as.matrix
      mu <- X %*% beta
      # link function
      p <- 1 / (1 + exp(-mu))
      # log-likelihood
      ll <- y * log(p) + (1 - y) * log(1 - p)
      # sum
      ll <- sum(ll)
      return(ll)
    }
    inits <- c(rep(0, ncol(X))) # # define starting values
    # results assignment moved outside of the function definition
    results <- optim(inits, ll.logit, y = y, X = X,
                     control = list(fnscale = -1),
                     method = "BFGS",
                     hessian = TRUE)
  } else {
    stop('Please specify the correct model! Currently only Linear and Logistic (link=logit) models are supported')
  }

  betas <- results$par[1:ncol(X)]
  std.err <- sqrt(diag(-solve(results$hessian)))[1:ncol(X)]
  print(cbind(betas, std.err))
}

