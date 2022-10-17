ridgereg_model <- function(){
  rr <- list(type = "Regression",
             library = "BonusLab",
             loop = NULL)

  prm <- data.frame(parameter = "lambda",
                    class = "numeric",
                    label = "Lambda")

  rr$parameters <- prm

  rrGrid <- function(x, y, len = NULL, search = "grid") {
    if(search == "grid") {
      out <- expand.grid(lambda = 1*10**(c(1:10)))
    } else {
      stop('random search not yet implemented')
    }
    return(out)
  }

  rr$grid <- rrGrid


  rrFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    library(BounsLab)
    dat <- if(is.data.frame(x)) x else as.data.frame(x)
    dat$.outcome <- y
    ridgereg(.outcome ~ ., data = dat, lambda = param$lambda, ...)
  }

  rr$fit <- rrFit

  rr$levels <- function(x) x@levels

  rrPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    library(BounsLab)
    ridgereg$predict(newdata)
  }

  rr$predict <- rrPred

  rrProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    library(BounsLab)
    ridgereg$predict(newdata, type = "prob")
    warning("Function not applicable for ridgereg!")
  }

  rr$prob <- rrProb

  rrSort <- function(x) x[order(x$lambda),]

  rr$sort <- rrSort

  return(rr)
}



ridgereg_model <- function(){
  rr <- list(type = "Regression",
             library = "BonusLab",
             loop = NULL)
  
  prm <- data.frame(parameter = "lambda",
                    class = "numeric",
                    label = "Lambda")
  
  rr$parameters <- prm
  
  rrGrid <- function(x, y, len = NULL, search = "grid") {
    if(search == "grid") {
      out <- expand.grid(lambda = 1*10**(c(1:10)))
    } else {
      stop('random search not yet implemented')
    }
    return(out)
  }
  
  rr$grid <- rrGrid
  
  
  rrFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    library(BonusLab)
    dat <- if(is.data.frame(x)) x else as.data.frame(x)
    dat$.outcome <- y
    ridgereg(.outcome ~ ., data = dat, lambda = param$lambda, ...)
  }
  
  rr$fit <- rrFit
  
  rr$levels <- function(x) x@levels
  
  rrPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    modelFit$predict(newdata)
  }
  
  rr$predict <- rrPred
  
  rrProb <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    modelFit$predict(newdata, type = "prob")
    warning("Function not applicable for ridgereg!")
  }
  
  rr$prob <- rrProb
  
  rrSort <- function(x) x[order(x$lambda),]
  
  rr$sort <- rrSort
  
  return(rr)
}
