#' Bonus Lab
#'
#' Using Ridge regression to improve lab 4
#'
#' @docType package
#'  
#' @author Kyriakos Papadopoulos  \email{kyrpa853@student.liu.se} YiHung Chen  \email{yihch883@student.liu.se}
#' @name lab6
#' @import dplyr
#' @import caret
#' @import nycflights13
#' @import mlbench
#' @import leaps
NULL

#' mtcars
#'
#' A Graph(data.frame) contain boston housing info


"mtcars"



library(dplyr)
library(caret)
library(nycflights13)
library(mlbench)
library(leaps)

data("BostonHousing")
#'  Ridge Regression
#' 
#'  Run Linear Regression 
#'  
#' @param formula, The formula
#' @param data, The data, iris  
#' @field formula, formula
#' @field data, data
#' @field x, x
#' @field y, y
#' @field regressions_coefficients, regressions_coefficients
#' @field fitted_values, fitted_values
#' @field the_residuals, the_residuals
#' @field n, n
#' @field p, p
#' @field the_degrees_of_freedom, the_degrees_of_freedom
#' @field the_residual_variance, the_residual_variance
#' @field the_variance_of_the_regression_coefficients, the_variance_of_the_regression_coefficients
#' @field standard_error, standard_error
#' @field sigma1, sigma1
#' @field t_values, t_values
#' @field p_values, p_values
#' @field formula_name, formula_name
#' @field data_name, data_name
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs ylab xlab theme element_text
#' @import methods
#' @export ridgereg

# Defining the class ridgereg  
ridgereg <- setRefClass("ridgereg", 
                        fields = list(formula="formula",
                                      data="data.frame",
                                      x = "matrix",
                                      y = "matrix",
                                      regressions_coefficients = "matrix",
                                      fitted_values = "matrix",
                                      the_residuals = "matrix",
                                      n = "numeric",
                                      p = "numeric",
                                      the_degrees_of_freedom = "numeric",
                                      the_residual_variance = "matrix",
                                      the_variance_of_the_regression_coefficients = "numeric",
                                      standard_error = "numeric",
                                      sigma1 = "matrix",
                                      t_values = "matrix", 
                                      p_values = "matrix",
                                      formula_name = "character",
                                      data_name = "character",
                                      lambda = "numeric",
                                      ridge_regressions_coefficients = "matrix"
                        ),
                        
                        methods = list(
                          
                          initialize = function(formula, data,lambda) {
                            # Finding x and y
                            x <<- model.matrix(formula, data)
                            y <<- as.matrix(data[all.vars(formula)[1]]) #as.vector does not work
                            
                            # Finding regressions coefficients
                            regressions_coefficients <<- solve( t(x)%*%x+lambda*diag(ncol(x))) %*% t(x)%*%y
                            # Finding the fitted values
                            fitted_values <<- x %*% regressions_coefficients
                            
                            # Finding the residuals
                            the_residuals <<- y - fitted_values
                            
                            # Finding the degrees of freedom
                            n <<- nrow(x)
                            p <<- ncol(x)
                            the_degrees_of_freedom <<- n - p
                            
                            # Finding the residual variance
                            the_residual_variance <<-t(the_residuals) %*% the_residuals / the_degrees_of_freedom
                            
                            # Finding the variance of the regression coefficients
                            the_variance_of_the_regression_coefficients <<-diag(drop(the_residual_variance) * solve((t(x) %*% x))) #use diag to eliminate unecessory covariances
                            
                            standard_error <<-sqrt(the_variance_of_the_regression_coefficients)
                            sigma1 <<- sqrt(the_residual_variance)
                            
                            # Finding the t values for each coefficient
                            t_values <<-regressions_coefficients / sqrt(the_variance_of_the_regression_coefficients)
                            p_values <<-2 * pt(abs(t_values), the_degrees_of_freedom, lower.tail = FALSE)
                            
                            formula_name <<- format(formula)
                            data_name <<- deparse(substitute(data))
                            
                          },
                          
                          print = function(){
                            
                            cat(paste("ridgereg(formula = ",  formula_name, ", data = ", data_name , ")", sep = ""))
                            print_mask(drop(regressions_coefficients))
                            
                          },
                          
                          
                          medians_of_resdiuals = function(){
                            first_species_x <- median(fitted_values[1:50])
                            second_species_x <- median(fitted_values[51:100])
                            third_species_x <- median(fitted_values[101:150])
                            
                            first_species_y <- rep(median(the_residuals[1:50]), 50)
                            second_species_y <- rep(median(the_residuals[51:100]), 50)
                            third_species_y <- rep(median(the_residuals[101:150]), 50)
                            
                            medians_data <- data.frame(x = c(first_species_x,second_species_x,third_species_x) , y = c(first_species_y, second_species_y, third_species_y))
                            
                            return( medians_data)
                          },
                          
                          medians_of_standerdized_residuals = function(){
                            first_species_x <- median(fitted_values[1:50])
                            second_species_x <- median(fitted_values[51:100])
                            third_species_x <- median(fitted_values[101:150])
                            
                            sqrt_standardized_residuals <- sqrt(abs(the_residuals / sd(the_residuals)))
                            first_species_y <- rep(median(sqrt_standardized_residuals[1:50]), 50)
                            second_species_y <- rep(median(sqrt_standardized_residuals[51:100]), 50)
                            third_species_y <- rep(median(sqrt_standardized_residuals[101:150]), 50)
                            
                            medians_data <- data.frame(x = c(first_species_x,second_species_x,third_species_x) , y = c(first_species_y, second_species_y, third_species_y))
                            
                            return( medians_data)
                          },
                          
                          plot = function(){
                            median_values_x <- medians_of_resdiuals()[1]
                            median_values_y <- medians_of_resdiuals()[2]
                            median_values_y2 <- medians_of_standerdized_residuals()[2]
                            sqrt_standardized_residuals <- sqrt(abs(the_residuals / sd(the_residuals)))
                            test <- sqrt(abs(the_residual_variance))
                            testdata <- data.frame(fitted_values, the_residuals, test, median_values_x, median_values_y, median_values_y2, sqrt_standardized_residuals)
                            
                            plot1 <- ggplot() +
                              geom_point(data=testdata, mapping =  aes(x=fitted_values, y= the_residuals)) +
                              geom_line(data=testdata, mapping =  aes(x=fitted_values, y= unlist(median_values_y)), color = "red")
                            
                            plot1 <- plot1 +  labs(title = "Residuals vs Fitted") +
                              theme(plot.title =  element_text(hjust = 0.5)) +  ylab("Residuals") +  xlab("Fitted values")
                            
                            plot2 <-  ggplot() +
                              geom_point(data=testdata, mapping =  aes(x=fitted_values, y= unlist(sqrt_standardized_residuals))) +
                              geom_line(data=testdata, mapping =  aes(x=fitted_values, y= unlist(median_values_y2)), color = "red")
                            
                            plot2 <- plot2 +  labs(title = "Scale-Location") +
                              theme(plot.title =  element_text(hjust = 0.5)) +  ylab( expression(paste(sqrt("Standerdized residuals")))) +  xlab("Fitted values")
                            
                            plot_output <- list(plot1, plot2)
                            
                            return(plot_output)
                          },
                          
                          
                          resid = function(){
                            return(the_residuals)
                          },
                          
                          predict = function(newdata){
                            regression_coefficients <- as.vector(regression_coefficients)
                            new_fitted_values <- rowSums(regressions_coefficients[1] + newdata * regressions_coefficients[-1])
                            return(new_fitted_values)
                          },
                          
                          coef = function(){
                            return(drop(regressions_coefficients))
                          },
                          
                          summary = function(){


                            summary_output <- as.data.frame(cbind(regressions_coefficients,as.matrix(standard_error),t_values, formatC(p_values, format = "e", digits = 5), p_stars(p_values)))
                            colnames(summary_output) <-c("Coefficients","Standard error","t_values", "p_values", "")

                            print.data.frame(summary_output)
                            cat(paste("\n\nResidual standard error: ", sigma1, " on ", the_degrees_of_freedom, " degrees of freedom: ", sep = ""))
                          }

                        ))

print_mask = function(x) {
  print(x)
}

p_stars = function(p_values) {
  stars <- c()
  i <- 1
  repeat {
    if ( p_values[i] > 0.1 ){ stars[i] <- " "}
    else if( p_values[i] > 0.05 ){ stars[i] <- " . "}
    else if( p_values[i] > 0.01 ){ stars[i] <- "*"}
    else if( p_values[i] >0.001 ){ stars[i] <-"**"}
    else {stars[i] <- "***"}
    i <- i+1
    
    if( i > length(p_values)){break}
  }
  return(stars)
}





visualize_airport_delays <- function(){
  airports <- arr_delay <- dest <- faa <- flights <- lat <- lon <-  name <- NULL

  # Keep only the collumns that we want
  df <- flights %>% select(arr_delay, dest) %>% group_by(dest)

  # Replace the collumn arr_delay with mean delay and have unique values of dest
  df <- summarise(df, mean_delay = mean(arr_delay, na.rm = TRUE))

  # Merge the 2 table but only the collumns that we need and use
  df <- left_join(df, airports %>% select(faa, name, lat, lon), by = c("dest" = "faa"))

  # Plot the plot
  my_plot = ggplot(data = df, aes(x = lon, y = lat, color = dest)) + geom_point()
}


# Boston Housing is not available in the package so I downloanded from the github

trainIndex <- createDataPartition(BostonHousing$medv, p = .8,list = FALSE,times = 1)

# Splitting the dataset into training and test
bostonHousingTrain <- BostonHousing[ trainIndex,]
bostonHousingTest  <- BostonHousing[-trainIndex,]

# Make the models
lr = train(medv ~ ., data = bostonHousingTrain, method="lm")
lr_forward = train(medv~., data = bostonHousingTrain , method="leapForward")

