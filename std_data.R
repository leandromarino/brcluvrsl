#' Standardize the Data Set
#'
#' @details
#' This is a generic function and can have new other \code{std_method}'s defineds
#' by the user. In this first implementation just uses the usual location (mu) and
#' scale (sigma) data standardization.
#' In future, we consider adding other methods such as Variance-to-Range Ratio
#' Weighting from Stenley and Brusco.
#'
#' @param data an \code{data.frame} or a \code{matrix} object that will be
#' standardized
#' @param std_method the method that will be used to standardize. For now just two
#' options: 'none' or 'std'
#'
#' @return The standardized dataset.
#'
#' @examples
#' df <- wine[, -1] # removing the class_id column
#' std_data(df, 'std')
#'
#' @export
#'
std_data <- function(data, std_method){
  if(std_method == 'std'){
    data <- std_data_musigma(data)
  }

  if(std_method == 'none'){
    data <- data
  }
  data
}




# internal std_data_musigma
#' @name std_data_musigma
#'
std_data_musigma <- function(data){
  data %>%
    caret::preProcess(., method = c('center', 'scale')) %>%
    stats::predict(., data)
}




