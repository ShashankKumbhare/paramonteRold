#' @title hello function
#'
#' @description This function prints "hello world".
#'
#' @param x someName
#'
#' @return The output from \code{\link{print}}
#' @export
#'
#' @examples
#' hello("Shashank")
#' \dontrun{
#' hello("Shashank")
#' }
hello <- function(x) {
  print(paste("Hello ", x, ", this is the world"))
}
