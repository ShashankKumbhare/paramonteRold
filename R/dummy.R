
####################################################################################################################################
##################################### dummy() ######################################################################################
# >>
#' @title This is the title of dummy function
#' @description This is the detailed description of this dummy function
#' @param arg1 description of arg1
#' @param arg2 description of arg2
#' @return this function returns object of class \code{\link{R6Class}}.
#' @examples dummyOut = dummy("Shashank", "Good job.")
#' @export
dummy <- function(arg1, arg2) {
    print( paste0(arg1, ", dummy func has worked!, ", arg2) )
    print( "Now running Running dummy2() within dummy()" )
    dummy2("Inside dummy()", "Ok")
    return( "dummy")
}
# <<
##################################### dummy() ######################################################################################
####################################################################################################################################
