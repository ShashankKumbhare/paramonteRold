
####################################################################################################################################
####################################################################################################################################
#' @title Instantiate a \code{\link{R6}} ParaMonte object for \code{\link{paramonteR}} package
#' @description ```paramonte_class$new()``` or ``` paramonte()``` initializes a ParaMonte object.\cr
#'     It can then be used to instantiate ParaMonte samplers object.
#' @return This function returns a \code{\link{R6}} ```paramonte_class``` object.
#' @examples
#' pm = paramonte_class$new()
#' @import ggplot2
#' @export
####################################################################################################################################
####################################################################################################################################


####################################################################################################################################
################################## paramonte_class #################################################################################
# >>

    # Sourcing Methods / Classes / Auxiliary-functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    # library('R6')
    # source(       paste( getwd(), "/", "paramonte/auxil/functions/sourceFolder.R", sep = "" ) )
    # sourceFolder( paste( getwd(), "/", "paramonte/auxil/functions", sep = "" ) )
    # sourceFolder( paste( getwd(), "/", "paramonte/auxil/classes", sep = "" ) )
    # source(       paste( getwd(), "/", "paramonte/interface/@ParaDRAM/ParaDRAM.R", sep = "" ) )
    # source(       paste( getwd(), "/", "paramonte/interface/@ParaDRAM/ParaDRAMUnlocked.R", sep = "" ) )
    # thisFileDir = getThisFileDir()

    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Sourcing Methods / Classes / Auxiliary-functions

paramonte_class <- R6::R6Class( "paramonte",

                            # Private >> ###########################################################################################

                            private = list(

                                Err     = NULL,
                                libName = "paramonte"

                            ), # << Private

                            # Public >> ############################################################################################

                            public = list(

                                website  = list(),
                                authors  = NULL,
                                credits  = NULL,
                                version  = NULL,
                                platform = "",
                                names    = list( paramonte = "ParaMonte",
                                                 paradram  = "ParaDRAM",
                                                 matdram   = "MatDRAM",
                                                 paradise  = "ParaDISE",
                                                 paranest  = "ParaNest",
                                                 paratemp  = "ParaTemp" ),

                                # initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                #' @description Instantiates a \code{\link{paramonte_class}} object.
                                #' @return Returns a new \code{\link{paramonte_class}} object.
                                #' @examples pm = paramonte_class$new()
                                #' @examples pm = paramonte()
                                initialize = function( ) {

                                    private$Err        = Err_class$new()
                                    private$Err$prefix = private$libName

                                    private$Err$note   = 0
                                    private$Err$note   = "'paramonte' object has been created."

                                    # class(self) = private$libName

                                    self$print()

                                },

                                # print >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                #' @description Prints \code{\link{paramonte_class}} object usage.
                                #' @examples
                                #' pm = paramonte()
                                #' pm
                                #' @examples
                                #' pm = paramonte()
                                #' pm$print()
                                print = function( ) {

                                    tab             = private$Err$tab

                                    private$Err$box = paste( "To instantiate 'ParaDRAM' object try:", "\n",
                                                             "\n",
                                                             tab, "pmpd = pm$ParaDRAM()", "\n",
                                                             sep = "" )

                                },

                                # ParaDRAM >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                #' @description This method initializes a ParaDRAM sampler object. It can then be used to call ParaDRAM routines.
                                #' @return This function returns a \code{\link{R6}} ```ParaDRAM``` object.
                                #' @examples
                                #' pm   = paramonte()
                                #' pmpd = pm$ParaDRAM()
                                ParaDRAM = function() {

                                    ParaDRAMObj = ParaDRAM$new()

                                    lockEnvironment(ParaDRAMObj)

                                    return( ParaDRAMObj )

                                }

                            ) # << Public

)
# <<
################################## paramonte_class #################################################################################
####################################################################################################################################


# ####################################################################################################################################
# ################################## paramonte #######################################################################################
# # >>
# paramonte = function() {  return( paramonte_class$new() )  }
# # <<
# ################################## paramonte #######################################################################################
# ####################################################################################################################################
