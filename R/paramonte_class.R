
####################################################################################################################################
####################################################################################################################################
#' @title Instantiate a [`R6`] **`paramonte_class`** object for **[paramonte]** package usage
#' @description
#' `paramonte_class$new()` initializes a `paramonte_class` object.\cr
#' It can then be used to instantiate \code{\link{ParaDRAM}} samplers object.\cr
#' *   This is the first list item.
#' *   Here's the second list item.
#'
#'     I need to add another paragraph below the  second list item.
#'
#' *   And here's the third list item.
#' # Heading 1
#' this is under heading 1
#' ## Heading 2
#' this is under heading 2
#' ### Heading 3
#' this is under heading 3
#' @return The class generator returns a \code{\link{R6}} `paramonte_class` object.
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
                                #' @description Instantiates a `paramonte_class` object.
                                #' @return Returns a new `paramonte_class` object.
                                #' @examples pm = paramonte_class$new()
                                initialize = function( ) {

                                    private$Err        = Err_class$new()
                                    private$Err$prefix = private$libName

                                    private$Err$note   = 0
                                    private$Err$note   = "'paramonte' object has been created."

                                    # class(self) = private$libName

                                    self$print()

                                },

                                # print >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                #' @description Prints `paramonte_class` object usage.
                                #' @examples
                                #'     pm = paramonte_class$new()
                                #'     pm$print()
                                #'     # or
                                #' @examples
                                #'     pm = paramonte_class$new()
                                #'     pm
                                print = function( ) {

                                    tab             = private$Err$tab

                                    private$Err$box = paste( "To instantiate 'ParaDRAM' object try:", "\n",
                                                             "\n",
                                                             tab, "pmpd = pm$ParaDRAM()", "\n",
                                                             sep = "" )

                                },

                                # ParaDRAM >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                #' @description This method instantiate a \code{\link{R6}} \code{\link{ParaDRAM}} class object. \cr
                                #'     It can then be used to call \code{\link{ParaDRAM}} routines. \cr
                                #'     This is the `ParaDRAM` class constructor to generate instances of **serial** and **parallel** \cr
                                #'     **Delayed-Rejection Adaptive Metropolis-Hastings Markov Chain Monte Carlo** sampler class \cr
                                #'     of the **\link{paramonte}** library. The `ParaDRAM` class is a child of the \cr
                                #'     `ParaMonteSampler` class. \cr
                                #'     The object of `ParaDRAM` class can be instantiated only via \code{\link{paramonte_class}} object \cr
                                #'     method (for example: `pm$ParaDRAM()`).\cr
                                #'     \cr
                                #'     All `ParaDRAM` class attributes are optional and all attributes can be set after a `ParaDRAM` \cr
                                #'     instance is returned by the class constructor.
                                #' @return This function returns a \code{\link{R6}} \code{\link{ParaDRAM}} class object.
                                #' @examples
                                #'     pm   = paramonte_class$new()
                                #'     pmpd = pm$ParaDRAM()
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
