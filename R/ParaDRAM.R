
####################################################################################################################################
################################## ParaDRAM ########################################################################################
# >>

    # Sourcing Methods / Classes / Auxiliary-functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    # library('R6')
    # source(       paste( getwd(), "/", "paramonte/auxil/functions/sourceFolder.R", sep = "" ) )
    # sourceFolder( paste( getwd(), "/", "paramonte/auxil/functions", sep = "" ) )
    # sourceFolder( paste( getwd(), "/", "paramonte/auxil/classes", sep = "" ) )
    # sourceFolder( paste( getwd(), "/", "paramonte/interface/@ParaMonteSampler", sep = "" ) )
    # thisFileDir = getThisFileDir()

    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Sourcing Methods / Classes / Auxiliary-functions

ParaDRAM <- R6::R6Class(    "ParaDRAM",

                            inherit = ParaMonteSampler,

                            # Private >> ###########################################################################################

                            private = list(

                                ParaDRAMUnlocked = NULL

                            ), # << Private

                            # Public >> ############################################################################################

                            public = list(

                                initialize = function( platform = "", # initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                       website  = "" ) {

                                    super$initialize(platform, website)                                  # >> ParaMonteSampler class

                                    private$method$isParaDRAM = TRUE
                                    private$Err$prefix        = private$methodName

                                    textParaDRAMUnlocked      = getTextParaDRAMUnlocked()

                                    eval( parse(text = textParaDRAMUnlocked) )

                                    private$ParaDRAMUnlocked  = ParaDRAMUnlocked

                                    self$print()

                                },

                                print = function() { # print >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                    tab             = private$Err$tab

                                    private$Err$box = paste( "Post-Processing Usage:", "\n",
                                                             "\n",
                                                             tab, "Call ParaDRAM routines as:", "\n",
                                                             "\n",
                                                             tab, tab, "To read Chain file:    pmpd$readChain('filename')   ", "\n",
                                                             tab, tab, "To read Sample file:   pmpd$readSample('filename')  ", "\n",
                                                             tab, tab, "To read Report file:   pmpd$readReport('filename')  ", "\n",
                                                             tab, tab, "To read Restart file:  pmpd$readRestart('filename') ", "\n",
                                                             tab, tab, "To read Progress file: pmpd$readProgress('filename')", "\n",
                                                             sep = "" )

                                }

                            ) # << Public

)
# <<
################################## ParaDRAM ########################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# pmpd = ParaDRAM$new()
# pmpd
# <<
################################## Help Code #######################################################################################
####################################################################################################################################


