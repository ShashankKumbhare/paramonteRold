
####################################################################################################################################
####################################################################################################################################
#' @title Instantiate a \code{\link{R6}} **`ParaMonteSampler`** class object for **\link{paramonte}** package
#' @description This is the `ParaMonteSampler` base class for the ParaMonte \cr
#'     sampler routines. This class is NOT meant to be directly accessed \cr
#'     or called by the user of the **\link{paramonte}** package. However, its children, \cr
#'     such as the **\link{ParaDRAM}** sampler class will be directly accessible to the public.
#' @return This function returns a \code{\link{R6}} `ParaMonteSampler` class object.
####################################################################################################################################
####################################################################################################################################


####################################################################################################################################
################################## ParaMonteSampler ################################################################################
# >>

    # Sourcing Methods / Classes / Auxiliary-functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    # thisFileDir = getThisFileDir()
    # sourceFolder( paste( thisFileDir, "/", "public_methods", sep = "" ) )
    # sourceFolder( paste( thisFileDir, "/", "private_methods", sep = "" ) )

    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Sourcing Methods / Classes / Auxiliary-functions

ParaMonteSampler <- R6::R6Class(  "ParaMonteSampler",

                            # Private >> ###########################################################################################

                            private = list(

                                Err        = NULL,
                                methodName = "ParaDRAM",
                                objectName = NULL,
                                fileInfo   = NULL,
                                platform   = NULL,
                                libName    = NULL,
                                website    = list(),
                                method     = NULL,
                                ndim       = NULL,

                                verifyFileType       = verifyFileType,       # verifyFileType >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                setFileToRead        = setFileToRead,        # setFileToRead >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                setDelimiterToRead   = setDelimiterToRead,   # setDelimiterToRead >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                checkInputArg        = checkInputArg,        # checkInputArg >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                parseEverything      = parseEverything,      # parseEverything >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                updateUserProcessing = updateUserProcessing, # updateUserProcessing >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                updateUserDone       = updateUserDone,       # updateUserDone >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                updateUserSucess     = updateUserSucess      # updateUserSucess >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                            ), # << Private

                            # Public >> ############################################################################################

                            public = list(

                                #' @field buildMode
                                #' optional string argument with the default value “release”. possible choices are:
                                #' - “debug” \cr
                                #'     to be used for identifying sources of bug and causes of code crash.
                                #' - “release” \cr
                                #'     to be used in all other normal scenarios for maximum runtime efficiency.
                                buildMode      = "release",

                                #' @field mpiEnabled
                                #' optional logical (boolean) indicator which is `FALSE` by default. \cr
                                #' If it is set to `TRUE`, it will cause the ParaMonte simulation \cr
                                #' to run in parallel on the requested number of processors. \cr
                                #' See the class documentation guidelines in the above for \cr
                                #' information on how to run a simulation in parallel.
                                mpiEnabled     = FALSE,

                                #' @field inputFile
                                #' optional string input representing the path to \cr
                                #' an external input namelist of simulation specifications. \cr
                                #' USE THIS OPTIONAL ARGUMENT WITH CAUTION AND \cr
                                #' ONLY IF YOU KNOW WHAT YOU ARE DOING. \cr
                                #' \cr
                                #' **WARNING** \cr
                                #' \cr
                                #' Specifying an input file will cause the sampler to ignore \cr
                                #' all other simulation specifications set by the user via \cr
                                #' sampler instance's `spec`-component attributes.
                                inputFile      = NULL,

                                #' @field spec
                                #' An R list containing all simulation specifications. \cr
                                #' All simulation attributes are by default set to appropriate \cr
                                #' values at runtime. To override the default simulation \cr
                                #' specifications, set the ``spec`` attributes to some \cr
                                #' desired values of your choice. \cr
                                #' \cr
                                #' If you need help on any of the simulation specifications, try \cr
                                #' the supplied ``helpme()`` function in this component. \cr
                                #' \cr
                                #' If you wish to reset some specifications to the default values, \cr
                                #' simply set them to ``None``.
                                spec           = NULL,

                                #' @field reportEnabled
                                #' optional logical (boolean) indicator which is ``TRUE`` by default. \cr
                                #' If it is set to ``TRUE``, it will cause extensive guidelines to be \cr
                                #' printed on the standard output as the simulation or post-processing \cr
                                #' continues with hints on the next possible steps that could be taken \cr
                                #' in the process. If you do not need such help and information set \cr
                                #' this variable to ``FALSE`` to silence all output messages.
                                reportEnabled  = NULL,

                                # initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                #' @description Instantiates a ``ParaMonteSampler`` class object.
                                #' @param platform platform for **\link{paramonte}** package.
                                #' @param website website for **\link{paramonte}** package.
                                #' @return Returns a new ``ParaMonteSampler`` class object.
                                initialize = function(platform, website) {

                                    self$spec            = list()
                                    self$reportEnabled   = TRUE

                                    private$Err               = Err_class$new()

                                    private$method            = list()
                                    private$method$isParaDRAM = FALSE
                                    private$method$isParaNest = FALSE
                                    private$method$isParaTemp = FALSE
                                    website                   = list()                                 # >> SET TEMPORARILY xxx <<
                                    website$home$url          = "https://www.cdslab.org/paramonte/"    # >> SET TEMPORARILY xxx <<
                                    private$website           = website
                                    private$platform          = platform

                                    return(invisible(self))

                                },

                                # readChain >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                #' @description Instantiates a ``ParaMonteSampler`` class object.
                                #' @param platform platform for **\link{paramonte}** package.
                                #' @param website website for **\link{paramonte}** package.
                                #' @return Returns a new ``ParaMonteSampler`` class object.
                                readChain       = readEverything, # readChain >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                readSample      = readEverything, # readSample >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                readReport      = readEverything, # readReport >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                readRestart     = readEverything, # readRestart >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                readProgress    = readEverything, # readProgress >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                readMarkovChain = readEverything  # readMarkovChain >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                            ) # << Public

)
# <<
################################## ParaMonteSampler ################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# ParaMonteSamplerObj = ParaMonteSampler$new( platform, website )
# <<
################################## Help Code #######################################################################################
####################################################################################################################################
