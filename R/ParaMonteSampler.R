
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

                            public = list( # Public >>

                                buildMode      = "release",
                                mpiEnabled     = FALSE,
                                inputFile      = NULL,
                                spec           = NULL,
                                reportEnabled  = NULL,

                                initialize = function(platform, website) { # initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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



