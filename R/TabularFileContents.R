
####################################################################################################################################
################################## TabularFileContents #############################################################################
# >>

    # Sourcing Methods / Classes / Auxiliary-functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    # sourceFolder( paste( getwd(), "/", "paramonte/stats", sep = "" ) )
    # source( paste( getwd(), "/", "paramonte/vis/BasePlot.R", sep = "" ) )
    # source( paste( getwd(), "/", "paramonte/vis/HeatMapPlot.R", sep = "" ) )
    # source( paste( getwd(), "/", "paramonte/vis/LineScatterPlot.R", sep = "" ) )
    # source( paste( getwd(), "/", "paramonte/vis/DensityPlot1D.R", sep = "" ) )
    # source( paste( getwd(), "/", "paramonte/vis/DensityPlot2D.R", sep = "" ) )
    # source( paste( getwd(), "/", "paramonte/vis/GridPlot.R", sep = "" ) )
    # source( paste( getwd(), "/", "paramonte/vis/Target.R", sep = "" ) )
    # library("egg")

    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Sourcing Methods / Classes / Auxiliary-functions

TabularFileContents <- R6::R6Class(  "TabularFileContents",

                            inherit = OutputFileContents,

                            # Private >> ###########################################################################################

                            private = list(

                                offset               = NULL,
                                plotTypeList         = NULL,
                                isProgressFile       = NULL,
                                sampleLogFuncColName = NULL,

                                reportWrongPlotName = reportWrongPlotName, # reportWrongPlotName >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>

                                checkInputPlotNames = checkInputPlotNames, # checkInputPlotNames >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>

                                checkResetType      = checkResetType, # checkResetType >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                resetPlot           = resetPlot # resetPlot >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                            ), # << Private

                            # Public >> ############################################################################################

                            public = list(

                                df        = NULL,
                                delimiter = NULL,
                                stats     = list(),
                                ncol      = NULL,
                                count     = NULL,
                                ndim      = NULL,
                                plot      = list(),

                                initialize = function( file, # Initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                       fileType,
                                                       delimiter,
                                                       methodName,
                                                       parseContents,
                                                       reportEnabled,
                                                       Err ) {

                                    super$initialize( file, methodName, reportEnabled, Err )

                                    markovChainRequested   = ( fileType == "markovchain" )
                                    private$isProgressFile = ( fileType == "progress"    )

                                    if ( private$isProgressFile ) { private$sampleLogFuncColName = "" } else { private$sampleLogFuncColName = "SampleLogFunc" }

                                    # Data >>

                                    self$delimiter = delimiter

                                    self$df        = read.table( file, header = TRUE, sep = self$delimiter ) # >> Used Delimiter here

                                    if ( private$isProgressFile ) {
                                        private$offset = 0
                                    } else {
                                        private$offset = which( colnames(self$df) == private$sampleLogFuncColName ) # >> index of the first variable
                                        self$ndim      = length(self$df) - private$offset
                                    }
                                    self$count = nrow(self$df)
                                    self$ncol  = ncol(self$df)

                                    # Statistics >>

                                    if ( !private$isProgressFile ) {

                                        self$stats = list()

                                        # Add chain cormat >>

                                        private$Err$note = "computing the sample correlation matrix..."

                                        self$stats$cormat = corMat$new( dataFrame     = self$df,
                                                                        columns       = (private$offset + 1) : (private$offset + self$ndim),
                                                                        methodName    = private$methodName,
                                                                        reportEnabled = private$reportEnabled,
                                                                        method        = "pearson",
                                                                        Err           = private$Err)

                                        self$stats$cormat$get()

                                        # Add chain covmat >>

                                        private$Err$note = "computing the sample covariance matrix..."

                                        self$stats$covmat = covMat$new( dataFrame     = self$df,
                                                                        columns       = (private$offset + 1) : (private$offset + self$ndim),
                                                                        methodName    = private$methodName,
                                                                        reportEnabled = private$reportEnabled,
                                                                        Err           = private$Err )

                                        self$stats$covmat$get()

                                        # Add chain autocorrelation >>

                                        private$Err$note = "computing the sample autocorrelations..."

                                        self$stats$autocorr = AutoCorr$new( dataFrame     = self$df,
                                                                            columns       = private$offset : (private$offset + self$ndim),
                                                                            methodName    = private$methodName,
                                                                            reportEnabled = private$reportEnabled,
                                                                            Err           = private$Err )

                                        self$stats$autocorr$get()

                                        # Add chain maxLogFunc >>

                                        self$stats$maxLogFunc = getMaxLogFunc(dataFrame = self$df)

                                    }

                                    # Graphics >>

                                    private$plotTypeList = c ( "line",
                                                               "scatter",
                                                               "lineScatter" )

                                    if ( !private$isProgressFile) {
                                        private$plotTypeList =  c ( private$plotTypeList,
                                                                    # "line3",
                                                                    # "scatter3",
                                                                    # "lineScatter3",
                                                                    # "jointplot",
                                                                    "histplot",
                                                                    # "kdeplot1",
                                                                    # "kdeplot2",
                                                                    # "contour3",
                                                                    "contourf",
                                                                    "contour",
                                                                    "grid" )
                                    }

                                    private$Err$note = "adding the graphics tools... "

                                    # Reset Plots >>

                                    private$resetPlot( resetType = "hard" )

                                }#,

                                # pubMethod1 = pubMethod1, # pubMethod1 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                #
                                # pubMethod2 = function(arg, ...) { # pubMethod2 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                #
                                #     return(invisible(self))
                                #
                                # }

                            ) # << Public

)
# <<
################################## TabularFileContents #############################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# TabularFileContentsObj = TabularFileContents$new(arg)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################
