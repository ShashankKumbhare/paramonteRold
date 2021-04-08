
####################################################################################################################################
################################## RestartFileContents #############################################################################
# >>

    # Sourcing Methods / Classes / Auxiliary-functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    # source( paste( getwd(), "/", "paramonte/vis/BasePlot.R", sep = "" ) )
    # source( paste( getwd(), "/", "paramonte/vis/EllipsoidPlot.R", sep = "" ) )

    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Sourcing Methods / Classes / Auxiliary-functions

RestartFileContents <- R6::R6Class( "RestartFileContents",

                                    inherit = OutputFileContents,

                            # Private >> ###########################################################################################

                            private = list(

                                fileType     = NULL,
                                lineList     = NULL,
                                lineListLen  = NULL,
                                plotTypeList = NULL,

                                readRestartParaDRAM = function(arg, ...) { # readRestartParaDRAM >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                    self$contents       = readLines( self$file )                                       # >> Get Data
                                    private$lineList    = self$contents
                                    private$lineListLen = length(private$lineList)

                                    # find count of updates >>

                                    self$count = sum( self$contents == self$propNameList[1] )

                                    # find ndim via meanVec entry: self$propNameList[5] >>

                                    rowOffset = 1
                                    while ( private$lineList[rowOffset] != self$propNameList[5] ) {
                                        rowOffset = rowOffset + 1
                                        if ( rowOffset > private$lineListLen ) { private$reportCorruptFile() }
                                    }
                                    rowOffset = rowOffset + 1 # the first numeric value of the meanVec

                                    self$ndim = 0

                                    while ( isNumericString( private$lineList[rowOffset+self$ndim] ) ) { self$ndim = self$ndim + 1 }
                                    if ( self$ndim == 0 ) { private$reportCorruptFile() }

                                    # parse the restart file contents >>

                                    fieldNamesDict = list()

                                    fieldNamesDict[[ self$propNameList[1] ]] = array( 0, c(self$count) )
                                    fieldNamesDict[[ self$propNameList[2] ]] = array( 0, c(self$count) )
                                    fieldNamesDict[[ self$propNameList[3] ]] = array( 0, c(self$count) )
                                    fieldNamesDict[[ self$propNameList[4] ]] = array( 0, c(self$count) )
                                    fieldNamesDict[[ self$propNameList[5] ]] = array( 0, c(self$count, self$ndim) )
                                    fieldNamesDict[[ self$propNameList[6] ]] = array( 0, c(self$ndim, self$ndim, self$count) )
                                    fieldNamesDict[[ self$propNameList[7] ]] = array( 0, c(self$ndim, self$ndim, self$count) )

                                    skip = 10 + self$ndim * (self$ndim + 3) / 2

                                    meanVecList = list()

                                    for ( icount in 1 : self$count ) {

                                        if ( icount %% 10 == 0 ) { private$updateProgress(icount/self$count) }

                                        istart = (icount-1) * skip + 1

                                        rowOffset = 1
                                        fieldNamesDict[[ self$propNameList[1] ]][icount] = as.double( private$lineList[istart+rowOffset] )

                                        rowOffset = 3
                                        fieldNamesDict[[ self$propNameList[2] ]][icount] = as.double( private$lineList[istart+rowOffset] )

                                        rowOffset = 5
                                        fieldNamesDict[[ self$propNameList[3] ]][icount] = as.double( private$lineList[istart+rowOffset] )

                                        rowOffset = 7
                                        fieldNamesDict[[ self$propNameList[4] ]][icount] = as.double( private$lineList[istart+rowOffset] )

                                        rowOffset = 9
                                        iend = istart + rowOffset + self$ndim
                                        fieldNamesDict[[ self$propNameList[5] ]][icount,] = as.double( private$lineList[(istart+rowOffset):(iend-1)] )
                                        meanVecList[[icount]] = as.list( fieldNamesDict[[ self$propNameList[5] ]][icount,] )

                                        # covMat >>
                                        for ( i in 1 : self$ndim ) {
                                            istart = iend + 1
                                            iend   = iend + i
                                            fieldNamesDict[[ self$propNameList[6] ]][1:i,    i,icount] = as.double( private$lineList[istart:iend] )
                                            fieldNamesDict[[ self$propNameList[6] ]][  i,1:i-1,icount] = fieldNamesDict[[ self$propNameList[6] ]][1:i-1,i,icount]
                                        }

                                        # corMat >>
                                        fieldNamesDict[[ self$propNameList[7] ]][,,icount] = cov2cor( fieldNamesDict[[ self$propNameList[6] ]][,,icount] )

                                    }

                                    private$updateProgress(1)

                                    # Make a data-frame >>

                                    self$df = data.frame( matrix(ncol = 4, nrow = self$count) )

                                    colnames(self$df) = self$propNameList[1:4]

                                    self$df[[ self$propNameList[1] ]] = fieldNamesDict[[ self$propNameList[1] ]]
                                    self$df[[ self$propNameList[2] ]] = fieldNamesDict[[ self$propNameList[2] ]]
                                    self$df[[ self$propNameList[3] ]] = fieldNamesDict[[ self$propNameList[3] ]]
                                    self$df[[ self$propNameList[4] ]] = fieldNamesDict[[ self$propNameList[4] ]]
                                    self$df[[ self$propNameList[5] ]] = meanVecList

                                    self$covMat  = fieldNamesDict[[ self$propNameList[6] ]]
                                    self$corMat  = fieldNamesDict[[ self$propNameList[7] ]]

                                    # # Reset covmat plot
                                    #
                                    # matrix            = self$covMat
                                    # requestedPlotType = "covMat"
                                    #
                                    # plotObject = EllipsoidPlot$new (    matrix        = matrix,
                                    #                                     plotType      = requestedPlotType,
                                    #                                     dataFrame     = self$df,
                                    #                                     methodName    = private$methodName,
                                    #                                     reportEnabled = private$reportEnabled,
                                    #                                     resetPlot     = private$resetPlot,
                                    #                                     Err           = private$Err )
                                    #
                                    # self$plot[[requestedPlotType]] = plotObject
                                    #
                                    # # Reset cormat plot
                                    #
                                    # matrix            = self$corMat
                                    # requestedPlotType = "corMat"
                                    #
                                    # plotObject = EllipsoidPlot$new (    matrix        = matrix,
                                    #                                     plotType      = requestedPlotType,
                                    #                                     dataFrame     = self$df,
                                    #                                     methodName    = private$methodName,
                                    #                                     reportEnabled = private$reportEnabled,
                                    #                                     resetPlot     = private$resetPlot,
                                    #                                     Err           = private$Err )
                                    #
                                    # self$plot[[requestedPlotType]] = plotObject

                                    # return(self)

                                },

                                reportWrongPlotName = reportWrongPlotName, # reportWrongPlotName >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>

                                checkInputPlotNames = checkInputPlotNames, # checkInputPlotNames >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>

                                checkResetType      = checkResetType, # checkResetType >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                resetPlot = resetPlot, # resetPlot >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                reportCorruptFile = function() { # reportCorruptFile >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                    tab = private$Err$tab

                                    private$Err$prefix = private$methodName
                                    private$Err$abort  = paste0("The structure of the file: ", "\n",
                                                                "\n",
                                                                tab, "'", self$file, "'", "\n",
                                                                "\n",
                                                                "does not match a ", private$methodName, " ", private$fileType, " file.", "\n",
                                                                "The contents of the file may have been compromised.",  "\n",
                                                                "Verify the integrity of the contents of this file before attempting to reread it.")

                                }

                            ), # << Private

                            # Public >> ############################################################################################

                            public = list(

                                ndim     = NULL,
                                count    = NULL,
                                contents = NULL,
                                df       = NULL,
                                covMat   = NULL,
                                corMat   = NULL,
                                plot     = list(),

                                propNameList = c ( "meanAcceptanceRateSinceStart",
                                                   "sampleSize",
                                                   "logSqrtDeterminant",
                                                   "adaptiveScaleFactorSquared",
                                                   "meanVec",
                                                   "covMat",
                                                   "corMat" ),

                                initialize = function( file, # Initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                       methodName,
                                                       reportEnabled,
                                                       Err ) {

                                    super$initialize( file, methodName, reportEnabled, Err )

                                    if ( private$methodName == pm$names$paradram | private$methodName == pm$names$matdram ) {

                                        private$readRestartParaDRAM()

                                    } else {

                                        private$Err$prefix = 'ParaMonte'
                                        private$Err$note   = 0
                                        private$Err$abort  = paste0( "Internal error occurred. Unrecognized methodName in the class", "\n",
                                                                    "constructor of RestartFileContents: ", self$methodName )
                                    }

                                    # Graphics >>

                                    private$plotTypeList = c ( "line",
                                                               "scatter",
                                                               "lineScatter" )

                                    private$Err$note = "adding the graphics tools... "

                                    private$resetPlot( resetType = "hard", isRestart = TRUE )

                                    # Reset covmat plot

                                    matrix            = self$covMat
                                    requestedPlotType = "covMat2"

                                    plotObject = EllipsoidPlot$new (    matrix        = matrix,
                                                                        plotType      = requestedPlotType,
                                                                        dataFrame     = self$df,
                                                                        methodName    = private$methodName,
                                                                        reportEnabled = private$reportEnabled,
                                                                        resetPlot     = private$resetPlot,
                                                                        Err           = private$Err )

                                    self$plot[[requestedPlotType]] = plotObject

                                    # Reset cormat plot

                                    matrix            = self$corMat
                                    requestedPlotType = "corMat2"

                                    plotObject = EllipsoidPlot$new (    matrix        = matrix,
                                                                        plotType      = requestedPlotType,
                                                                        dataFrame     = self$df,
                                                                        methodName    = private$methodName,
                                                                        reportEnabled = private$reportEnabled,
                                                                        resetPlot     = private$resetPlot,
                                                                        Err           = private$Err )

                                    self$plot[[requestedPlotType]] = plotObject

                                }

                            ) # << Public

)
# <<
################################## RestartFileContents #############################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# RestartFileContentsObj = RestartFileContents$new(arg)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################

