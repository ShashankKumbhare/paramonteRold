
####################################################################################################################################
################################## CorCovMat #######################################################################################
# >>

    # Sourcing Methods / Classes / Auxiliary-functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    library("corrplot")
    
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Sourcing Methods / Classes / Auxiliary-functions

CorCovMat <- R6::R6Class(  "CorCovMat",
                            
                            # Private >> ###########################################################################################
                            
                            private = list(
                                
                                cormatPrecision = NULL,
                                reportEnabled   = NULL,
                                plotTypeList    = NULL,
                                matrixType      = NULL,
                                rowsindex       = NULL,
                                isCorMat        = NULL,
                                dfref           = NULL,
                                Err             = NULL,
                                title           = list(),
                                methodName      = NULL,
                                
                                reportWrongPlotName = reportWrongPlotName, # reportWrongPlotName >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>
                                
                                checkInputPlotNames = checkInputPlotNames, # checkInputPlotNames >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>
                                
                                checkResetType      = checkResetType, # checkResetType >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                
                                resetPlot = function( resetType = "soft", # resetPlot >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                      plotNames = "all" ) {
                                    
                                    # Check if plotNames are elements of private$plotTypeList >>
                                    
                                    requestedPlotTypeList = private$checkInputPlotNames( plotNames, className = class(self)[1] )
                                    
                                    # Check if resetType is "hard" >>
                                    
                                    resetTypeIsHard = private$checkResetType(resetType)
                                    
                                    # Reset plots >>
                                    
                                    for ( requestedPlotType in requestedPlotTypeList ) {
                                        
                                        plotObject             = NULL
                                        requestedPlotTypeLower = tolower(requestedPlotType)
                                        
                                        isHeatmap = is.element("heatmap", requestedPlotTypeLower)
                                        
                                        if ( !resetTypeIsHard ) {
                                            plotComponent                      = self$plot
                                            plotObject                         = plotComponent[[requestedPlotType]]
                                            plotObject$reset()
                                        }
                                        
                                        # reset heatmap >>
                                        
                                        if ( isHeatmap ) {
                                            
                                            if ( resetTypeIsHard ) {

                                                plotObject = HeatMapPlot$new( plotType      = requestedPlotType,
                                                                              dataFrame     = self$df,
                                                                              methodName    = private$methodName,
                                                                              reportEnabled = private$reportEnabled,
                                                                              resetPlot     = private$resetPlot,
                                                                              Err           = private$Err )
                                                                              
                                            }
                                            
                                            plotObject$heatmap$kws$vmin  = floor  ( min(self$df) * 10 ) / 10
                                            plotObject$heatmap$kws$vmax  = ceiling( max(self$df) * 10 ) / 10
                                            
                                            if ( private$isCorMat ) {
                                                
                                                plotObject$heatmap$kws$title = paste0( capitalize(self$method), "'s Correlation Strength")
                                                
                                            } else {
                                                
                                                plotObject$heatmap$kws$title = "Covariance Strength"
                                                
                                            }
                                            
                                            if ( !is.null(plotObject) ) { self$plot[[requestedPlotType]] = plotObject }
                                                
                                        }
                                        
                                    }
                                    
                                }
                                
                            ), # << Private
                            
                            # Public >> ############################################################################################
                            
                            public = list(
                                
                                columns = NULL,
                                method  = NULL,
                                plot    = list(),
                                rows    = NULL,
                                df      = NULL,
                                
                                initialize = function( dataFrame, # initialize >> ##################################################
                                                       columns    = NULL,
                                                       methodName = "ParaMonte",
                                                       # rows,
                                                       reportEnabled = TRUE,
                                                       Err ) {
                                    
                                    self$columns          = columns
                                    
                                    private$dfref         = dataFrame
                                    
                                    private$methodName    = methodName
                                    private$reportEnabled = reportEnabled
                                    
                                    private$Err           = Err
                                    
                                },
                                
                                helpme = function(...) { # helpme >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    methodNotFound = true
                                    varargin = c(...)
                                    
                                    if ( length(varargin) == 1 ) {
                                        if ( grepl("reset", varargin[1], fixed = TRUE) ) {
                                            cmd = "doc self.resetPlot"
                                            methodNotFound = false
                                        } else {
                                            methodList = c("plot", "helpme", "get")
                                            for ( method in methodList ) {
                                                if ( varargin[1] == method ) {
                                                    methodNotFound = false
                                                    cmd            = paste0( "doc self.", method )
                                                }
                                            }
                                        }
                                    } else if ( length(varargin) != 1 ) {
                                        private$Err$abort = "The helpme() method takes at most one argument that must be string."
                                    }
                                    if ( methodNotFound ) {
                                        cmd = "doc self"
                                    }
                                    eval(cmd)
                                },
                                
                                get = function( reself = FALSE ) { # get >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    if ( is.null(self$method) ) {
                                        private$isCorMat   = FALSE
                                        private$matrixType = "covariance"
                                    } else if ( any( c("pearson", "kendall", "spearman") == self$method ) ) {
                                        private$isCorMat   = TRUE
                                        private$matrixType = "correlation"
                                    } else {
                                        tab = private$Err$tab
                                        private$abort = paste0( "\n",
                                                                "The requested correlation type must be one of the following string values, ", "\n", "\n",
                                                                tab, "pearson  : standard correlation coefficient", "\n",
                                                                tab, "kendall  : Kendall Tau correlation coefficient", "\n",
                                                                tab, "spearman : Spearman rank correlation.", "\n",
                                                                "\n" )
                                    }
                                    
                                    # check columns presence >>
                                    
                                    if ( any( self$columns != 0 ) ) {
                                        colnames = colnames(private$dfref)[self$columns]
                                    } else {
                                        colnames = colnames(private$dfref)
                                    }
                                    
                                    # check rows presence >>
                                    
                                    if ( is.null(self$rows) ) { self$rows = 1 : nrow(private$dfref) }
                                    
                                    # construct the matrix dataframe >>
                                    
                                    if ( private$isCorMat ) {
                                        self$df = cor( private$dfref[self$rows, colnames], method = self$method )
                                    } else {
                                        self$df = cov( private$dfref[self$rows, colnames] )
                                    }
                                    
                                    # specify columns/index names >>
                                    
                                    colnames(self$df) = colnames
                                    
                                    # Graphics >>
                                    
                                    private$plotTypeList = c("heatmap")
                                    private$Err$note     = paste0( "adding the ", private$matrixType, " graphics tools... " )
                                    self$plot            = list()
                                    
                                    private$resetPlot( resetType = "hard" )
                                    self$plot$reset      = private$resetPlot
                                    
                                    if ( reself ) { return(self) }
                                    
                                }
                                
                            ) # << Public
                        
)
# <<
################################## CorCovMat #######################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# CorCovMatObj = CorCovMat$new(arg)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################

