
####################################################################################################################################
################################## AutoCorr ########################################################################################
# >>
AutoCorr <- R6::R6Class(  "AutoCorr",
                          
                            # Private >> ###########################################################################################

                            private = list(
                                
                                dfref         = NULL,
                                methodName    = NULL,
                                reportEnabled = NULL,
                                Err           = NULL,
                                plotTypeList  = c ( "line",
                                                    "scatter",
                                                    "lineScatter" ),
                                
                                reportWrongPlotName = reportWrongPlotName, # reportWrongPlotName >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>
                                
                                checkInputPlotNames = checkInputPlotNames, # checkInputPlotNames >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>
                                
                                checkResetType      = checkResetType, # checkResetType >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                
                                resetPlot           = resetPlot # resetPlot >> (auxil) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                
                                # resetPlot = function( resetType="soft", plotNames = "all" ) { # resetPlot >>>>>>>>>>>>>>>>>>>>>>>>>>
                                #     
                                #     # Check if plotNames are elements of private$plotTypeList >>
                                #     
                                #     requestedPlotTypeList = private$checkInputPlotNames( plotNames, className = class(self)[1] )
                                #     
                                #     # Check if resetType is "hard" >>
                                #     
                                #     resetTypeIsHard = private$checkResetType(resetType)
                                #     
                                #     # Reset plots >>
                                #     
                                #     for ( requestedPlotType in requestedPlotTypeList ) {
                                #         plotObject             = NULL
                                #         requestedPlotTypeLower = tolower(requestedPlotType)
                                #         
                                #         isHeatmap = is.element("heatmap", requestedPlotTypeLower)
                                #         
                                #         if ( !resetTypeIsHard ) {
                                #             plotComponent                      = self$plot
                                #             plotObject                         = plotComponent[[requestedPlotType]]
                                #             plotObject$reset()
                                #         }
                                #         
                                #         # reset heatmap >>
                                #         
                                #         if ( isHeatmap ) {
                                #             
                                #             if ( resetTypeIsHard ) {
                                #                 
                                #                 plotObject = HeatMapPlot$new( plotType      = requestedPlotType,
                                #                                               dataFrame     = self$df,
                                #                                               methodName    = private$methodName,
                                #                                               reportEnabled = private$reportEnabled,
                                #                                               resetPlot     = private$resetPlot,
                                #                                               Err           = private$Err )
                                #                 
                                #             }
                                #             
                                #             plotObject$heatmap$kws$vmin  = floor  ( min(self$df) * 10 ) / 10
                                #             plotObject$heatmap$kws$vmax  = ceiling( max(self$df) * 10 ) / 10
                                #             
                                #             if ( private$isCorMat ) {
                                #                 
                                #                 plotObject$heatmap$kws$title = paste0( capitalize(self$method), "'s Correlation Strength")
                                #                 
                                #             } else {
                                #                 
                                #                 plotObject$heatmap$kws$title = "Covariance Strength"
                                #                 
                                #             }
                                #             
                                #             if ( !is.null(plotObject) ) { self$plot[[requestedPlotType]] = plotObject }
                                #             
                                #         }
                                #         
                                #     }
                                #     
                                # }
                                
                            ), # << Private
                            
                            # Public >> ############################################################################################

                            public = list(
                                
                                df      = NULL,
                                rows    = NULL,
                                plot    = list(),
                                columns = NULL,

                                initialize = function( dataFrame     = dataFrame, # initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                       columns       = columns,
                                                       methodName    = methodName,
                                                       reportEnabled = reportEnabled,
                                                       Err ) {
                                    
                                    self$columns          = columns
                                    
                                    private$dfref         = dataFrame[self$columns]
                                    
                                    private$methodName    = methodName
                                    private$reportEnabled = reportEnabled
                                    
                                    private$Err           = Err
                                    
                                },
                                
                                get = function( reself = FALSE ) { # get >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    # check columns presence >>
                                    
                                    colnames = colnames(private$dfref)
                                    
                                    # check rows presence >>
                                    
                                    if ( is.null(self$rows) ) { self$rows = 1 : nrow(private$dfref) }
                                    
                                    # Compute the autocorrelation >>
                                    
                                    self$df = list()
                                    for ( colname in colnames ) {
                                        i = 1 + 1
                                        x = private$dfref[[colname]]
                                        ACF = acf( x = x, lag = length(x)-1, pl = FALSE )
                                        self$df[[colname]] = ACF$acf
                                    }
                                    self$df = as.data.frame(self$df)
                                    
                                    # Add lags to df >>
                                    
                                    self$df$lag = ACF$lag
                                    
                                    # Graphics >>
                                    
                                    private$Err$note = "adding the autocrrelation graphics tools... "
                                    
                                    private$resetPlot( resetType="hard" )
                                    
                                    # for ( plotType in private$plotTypeList) {
                                    #     
                                    #     self$plot[[plotType]] = LineScatterPlot$new (   plotType      = plotType,
                                    #                                                     dataFrame     = self$df,
                                    #                                                     methodName    = private$methodName,
                                    #                                                     reportEnabled = private$reportEnabled,
                                    #                                                     resetPlot     = private$resetPlot,
                                    #                                                     isAutocorr    = TRUE,
                                    #                                                     Err           = private$Err )
                                    #     
                                    # }
                                    
                                }# ,
                                
                                # reset = NULL

                            ) # << Public

)
# <<
################################## AutoCorr ########################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# AutoCorrObj = AutoCorr$new(arg)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################


