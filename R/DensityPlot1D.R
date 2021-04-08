
####################################################################################################################################
################################## DensityPlot1D ###################################################################################
# >>
DensityPlot1D <- R6::R6Class(  "DensityPlot1D",
                            
                            inherit = BasePlot,
                            
                            # Private >> ###########################################################################################
                            
                            private = list(
                                
                                binwidth  = 0.15
                                
                            ), # << Private
                            
                            # Public >> ############################################################################################
                            
                            public = list(
                                
                                rows     = NULL,
                                xcolumns = NULL,
                                target   = NULL,
                                
                                initialize = function( plotType, # initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                       dataFrame     = NULL,
                                                       methodName    = "ParaMonte",
                                                       reportEnabled = TRUE,
                                                       resetPlot     = NULL,
                                                       isGrid        = FALSE,
                                                       Err ) {
                                    
                                    super$initialize ( plotType      = plotType,
                                                       dataFrame     = dataFrame,
                                                       methodName    = methodName,
                                                       reportEnabled = reportEnabled,
                                                       resetPlot     = resetPlot,
                                                       isGrid        = isGrid,
                                                       Err           = Err )
                                    
                                    private$isGrid     = isGrid
                                    
                                    self$reset( isFirstTime = TRUE )
                                    
                                },
                                
                                reset = function( resetType = "soft", isFirstTime = FALSE ) { # reset >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    if ( resetType == "hard" ) {
                                        private$resetPlot( resetType = "hard", plotNames = private$type$name )
                                    } else if ( !isFirstTime ) {
                                        private$Err$note = paste0( "resetting the properties of the ", private$type$name, " plot..." )
                                        super$reset()
                                    }
                                    
                                    self$xcolumns         = private$dfHeaders[private$offset+1]
                                    # self$legend$enabled   = FALSE
                                    self$legend$enabled   = TRUE
                                    
                                    self$theme$builtInTheme$reset()
                                    
                                },
                                
                                make = function( reself = FALSE, # make >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                 xcolumns,
                                                 show = TRUE,
                                                 target = FALSE ) {
                                    
                                    isGrid = private$isGrid
                                    
                                    # Plot size for R Notebook >>
                                    
                                    private$setPlotSizeforRNotebook()
                                    
                                    # Set plot themes >>
                                    
                                    plot = private$setPlotTheme()
                                    
                                    # Set geometry >>
                                    
                                    geometry = 'histogram'
                                    
                                    # Loop for plots >>
                                    
                                    text       = "plot = plot"
                                    showLegend = self$legend$enabled
                                    i = 0
                                    
                                    if( !missing(xcolumns) ) { self$xcolumns = xcolumns }
                                    
                                    private$checkXcolumnsValues()
                                    
                                    for ( xcolumn in self$xcolumns ) {
                                        
                                        i = i + 1
                                        color = xcolumn # >> legend name
                                        text = paste0( text , " + geom_", geometry, "( aes(x=", xcolumn, ", color='", color, "', fill='", color, "'), show.legend = ", showLegend, ", binwidth=", private$binwidth, " )" )
                                        plot  = plot + theme( legend.position = c(0.8, 0.5) ) +
                                                       theme( legend.background = element_rect( fill = alpha('white', 0.5) ) )
                                        
                                    }
                                    
                                    eval( parse( text = text ) )
                                    
                                    # Add color >>
                                    
                                    if ( isGrid == TRUE ) {
                                        colorValues = lighten( private$colorLow )
                                    } else {
                                        colorValues = lighten( private$color[1:length(self$xcolumns)] )
                                    }
                                    
                                    plot = plot + scale_color_manual( name = "Variables", values = colorValues ) +
                                                  scale_fill_manual ( name = "Variables", values = colorValues )
                                    
                                    # Add xlable & ylable >>
                                    
                                    if ( length(self$xcolumns) == 1 ) { xlab = self$xcolumns } else { xlab = "Variable Values" }
                                    if ( isGrid == TRUE ) { ylab = self$xcolumns } else { ylab = "Count" }
                                    plot = plot + labs( x = xlab, y = ylab )
                                    
                                    # Finalize make method >>
                                    
                                    private$finalizeMake( plot, isGrid, target, show )
                                    
                                    return( invisible( self$currentFig ) )
                                    
                                }
                                
                            ) # << Public
                         
)
# <<
################################## DensityPlot1D ###################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# DensityPlot1DObj = DensityPlot1D$new(arg)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################





