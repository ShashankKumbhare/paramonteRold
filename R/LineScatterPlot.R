
####################################################################################################################################
################################## LineScatterPlot #################################################################################
# >>
LineScatterPlot <- R6::R6Class(  "LineScatterPlot",
                            
                            inherit = BasePlot,
                            
                            # Private >> ###########################################################################################
                            
                            private = list(
                                
                                # privMethod = function() { # privMethod >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                #     
                                # }
                                
                            ), # << Private
                            
                            # Public >> ############################################################################################
                            
                            public = list(
                                
                                rows     = NULL,
                                xcolumns = NULL,
                                ycolumns = NULL,
                                ccolumns = NULL,
                                colorbar = list(),
                                target   = NULL,
                                
                                initialize = function( plotType, # initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                       dataFrame     = NULL,
                                                       methodName    = "ParaMonte",
                                                       reportEnabled = TRUE,
                                                       resetPlot     = NULL,
                                                       isGrid        = FALSE,
                                                       isAutocorr    = FALSE,
                                                       isRestart     = FALSE,
                                                       Err ) {
                                    
                                    super$initialize ( plotType      = plotType,
                                                       dataFrame     = dataFrame,
                                                       methodName    = methodName,
                                                       reportEnabled = reportEnabled,
                                                       resetPlot     = resetPlot,
                                                       isGrid        = isGrid,
                                                       Err           = Err )
                                    
                                    private$isGrid     = isGrid
                                    private$isAutocorr = isAutocorr
                                    private$isRestart  = isRestart
                                    
                                    self$reset( isFirstTime = TRUE )
                                    
                                },
                                
                                reset = function( resetType = "soft", isFirstTime = FALSE ) { # reset >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    if ( resetType == "hard" ) {
                                        private$resetPlot( resetType = "hard", plotNames = private$type$name )
                                    } else if ( !isFirstTime ) {
                                        private$Err$note = paste0( "resetting the properties of the ", private$type$name, " plot..." )
                                        super$reset()
                                    }
                                    
                                    lagColumn           = "lag"
                                    countColumn         = "count"
                                    SampleLogFuncColumn = private$dfHeaders[private$offset]
                                    
                                    if ( private$isAutocorr == TRUE ) {
                                        self$xcolumns       = lagColumn
                                        self$ycolumns       = setdiff( private$dfHeaders, c(lagColumn, countColumn) )
                                        self$ccolumns       = NULL
                                        self$legend$enabled = TRUE
                                    } else {
                                        if ( private$isRestart == TRUE ) {
                                            self$ccolumns = countColumn
                                            self$ycolumns = "meanAcceptanceRateSinceStart"
                                        } else {
                                            self$ccolumns = private$dfHeaders[private$offset]
                                            self$ycolumns = private$dfHeaders[private$offset+1]
                                        }
                                        self$xcolumns       = private$dfHeaders[length(private$dfHeaders)]
                                        self$legend$enabled = TRUE
                                    }
                                    
                                    self$colorbar$enabled = TRUE
                                    
                                    self$theme$builtInTheme$reset()
                                    
                                },
                                
                                make = function( reself = FALSE, # make >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                 xcolumns,
                                                 ycolumns,
                                                 target = FALSE,
                                                 ccolumns,
                                                 show = TRUE,
                                                 pointSize,
                                                 lineSize ) {
                                    
                                    isGrid = private$isGrid
                                    
                                    # Plot size in R Notebook >>
                                    
                                    private$setPlotSizeforRNotebook()
                                    
                                    # Set plot themes >>
                                    
                                    plot = private$setPlotTheme()
                                    
                                    # Set geometry >>
                                    
                                    if ( private$type$name == "line" )        { geometry = "path";  size = 0.8 }
                                    if ( private$type$name == "scatter" )     { geometry = "point"; size = 0.9 }
                                    if ( private$type$name == "lineScatter" ) { 
                                        geometry  = "point"
                                        if ( isGrid == TRUE ) { size  = pointSize * 5 / 3 } else { size  = 0.9 }
                                        geometry2 = "path"
                                        if ( isGrid == TRUE ) { size2 = lineSize * 5 / 3 } else { size2 = 0.25 }
                                        color2    = "grey"
                                    }
                                    
                                    # Set ccolumns >>
                                    
                                    if ( !missing(ccolumns) ) { self$ccolumns = ccolumns }
                                    
                                    # Loop for plots >>
                                    
                                    text         = "plot = plot"
                                    showColorbar = self$colorbar$enabled
                                    showLegend   = self$legend$enabled
                                    
                                    if( !missing(xcolumns) ) { self$xcolumns = xcolumns }
                                    if( !missing(ycolumns) ) { self$ycolumns = ycolumns }
                                    
                                    private$checkXcolumnsValues()
                                    private$checkYcolumnsValues()
                                    
                                    for ( ycolumn in self$ycolumns ) {
                                        
                                        xcolumn = self$xcolumns
                                        
                                        if ( private$type$name == "lineScatter" ) {
                                            text  = paste0( text , " + geom_", geometry2, "( aes(x=", xcolumn, ", y=", ycolumn, "), color='",  color2,  "', size=", size2, " )" )
                                        }
                                        
                                        if ( is.null(self$ccolumns) ) {
                                            color = ycolumn # >> legend name
                                            text  = paste0( text , " + geom_", geometry, "( aes(x=", xcolumn, ", y=", ycolumn, ", color='", color, "'), size=", size, ", show.legend = ", showLegend, " )" )
                                            plot  = plot + theme( legend.position   = c(0.8, 0.5) ) +
                                                           theme( legend.background = element_rect( fill = alpha('white', 0.5) ) )
                                        } else {
                                            color = self$ccolumns
                                            text  = paste0( text , " + geom_", geometry, "( aes(x=", xcolumn, ", y=", ycolumn, ", color=",  color,  "), size=", size, ", show.legend = ", showColorbar, " )" )
                                        }
                                    }
                                    eval( parse( text = text ) )
                                    
                                    if ( private$isAutocorr == TRUE ) {
                                     
                                        plot = plot + scale_x_continuous(trans='log10') + aes(ymax = 1)
                                        
                                    }
                                    
                                    # Add Color-bar & Legend >>
                                    
                                    if ( !is.null(self$ccolumns) | self$colormap$enabled == TRUE ) { # Add Color-bar >>
                                        
                                        plot =  plot  + scale_color_gradient( low = private$colorLow, high = private$colorHigh, name = self$ccolumns, guide = "colourbar" ) +
                                                        theme ( legend.key.height  = unit(private$cbarHeight, "inch") ) +
                                                        theme ( legend.title       = element_text(angle = 90), legend.title.align = 0.5 ) +
                                                        guides( color = guide_colourbar(title.position = "right") )
                                        
                                        if ( isGrid == TRUE ) {
                                            gridCbarHeight  = 1.325
                                            plot =  plot  + theme ( legend.key.height  = unit(gridCbarHeight, "inch") ) +
                                                            theme ( legend.title       = element_text(size = 10) ) +
                                                            theme ( legend.text        = element_text(size = 8) )
                                        }
                                        
                                    } else { # Add Color >>
                                        plot = plot + scale_color_manual  ( name = "Variables", values = private$color[ 1 : length(self$ycolumns) ] ) 
                                    }
                                    
                                    # Add xlable & ylable >>
                                    
                                    if ( private$isAutocorr == TRUE ) { ylab = "AotoCorrelation Function \n (ACF) Value" }
                                    else if ( length(self$ycolumns) == 1 ) { ylab = ycolumn }
                                    else { ylab = "Variable Values" }
                                    
                                    plot = plot + labs( x = self$xcolumns, y = ylab )
                                    
                                    # Finalize make method >>
                                    
                                    private$finalizeMake( plot, isGrid, target, show )
                                    
                                    return( invisible( self$currentFig ) )
                                    
                                }
                                
                            ) # << Public
                        
)
# <<
################################## LineScatterPlot #################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# LineScatterPlotObj = LineScatterPlot$new(arg)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################

