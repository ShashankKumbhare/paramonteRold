
####################################################################################################################################
################################## DensityPlot2D ###################################################################################
# >>
DensityPlot2D <- R6::R6Class(  "DensityPlot2D",
                            
                            inherit = BasePlot,
                            
                            # Private >> ###########################################################################################
                            
                            private = list(
                                
                                # cbarHeight = NULL
                                
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
                                    self$ycolumns         = private$dfHeaders[private$offset+2]
                                    self$ccolumns         = "density"
                                    self$colorbar$enabled = TRUE
                                    
                                    self$theme$builtInTheme$reset()
                                    
                                },
                                
                                make = function( reself      = FALSE, # make >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                 xcolumns,
                                                 ycolumns,
                                                 target      = FALSE,
                                                 show = TRUE,
                                                 contourSize = NULL ) {
                                    
                                    isGrid = private$isGrid
                                    
                                    # Plot size in R Notebook >>
                                    
                                    private$setPlotSizeforRNotebook()
                                    
                                    # Set plot themes >>
                                    
                                    plot = private$setPlotTheme()
                                    
                                    # Set geometry >>
                                    
                                    geometry = 'density_2d'
                                    
                                    if ( isGrid == TRUE ) { size = contourSize * 5 / 3 } else { size = 1.1 }
                                    
                                    if ( private$type$name == 'contourf' ) { binwidth = 0.003 }
                                    else if ( isGrid == TRUE )             { binwidth = 0.015 }
                                    else                                   { binwidth = 0.008 }
                                    
                                    # plot points >>
                                    
                                    text       = "plot = plot"
                                    showLegend = self$colorbar$enabled
                                    
                                    if( !missing(xcolumns) ) { self$xcolumns = xcolumns }
                                    if( !missing(ycolumns) ) { self$ycolumns = ycolumns }
                                    
                                    private$checkXcolumnsValues()
                                    private$checkYcolumnsValues()
                                    
                                    if (private$type$name == 'contour' ) {
                                        text = paste0( text , " + geom_", geometry, "( aes(x=", self$xcolumns, ", y=", self$ycolumns, ", colour = after_stat(level)), size=", size, ", show.legend = ", showLegend, ", binwidth = binwidth )" )
                                        eval( parse( text = text ) )
                                        if ( is.null(self$ccolumns) ) {
                                            private$colorHigh  = private$colorLow
                                        }
                                        plot = plot + scale_color_gradient( name = "Density", low = private$colorLow, high = private$colorHigh, guide = "colorbar", space = "Lab" )
                                    }
                                    
                                    if (private$type$name == 'contourf' ) {
                                        text = paste0( text , " + stat_", geometry, "( aes(x=", self$xcolumns, ", y=", self$ycolumns, ", fill = ..level..), geom='polygon', show.legend = ", showLegend, ", binwidth = binwidth )" )
                                        eval( parse( text = text ) )
                                        plot = plot + scale_fill_continuous( name = "Density", high = "#0051a3", low = "#FFFFFF")
                                    }
                                    
                                    # color-bar height

                                    plot = plot + theme( legend.key.height = unit(private$cbarHeight, "inch") )

                                    if ( isGrid == TRUE ) {
                                        gridCbarHeight  = 1.325
                                        plot =  plot  + theme ( legend.key.height  = unit(gridCbarHeight, "inch") ) +
                                                        theme ( legend.title       = element_text(size = 10) ) +
                                                        theme ( legend.text        = element_text(size = 8) )
                                    }
                                    
                                    # Add Color-bar & Legend >>
                                    
                                    # if ( !is.null(self$ccolumns) ) { # Add Color-bar >>
                                    #     
                                    #     if ( is.null(self$ccolumns) ) {  private$colorHigh  = private$colorLow }
                                    #     
                                    #     plot =  plot +  scale_color_gradient( name = "Density", low = private$colorLow, high = private$colorHigh, guide = "colourbar", space = "Lab"  ) +
                                    #                     theme ( legend.key.height  = unit(private$cbarHeight, "inch") )
                                    # 
                                    #     if ( isGrid == TRUE ) {
                                    #         gridCbarHeight  = 1.325
                                    #         plot =  plot  + theme ( legend.key.height  = unit(gridCbarHeight, "inch") ) +
                                    #                         theme ( legend.title       = element_text(size = 10) ) +
                                    #                         theme ( legend.text        = element_text(size = 8) )
                                    #     }
                                    # 
                                    # }
                                    # else { # Add Color >>
                                    # 
                                    #     plot = plot + scale_color_manual  ( name = "Variables", values = private$color[ 1 : length(self$ycolumns) ] )
                                    # }
                                    
                                    # Finalize make method >>
                                    
                                    private$finalizeMake( plot, isGrid, target, show )
                                    
                                    return( invisible( self$currentFig ) )
                                    
                                }
                                
                            ) # << Public
                            
)
# <<
################################## DensityPlot2D ###################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# DensityPlot2DObj = DensityPlot2D$new(arg)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################

