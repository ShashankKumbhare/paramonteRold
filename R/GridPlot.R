
####################################################################################################################################
################################## GridPlot ########################################################################################
# >>

    # Sourcing Methods / Classes / Auxiliary-functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    # library("gridExtra")

    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Sourcing Methods / Classes / Auxiliary-functions

GridPlot <- R6::R6Class(  "GridPlot",

                            inherit = BasePlot,

                            # Private >> ###########################################################################################

                            private = list(

                                plotObj = list()

                            ), # << Private

                            # Public >> ############################################################################################

                            public = list(

                                rows     = NULL,
                                columns  = NULL,
                                ccolumns = NULL,
                                colorbar = list(),
                                plotType = list(),

                                initialize = function( plotType, # initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                       dataFrame     = NULL,
                                                       methodName    = "ParaMonte",
                                                       reportEnabled = TRUE,
                                                       resetPlot     = NULL,
                                                       Err ) {

                                    super$initialize ( plotType      = plotType,
                                                       dataFrame     = dataFrame,
                                                       methodName    = methodName,
                                                       reportEnabled = reportEnabled,
                                                       resetPlot     = resetPlot,
                                                       Err           = Err )

                                    self$reset( isFirstTime = TRUE )

                                },

                                reset = function( resetType = "soft", isFirstTime = FALSE ) { # reset >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                    if ( resetType == "hard" ) {
                                        private$resetPlot( resetType = "hard", plotNames = private$type$name )
                                    } else if ( !isFirstTime ) {
                                        private$Err$note = paste0( "resetting the properties of the ", private$type$name, " plot..." )
                                        super$reset()
                                    }

                                    # lineScatter plot >>

                                    private$plotObj$lineScatter = LineScatterPlot$new ( plotType   = "lineScatter",
                                                                                        dataFrame  = self$rows,
                                                                                        methodName = private$methodName,
                                                                                        resetPlot  = private$resetPlot,
                                                                                        isGrid     = TRUE,
                                                                                        Err        = private$Err )

                                    # histplot >>

                                    private$plotObj$histplot    = DensityPlot1D$new   ( plotType   = "histplot",
                                                                                        dataFrame  = self$rows,
                                                                                        methodName = private$methodName,
                                                                                        resetPlot  = private$resetPlot,
                                                                                        isGrid     = TRUE,
                                                                                        Err        = private$Err )

                                    # contour >>

                                    private$plotObj$contour     = DensityPlot2D$new   ( plotType   = "contour",
                                                                                        dataFrame  = self$rows,
                                                                                        methodName = private$methodName,
                                                                                        resetPlot  = private$resetPlot,
                                                                                        isGrid     = TRUE,
                                                                                        Err        = private$Err )

                                    # contourf >>

                                    private$plotObj$contourf    = DensityPlot2D$new   ( plotType   = "contourf",
                                                                                        dataFrame  = self$rows,
                                                                                        methodName = private$methodName,
                                                                                        resetPlot  = private$resetPlot,
                                                                                        isGrid     = TRUE,
                                                                                        Err        = private$Err )

                                    self$columns          = private$dfHeaders[private$offset : (length(private$dfHeaders)-1)]

                                    self$ccolumns         = "SampleLogFunc"

                                    self$plotType$upper   = list(value = "lineScatter", enabled = TRUE)
                                    self$plotType$lower   = list(value = "contour",     enabled = TRUE)
                                    self$plotType$diag    = list(value = "histplot",    enabled = TRUE)
                                    self$colorbar$enabled = TRUE

                                },

                                make = function( reself = FALSE, target = FALSE ) { # make >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                    private$plotObj$lineScatter$rows = self$rows
                                    private$plotObj$histplot$rows    = self$rows
                                    private$plotObj$contour$rows     = self$rows
                                    private$plotObj$contourf$rows    = self$rows

                                    miniPlot        = list()
                                    xLabelPlot      = list()
                                    yLabelPlot      = list()
                                    plotNames       = ""
                                    lenGridColNames = length(self$columns)
                                    pointSize       = 0.5  * 5 / (lenGridColNames+1)
                                    lineSize        = 0.25 * 5 / (lenGridColNames+1)
                                    contourSize     = 0.3  * 5 / (lenGridColNames+1)
                                    color           = "white"
                                    panelBorderSize = 0.7
                                    commonColorbar  = NULL
                                    if ( lenGridColNames > 5 ) { labelSize = 2.75 * 5 / (lenGridColNames+1) }
                                    else                       { labelSize = 2.75 }
                                    p    = 0
                                    i    = 0
                                    ylab = 0

                                    for( icolumn in self$columns ) {
                                        i    = i + 1
                                        ylab = ylab + 1

                                        # Plots for Y-labels

                                        yLabelPlot[[ylab]] = ggplot() +  geom_text( aes(0,0), label=icolumn, angle = "90", size = labelSize, fontface = "bold") +
                                                                    theme_void()
                                        yLabelPlotName = eval( parse( text = paste0( "getObjName(yLabelPlot[[", ylab, "]])" ) ) )
                                        plotNames = paste0( plotNames, yLabelPlotName, ", ")

                                        # Plots for points

                                        j = 0
                                        for( jcolumn in self$columns ) {
                                            j = j + 1
                                            p = p + 1
                                            private$Err$note = paste0( "generating subplot #", p, ": [", i, ",", j, "] out of ", lenGridColNames*lenGridColNames )
                                            if( i == j ) { # Diagoonal plots

                                                if ( self$plotType$diag$enabled ) {
                                                    miniPlot[[p]] = private$plotObj$histplot$make   ( xcolumns = icolumn, target = target )
                                                } else {
                                                    miniPlot[[p]] = ggplot() + theme_void()
                                                }

                                            } else if( i < j ) { # Upper triangle plots

                                                if ( self$plotType$upper$enabled ) {
                                                    miniPlot[[p]] = private$plotObj$lineScatter$make( xcolumns = jcolumn, ycolumns = icolumn, target = target, ccolumns = self$ccolumns, pointSize = pointSize, lineSize = lineSize )
                                                    if ( !is.null(self$ccolumns) ) {
                                                        if( is.null(commonColorbar) ) { commonColorbar = grabLegend( miniPlot[[p]] ) }
                                                    }
                                                } else {
                                                    miniPlot[[p]] = ggplot() + theme_void()
                                                }

                                            } else if( i > j ) { # Lower triangle plots

                                                if ( self$plotType$lower$enabled ) {

                                                    if ( self$plotType$lower$value == "contour" ) {
                                                        miniPlot[[p]] = private$plotObj$contour$make(  xcolumns = jcolumn, ycolumns = icolumn, target = target, contourSize = contourSize )
                                                        if ( !is.null(self$ccolumns) ) {
                                                            if( is.null(commonColorbar) ) { commonColorbar = grabLegend( miniPlot[[p]] ) }
                                                        }
                                                    } else if ( self$plotType$lower$value == "contourf" ) {
                                                        miniPlot[[p]] = private$plotObj$contourf$make( xcolumns = jcolumn, ycolumns = icolumn, target = target )
                                                    } else {
                                                        private$Err$abort = "Please enter proper value for 'plotType$lower$value'."
                                                    }

                                                } else {
                                                    miniPlot[[p]] = ggplot() + theme_void()
                                                }

                                            }

                                            # Set axisTextColor = "white" for plots not in 1st column

                                            if( j != 1 ) {
                                                miniPlot[[p]] = miniPlot[[p]] + theme( axis.text.y  = element_text(colour = color) )
                                            } else {
                                                miniPlot[[p]] = miniPlot[[p]] + theme( axis.text.y  = element_text(face = "bold") )
                                            }

                                            # Set axisTextColor = "white" for plots not in last row

                                            if( i != lenGridColNames ) {
                                                miniPlot[[p]] = miniPlot[[p]] + theme( axis.text.x  = element_text(colour = color) )
                                            } else {
                                                miniPlot[[p]] = miniPlot[[p]] + theme( axis.text.x  = element_text(face = "bold") )
                                            }

                                            # Set miniPlots theme

                                            aspectRatio = 1
                                            if ( lenGridColNames > 5 ) { axisTextSize  = 7 * 5 / (lenGridColNames+1) }
                                            else                       { axisTextSize  = 7 }

                                            miniPlot[[p]] = miniPlot[[p]] + theme( aspect.ratio    = aspectRatio ) +
                                                                            theme( axis.title      = element_blank() ) +
                                                                            theme( axis.ticks      = element_blank() ) +
                                                                            theme( legend.position = "none" ) +
                                                                            theme( axis.text       = element_text( size = axisTextSize ) ) +
                                                                            theme( axis.text.y     = element_text(margin = margin(r = -0.1, unit = "cm")),
                                                                                   axis.text.x     = element_text(margin = margin(t = -0.1, unit = "cm")) ) +
                                                                            theme( plot.margin     = margin(0.022, 0.022, 0.022, 0.022, "cm") )

                                            miniPlotName = eval( parse( text = paste0( "getObjName(miniPlot[[", p, "]])" ) ) )
                                            plotNames = paste0( plotNames, miniPlotName, ", ")

                                        }

                                    }

                                    # Blank plot for bottom-left corner position

                                    blankPlot = ggplot() + theme( aspect.ratio = 1 ) + theme_void()
                                    blankPlotName = eval( parse( text = paste0( "getObjName(blankPlot)" ) ) )
                                    plotNames = paste0( plotNames, blankPlotName, ", ")

                                    # Plots for X-labels

                                    xlab = 0
                                    for( icolumn in self$columns ) {

                                        xlab = xlab + 1
                                        xLabelPlot[[xlab]] = ggplot() +  geom_text( aes(0,0), label=icolumn, angle = "0", size = labelSize, fontface = "bold") +
                                                                    theme_void()
                                        xLabelPlotName = eval( parse( text = paste0( "getObjName(xLabelPlot[[", xlab, "]])" ) ) )
                                        plotNames = paste0( plotNames, xLabelPlotName, ", ")

                                    }

                                    # Plot size in R Notebook >>

                                    private$setPlotSizeforRNotebook()
                                    # plotWidth  = 8
                                    # plotHeight = 8
                                    # plotRes    = 208
                                    # options(repr.plot.width = plotWidth, repr.plot.height = plotHeight, repr.plot.res = plotRes)

                                    nrow = lenGridColNames + 1
                                    ncol = nrow
                                    a    = 0.17
                                    b    = 1.00
                                    widths  = paste0( "widths  = c(a", strrep(",b", lenGridColNames ), ")" )
                                    heights = paste0( "heights = c(", strrep("b,", lenGridColNames ), "a)" )

                                    # Plot in new window if RStudio >>

                                    if ( Sys.getenv("RSTUDIO") == "1" ) { newWindowForPlot() }

                                    text    = paste0( "grob = arrangeGrob( ", plotNames, "nrow = ", nrow, ", ncol =", ncol, ", ", widths, ", ", heights, " )" )
                                    # print( text )
                                    # print( lenGridColNames )
                                    # print( nrow )
                                    # print( ncol )
                                    # print( widths )
                                    # print( heights )
                                    eval( parse( text = text ) )

                                    if ( !is.null(self$ccolumns) & !is.null(commonColorbar) ) {
                                        grid.arrange( grob, commonColorbar, nrow = 1, ncol = 2, widths = c(8,1.5) )#, heights = c( , ) )
                                    } else {
                                        grid.arrange( grob )
                                    }

                                    # grid.arrange( grob )

                                    if ( Sys.getenv("RSTUDIO") == "1" ) { setRStudioActive() }

                                    # Return invisible plot >>

                                    return( invisible(grob) )

                                },

                                addTarget = function( ) { # addTarget >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                                    # callerName = getFuncName()
                                    # objectName = callerName[2]                                                       # >> objectName
                                    #
                                    # plotObj    = gsub("plot.*", "\\1", objectName)
                                    # plotObj    = paste0( plotObj, "plot")
                                    #
                                    # text = paste0( "private$plotObj = ", plotObj )
                                    #
                                    # eval( parse( text = text ) )
                                    #
                                    self$make( target = TRUE )

                                }

                            ) # << Public

)
# <<
################################## GridPlot ########################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# GridPlotObj = GridPlot$new(arg)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################

