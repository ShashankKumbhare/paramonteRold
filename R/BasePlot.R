
####################################################################################################################################
################################## BasePlot ########################################################################################
# >>

    # Sourcing Methods / Classes / Auxiliary-functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    library("ggplot2")
    
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Sourcing Methods / Classes / Auxiliary-functions

BasePlot <- R6::R6Class(    "BasePlot",
                            
                            # Private >> ###########################################################################################
                            
                            private = list(
                                
                                type           = list(),
                                dfref          = NULL,
                                methodName     = "",
                                reportEnabled  = NULL,
                                Err            = NULL,
                                isGrid         = FALSE,
                                isAutocorr     = FALSE,
                                isRestart      = FALSE,
                                dfHeaders      = NULL,
                                offset         = NULL,
                                color          = c( "dodgerblue4", "firebrick2", "darkorange2", "darkolivegreen4", "mediumpurple4","deeppink", "goldenrod1",
                                                    "darkred", "darkviolet", "darkolivegreen1", "lightcoral", "mediumorchid1", "navy",
                                                    "seagreen1", "seagreen1", "yellow", "springgreen", "tan2" ),
                                colorHigh      = NULL,
                                colorLow       = NULL,
                                plotWidth      = 7,   # >> For R Notebook only
                                plotHeight     = 4.8, # >> For R Notebook only
                                plotRes        = 200, # >> For R Notebook only
                                cbarHeight     = NULL,
                                builtInTheme   = "theme", # >> default theme is `theme()`, which does nothing
                                builtInThemeList = c("theme_gray", "theme_bw", "theme_linedraw", "theme_light", "theme_dark",
                                                     "theme_minimal", "theme_classic", "theme_void", "theme_test", "reset"),
                                                    # >> `reset` is not a built-in-theme, but added to reset built-in-theme
                                
                                resetPlot      = NULL, # resetPlot >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                
                                updateUserDone = updateUserDone, # updateUserDone >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                
                                addTargetProp = function(ggplot , plotType) { # addTargetProp >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    self$target = Target$new( ggplot = ggplot, plotType = plotType )
                                    
                                    if( target == TRUE ) { plot = self$target$make( isGrid = TRUE ) }
                                    
                                    if( isGrid == FALSE ) { print( plot ) }
                                    
                                    return( invisible(plot) )
                                    
                                },
                                
                                checkXcolumnsValues = function() { # checkXcolumnsValues >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    if ( private$isAutocorr == TRUE ) {
                                        lagColumn   = private$dfHeaders[length(private$dfHeaders)]
                                        if ( is.null(self$xcolumns) ) { self$xcolumns = lagColumn }
                                    } else {
                                        countColumn = private$dfHeaders[length(private$dfHeaders)]
                                        firstVarCol = private$dfHeaders[private$offset+1]
                                        if ( is.null(self$xcolumns) ) {
                                            if ( private$type$isHistplot == TRUE ) { self$xcolumns = firstVarCol }
                                            else { self$xcolumns = countColumn }
                                        }
                                    }
                                    
                                },
                                
                                checkYcolumnsValues = function() { # checkYcolumnsValues >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    if ( private$isAutocorr == TRUE ) {
                                        lagColumn   = private$dfHeaders[length(private$dfHeaders)]
                                        if ( is.null(self$ycolumns) ) { self$ycolumns = lagColumn }
                                    } else {
                                        countColumn = private$dfHeaders[length(private$dfHeaders)]
                                        if ( is.null(self$ycolumns) ) {
                                            if ( private$isRestart == TRUE ) { self$ycolumns = "meanAcceptanceRateSinceStart" }
                                            else                             { self$ycolumns = countColumn }
                                        }
                                    }
                                    
                                },
                                
                                setPlotTheme = function( ) { # setPlotTheme >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    # Aspect ratio >>
                                    
                                    if ( private$type$name == 'contour' | private$type$name == 'contourf' ) {
                                        aspectRatio = 1
                                    } else if ( private$type$name == 'covMat2' | private$type$name == 'corMat2' ) {
                                        aspectRatio = 1
                                    } else{
                                        aspectRatio = 0.7
                                    }
                                    
                                    # Grid color >>
                                    
                                    if ( private$type$name == 'contourf' ) { gridColorMajor  = "white";  gridColorMinor  = "white" }
                                    else                                   { gridColorMajor  = "grey85"; gridColorMinor  = "grey95" }
                                    
                                    # Adding theme layers >>
                                    
                                    if ( typeof(self$rows) == "double" ) {
                                        private$dfref$count = 1 : nrow(private$dfref)
                                        self$rows           = private$dfref[self$rows,]
                                    }
                                    
                                    if ( private$isAutocorr == TRUE ) { plot = ggplot( data = subset(self$rows, count != 1) ) }
                                    else    { plot = ggplot( data = self$rows ) }
                                    
                                    plot = plot  +  # theme( aspect.ratio     = aspectRatio ) +
                                                    theme( plot.background  = element_rect( fill = "white" ) ) +
                                                    theme( panel.grid.major = element_line( size = 0.7, colour = gridColorMajor ),
                                                           panel.grid.minor = element_line( size = 0.7, colour = gridColorMinor ) ) +
                                                    theme( panel.background = element_rect( fill = "white" ) ) +
                                                    theme( panel.border     = element_rect( linetype = "solid", fill = NA  ) ) +
                                                    theme( axis.text        = element_text( size = 10 ),
                                                           axis.title       = element_text( size = 14 ) ) +
                                                    theme( legend.text      = element_text( size =  9 ),
                                                           legend.title     = element_text( size = 12 ) )
                                    
                                    # Set Manual themes >>
                                    
                                    plot = private$applyBuiltInTheme(plot)
                                    
                                    # Aspect Ratio >>
                                    
                                    plot = plot  +  theme( aspect.ratio = aspectRatio )
                                    
                                    # Axes Scale >>
                                    
                                    if ( self$axes$xscale == "log" ) { plot = plot + scale_x_continuous(trans='log10') }
                                    if ( self$axes$yscale == "log" ) { plot = plot + scale_y_continuous(trans='log10') }
                                    
                                    # Colorbar height >>
                                    
                                    if ( Sys.getenv("RSTUDIO") == "1" ) {           # >> For RStudio
                                        
                                        if ( private$type$name == 'contour' | private$type$name == 'contourf' ) {
                                            private$cbarHeight = 0.942 * aspectRatio
                                        } else {
                                            if ( private$isAutocorr == TRUE ) {
                                                private$cbarHeight = 0.865 * aspectRatio
                                            } else {
                                                private$cbarHeight = 0.937 * aspectRatio
                                            }
                                        }
                                        
                                    } else {                                        # >> For R Notebook
                                        
                                        if ( private$type$name == 'contour' | private$type$name == 'contourf' ) {
                                            private$cbarHeight = 0.748 * aspectRatio
                                        } else {
                                            if ( private$isAutocorr == TRUE ) {
                                                private$cbarHeight = 0.887 * aspectRatio
                                            } else {
                                                private$cbarHeight = 0.96 * aspectRatio
                                            }
                                        }
                                        
                                    }
                                    
                                    # Set colors >>

                                    private$setColors()
                                    
                                    return( invisible(plot) )
                                    
                                },
                                
                                setPlotSizeforRNotebook = function() { # setPlotSizeforRNotebook >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    if ( private$type$name == 'grid' ) {
                                        private$plotWidth  = 8
                                        private$plotHeight = 8
                                        private$plotRes    = 208
                                    } else if ( private$type$name == 'covMat2' | private$type$name == 'corMat2' ) {
                                        private$plotWidth  = 11
                                        private$plotHeight = 11
                                        private$plotRes    = 90
                                    }
                                    
                                    options(repr.plot.width = private$plotWidth, repr.plot.height = private$plotHeight, repr.plot.res = private$plotRes)
                                    
                                },
                                
                                setColors = function() { # initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    if ( private$type$isHistplot == FALSE ) {
                                        if ( self$colormap$enabled == FALSE | is.null(self$ccolumns) ) {
                                            self$colormap$enabled = FALSE
                                            self$ccolumns         = NULL
                                        } else {
                                            if ( self$colormap$values == "winter" ) {
                                                private$colorHigh = "#00ff88"
                                                private$colorLow  = "#0300ad"
                                            } else if ( self$colormap$values == "autumn" ) {
                                                private$colorHigh = "#f2ff00"
                                                private$colorLow  = "#ff0000"
                                            } else {
                                                private$Err$warn  = "Please enter valid colormap: 'winter' or 'autumn'"
                                                private$colorHigh = "#f2ff00"
                                                private$colorLow  = "#ff0000"
                                            }
                                        }
                                    }
                                    
                                },
                                
                                applyBuiltInTheme = function(plot) { # setManualTheme >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    text = paste0( "plot = plot + ", private$builtInTheme, "()" )
                                    eval( parse( text = text ) )
                                    
                                    return( plot )
                                    
                                },
                                
                                set_builtInTheme = function() { # set_builtInTheme >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    funcName             = getFuncName()
                                    setPlusThemeName     = funcName[3]
                                    themeName            = gsub( "set_", "", setPlusThemeName, fixed = FALSE )
                                    if ( themeName == "reset" ) { private$builtInTheme = "theme" }
                                    else                        { private$builtInTheme = themeName }
                                    
                                },
                                
                                finalizeMake = function( plot, isGrid, target, show ) { # finalizeMake >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    self$currentFig      = plot
                                    
                                    self$currentFig$show = function() {
                                        
                                        if ( Sys.getenv("RSTUDIO") == "1" ) { plotInNewWindow( self$currentFig ) }
                                        else                                { print( self$currentFig )           }
                                        
                                    }
                                    
                                    # Plot `plot` in new window if running RStudio >>
                                    
                                    if( isGrid == FALSE ) { 
                                        if( show == TRUE ) { self$currentFig$show() }
                                    }
                                    
                                    # Create Target Object as a property >>
                                    
                                    self$target = Target$new( plotObj  = self,
                                                              plotType = private$type$name )
                                    
                                    if( target == TRUE ) { plot = self$target$make( isGrid = TRUE ) }
                                    
                                },
                                
                                reset = function() { # reset >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    df                    = private$dfref
                                    df$count              = 1 : nrow(df)
                                    private$dfHeaders     = colnames(df)
                                    private$offset        = which( colnames(df) == "SampleLogFunc" )
                                    self$rows             = df
                                    self$colormap$values  = "winter"
                                    self$colormap$enabled = TRUE
                                    self$axes$xscale      = 'linear'
                                    self$axes$yscale      = 'linear'
                                    
                                }
                                
                            ), # << Private
                            
                            # Public >> ############################################################################################
                            
                            public = list(
                                
                                rows       = NULL,
                                colormap   = list(),
                                # set        = list(),
                                # figure     = NULL,
                                # axes3d     = list(),
                                legend     = list(),
                                axes       = list(),
                                theme      = list(),
                                currentFig = new.env(),
                                
                                initialize = function( plotType, # initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                       dataFrame     = NULL,
                                                       methodName    = "ParaMonte",
                                                       reportEnabled = TRUE,
                                                       resetPlot     = NULL,
                                                       isGrid        = FALSE,
                                                       Err ) {
                                    
                                    private$dfref     = dataFrame
                                    private$Err       = Err
                                    
                                    private$type$name = plotType
                                    
                                    plotTypeLower     = tolower(plotType)
                                    
                                    private$type$is3d        = grepl("3",         plotTypeLower, fixed = TRUE)
                                    private$type$isLine      = grepl("line",      plotTypeLower, fixed = TRUE)
                                    private$type$isScatter   = grepl("scatter",   plotTypeLower, fixed = TRUE)
                                    private$type$isHeatmap   = grepl("heatmap",   plotTypeLower, fixed = TRUE)
                                    private$type$isKdeplot1  = grepl("kdeplot1",  plotTypeLower, fixed = TRUE)
                                    private$type$isKdeplot2  = grepl("kdeplot2",  plotTypeLower, fixed = TRUE)
                                    private$type$isHistplot  = grepl("histplot",  plotTypeLower, fixed = TRUE)
                                    private$type$isJointplot = grepl("jointplot", plotTypeLower, fixed = TRUE)
                                    private$type$isEllipsoid = grepl("covmat",    plotTypeLower, fixed = TRUE)
                                    private$type$isEllipsoid = grepl("cormat",    plotTypeLower, fixed = TRUE)
                                    private$type$isGridPlot  = grepl("grid",      plotTypeLower, fixed = TRUE)
                                    private$type$isContour3  = grepl("contour3",  plotTypeLower, fixed = TRUE)
                                    private$type$isContourf  = grepl("contourf",  plotTypeLower, fixed = TRUE)
                                    private$type$isContour   = grepl("contour",   plotTypeLower, fixed = TRUE) & !(private$type$isContour3 | private$type$isContourf)
                                    
                                    private$type$is1d        =   private$type$isKdeplot1 | private$type$isDistplot
                                    private$type$is2d        = !(private$type$isGridPlot | private$type$is1d | private$type$is3d)
                                    
                                    private$type$isDiffusionPlot = private$type$isContour | private$type$isContourf | private$type$isContour3
                                    
                                    private$methodName    = methodName
                                    private$reportEnabled = reportEnabled
                                    
                                    private$Err$prefix = methodName
                                    if ( reportEnabled & !isGrid) {
                                        private$Err$note   = paste0("creating a ", private$type$name, " plot object from scratch... ")
                                    }
                                    
                                    private$reset()
                                    
                                    private$resetPlot = resetPlot
                                    
                                    # Add built-in-themes to `theme$builtInTheme$`
                                    
                                    for ( theme in private$builtInThemeList) {
                                        if ( theme != "reset" ) { theme = paste0("set_", theme) }
                                        text = paste0( "self$theme$builtInTheme$", theme, " = private$set_builtInTheme" )
                                        eval( parse( text = text ) )
                                    }
                                    
                                },
                                
                                getLogLinSpace = getLogLinSpace # getLogLinSpace >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                
                            ) # << Public
                            
)
# <<
################################## BasePlot ########################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# BasePlotObj = BasePlot$new(arg)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################


