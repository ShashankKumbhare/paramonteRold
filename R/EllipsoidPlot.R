
####################################################################################################################################
################################## EllipsoidPlot ###################################################################################
# >>

    # Sourcing Methods / Classes / Auxiliary-functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    library("mvtnorm")

    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Sourcing Methods / Classes / Auxiliary-functions

EllipsoidPlot <- R6::R6Class(  "EllipsoidPlot",
                            
                            inherit = BasePlot,
                            
                            # Private >> ###########################################################################################
                            
                            private = list(
                                
                                dimPairMatrix = NULL,
                                dimPairMean   = NULL
                                
                                # priMethod2 = function(arg, ...) { # priMethod2 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                #     
                                #     return( invisible(self) )
                                #     
                                # }
                                
                            ), # << Private
                            
                            # Public >> ############################################################################################
                            
                            public = list(
                                
                                matrix        = NULL,
                                dimensionPair = NULL,
                                centerColumn  = NULL,
                                ccolumns      = NULL,
                                colorbar      = list(),
                                target        = NULL,
                                
                                initialize = function(  matrix, # initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                        plotType,
                                                        dataFrame     = NULL,
                                                        methodName    = "ParaMonte",
                                                        reportEnabled = TRUE,
                                                        resetPlot     = NULL,
                                                        isGrid        = FALSE,
                                                        Err ) {
                                    
                                    super$initialize (  plotType      = plotType,
                                                        dataFrame     = dataFrame,
                                                        methodName    = methodName,
                                                        reportEnabled = reportEnabled,
                                                        resetPlot     = resetPlot,
                                                        isGrid        = isGrid,
                                                        Err           = Err )
                                    
                                    self$matrix = matrix
                                    
                                    self$reset()
                                    
                                },
                                
                                reset = function() { # reset >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    private$Err$note = paste0( "resetting the properties of the ", private$type$name, " plot..." )
                                    
                                    super$reset()
                                    
                                    self$dimensionPair  = c(1, 2)
                                    
                                    private$dimPairMean = c()
                                    
                                    self$centerColumn = "meanVec"
                                    
                                    self$ccolumns         = 'SampleLogFunc'
                                    self$legend$enabled   = FALSE
                                    self$colorbar$enabled = TRUE
                                    
                                },
                                
                                make = function( reself = FALSE, # make >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                 target = FALSE,
                                                 ccolumns,
                                                 pointSize,
                                                 show = TRUE,
                                                 lineSize ) {
                                    
                                    # Plot size for R Notebook >>
                                    
                                    private$setPlotSizeforRNotebook()
                                    
                                    # Set plot themes >>
                                    
                                    plot = private$setPlotTheme()
                                    
                                    showColorbar = self$colorbar$enabled
                                    
                                    # Set center of ellipses >>
                                    
                                    if ( is.null(self$centerColumn) ) {
                                        
                                        meanVec = self$rows$meanVec
                                        for ( i in 1: length(meanVec) ) {
                                            private$dimPairMean[[i]] = rep( 0, length(unlist( self$rows$meanVec[[1]] )) )
                                        }
                                    } else {
                                        
                                        meanVec = self$rows$meanVec
                                        
                                        for ( i in 1: length(meanVec) ) {
                                            private$dimPairMean[[i]] = unlist( self$rows$meanVec[[i]] )
                                        }
                                    }
                                    
                                    # Loop for ellipses >>
                                    
                                    
                                    i = self$dimensionPair[1]
                                    j = self$dimensionPair[2]
                                    
                                    private$dimPairMatrix = self$matrix[ c(i,j), c(i,j), c(self$rows$count) ]
                                    
                                    lenDimPairMatrix = length(private$dimPairMatrix[1,1,])
                                    
                                    ellipseSeq       = self$getLogLinSpace(1.07)
                                    color            = colorRampPalette(c("#0022ff", "#00ff88"))( lenDimPairMatrix )
                                    
                                    n = 100
                                    for ( k in ellipseSeq ) {
                                        set.seed(1)
                                        mean = private$dimPairMean[[k]][c(i,j)]
                                        x    = rmvnorm(n, mean=mean, sigma = private$dimPairMatrix[ , ,k])
                                        data = data.frame(x)
                                        plot = plot + stat_ellipse( data = data, aes(x=X1, y=X2), type = "t", level = 0.53, color = color[k], size = 1.2, show.legend = showColorbar )
                                    }
                                    
                                    # Add Color-bar & Legend >>
                                    
                                    if ( !is.null(self$ccolumns) ) {
                                        # Add Color-bar >>
                                        
                                        plot =  plot + scale_color_gradient( low = "#0022ff", high = "#00ff88", name = self$ccolumns, guide = "colourbar" )# +
                                            # theme ( legend.key.height  = unit(private$cbarHeight, "inch") ) +
                                            # theme ( legend.title       = element_text(angle = 90), legend.title.align = 0.5 ) +
                                            # guides( color = guide_colourbar(title.position = "right") )
                                        
                                    } else {
                                        # Add Color >>
                                        # plot = plot + scale_color_manual  ( name = "Variables", values = private$color[ 1 : length(self$ycolumns) ] )
                                        plot = plot + scale_color_manual  ( name = "Variables", values = color )
                                    }
                                    
                                    # Add xlable & ylable >>
                                    
                                    xlab = paste0( "Dimension ", i )
                                    ylab = paste0( "Dimension ", j )
                                    
                                    plot = plot + labs( x = xlab, y = ylab )
                                    
                                    # # Create Target Object as a property >>
                                    # 
                                    # self$target = Target$new( ggplot   = plot,
                                    #                           plotType = private$type$name )
                                    # 
                                    # if( target == TRUE ) { plot = self$target$make() }
                                    # 
                                    # # Plot plot in new window if running RStudio
                                    # 
                                    # if ( show == TRUE ) {
                                    #     
                                    #     if ( Sys.getenv("RSTUDIO") == "1" ) { plotInNewWindow( plot ) }
                                    #     else                                { print( plot )           }
                                    #     
                                    # }
                                    # 
                                    # return( invisible(plot) )
                                    
                                    # Finalize make method >>
                                    
                                    private$finalizeMake( plot, isGrid = FALSE, target, show )
                                    
                                    return( invisible( self$currentFig ) )
                                    
                                }
                                
                            ) # << Public
                         
)
# <<
################################## EllipsoidPlot ###################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# EllipsoidPlotObj = EllipsoidPlot$new(arg)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################



