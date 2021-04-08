
####################################################################################################################################
################################## HeatMapPlot #####################################################################################
# >>

    # Sourcing Methods / Classes / Auxiliary-functions >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    # source( paste( getwd(), "/", "paramonte/vis/BasePlot.R", sep = "" ) )

    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Sourcing Methods / Classes / Auxiliary-functions

HeatMapPlot <- R6::R6Class( "HeatMapPlot",
                            
                            inherit = BasePlot,
                            
                            # Private >> ###########################################################################################
                            
                            private = list(
                                
                                colorStart  = NULL,
                                colorCount  = NULL,
                                colorEnd    = NULL,
                                isdryrun    = NULL,
                                df          = NULL,
                                
                                reset = function() { # reset >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    # super$reset()
                                    
                                    self$heatmap$enabled = TRUE
                                    self$heatmap$kws     = list( vmin = NULL, vmax = NULL, title = NULL )
                                    
                                    self$xticklabels$kws = list()
                                    self$yticklabels$kws = list()
                                    
                                    self$columns         = ""
                                    
                                    private$colorStart   = 20
                                    private$colorCount   = 200
                                    private$colorEnd     = 220
                                    
                                    self$annotPrecision  = 2
                                    
                                    # private$isdryrun = TRUE
                                    # self$make()
                                    # private$isdryrun = FALSE
                                    
                                }
                                
                            ), # << Private
                            
                            # Public >> ############################################################################################
                            
                            public = list(
                                
                                heatmap        = list(),
                                xticklabels    = list(),
                                yticklabels    = list(),
                                columns        = "",
                                annotPrecision = NULL,
                                
                                initialize = function( plotType,  # initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
                                    
                                    private$reset()
                                    if ( is.null(resetPlot) ) { private$resetPlot = private$reset }
                                    private$updateUserDone()
                                    
                                },
                                
                                make = function( reself = FALSE ) { # make >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    objectName = getFuncName()
                                    
                                    # plotObject = objectName[2] >>
                                    eval( parse( text = paste0("plotObject = ", objectName[2]) ) )
                                    
                                    col   = colorRampPalette(c("#944723", "#FFFFFF", "#2d717d"))
                                    vmin  = plotObject$heatmap$kws$vmin
                                    vmax  = plotObject$heatmap$kws$vmax
                                    title = paste( "\n", plotObject$heatmap$kws$title )
                                    
                                    corrplot (  corr          = private$dfref,
                                                method        = "color",
                                                col           = col(100),
                                                is.corr       = FALSE,
                                                title         = paste( "\n", plotObject$heatmap$kws$title ),
                                                tl.col        = "black",
                                                cl.lim        = c( vmin, vmax ),
                                                cl.align.text = "l" )
                                    
                                }
                                
                            ) # << Public
                         
)
# <<
################################## HeatMapPlot #####################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# HeatMapPlotObj = HeatMapPlot$new(arg)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################

