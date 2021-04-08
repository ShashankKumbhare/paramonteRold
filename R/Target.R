
####################################################################################################################################
################################## Target ##########################################################################################
# >>
Target <- R6::R6Class(  "Target",
                            
                            # Private >> ###########################################################################################
                            
                            private = list(
                                
                                plotType     = NULL,
                                plotTypeObj  = NULL,
                                plotObj      = NULL,
                                df           = NULL,
                                histColumns  = NULL,
                                rawPlot      = NULL,
                                layeredPlot  = NULL,
                                hlineLayer   = list(),
                                vlineLayer   = list(),
                                layerCounter = NULL,
                                counter      = 0,
                                legPar       = new.env(),
                                
                                getLegPar = function( lineType, aes_param ) { # getLegPar >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    if( strsplit(lineType, "")[[1]][1] == "h" ) { key = "h"; layerType = "hlineLayer" }
                                    if( strsplit(lineType, "")[[1]][1] == "v" ) { key = "v"; layerType = "vlineLayer" }
                                    
                                    legPar = paste0( key, aes_param, "s" ) # << Ex. "hsizes", "vlinetypes"
                                    
                                    layerLength = length( private[[layerType]] )
                                    private$legPar[[legPar]] = c()
                                    
                                    for ( i in 1 : layerLength ) {
                                        
                                        private$legPar[[legPar]] = append( private$legPar[[legPar]], private[[layerType]][[i]]$aes_params[[aes_param]] )
                                        
                                    }
                                    
                                    return( invisible(private$legPar[[legPar]]) )
                                    
                                },
                                
                                getLegPar2 = function( aes_param ) { # getLegPar >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    layerLength = length( private$hlineLayer )
                                    legPar = c()
                                    
                                    for ( i in 1 : layerLength ) {
                                        legPar = append( legPar, private$hlineLayer[[i]]$aes_params[[aes_param]] )
                                        legPar = append( legPar, private$vlineLayer[[i]]$aes_params[[aes_param]] )
                                    }
                                    
                                    return( invisible(legPar) )
                                    
                                }
                                
                            ), # << Private
                            
                            # Public >> ############################################################################################
                            
                            public = list(
                                
                                axhline    = list(),
                                axvline    = list(),
                                scatter    = list(),
                                values     = NULL,
                                
                                initialize = function( plotObj, plotType, histColumns = NULL ) { # initialize >>>>>>>>>>>>>>>>>>>>>
                                    
                                    private$plotObj     = plotObj
                                    private$rawPlot     = private$plotObj$clone()
                                    private$plotType    = plotType
                                    private$df          = ggplot_build(private$plotObj$currentFig)$data
                                    private$histColumns = histColumns
                                    
                                    self$reset()
                                    
                                },
                                
                                reset = function() { # reset >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    self$axhline$enabled = TRUE
                                    self$axvline$enabled = TRUE
                                    self$scatter$enabled = TRUE
                                    
                                    self$axhline$color = "#ff3300"
                                    self$axvline$color = "#ff3300"
                                    self$scatter$color = "#ff3300"
                                    
                                    if ( private$plotType == "histplot" ) {
#                                        self$axhline$enabled = FALSE
                                        self$axhline$enabled = TRUE
                                        self$scatter$enabled = FALSE
                                    }
                                    
                                    # if ( private$plotType == "histplot" ) {
                                        # self$values = mean( private$histColumns )
                                        # self$values = mean( private$df[[1]]$x )
                                    # } else {
                                        self$values = c( mean(private$df[[1]]$x), mean(private$df[[1]]$y) )
                                    # }
                                    
                                },
                                
                                make = function( values, color, isGrid = FALSE, show = TRUE ) { # make >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    # Check NA & NULL values of self$values >>
                                    
                                    if ( !missing(color) )  { self$color  = color }
                                    
                                    if ( !missing(values) ) {
                                        
                                        if( is.null(values[1]) ) { self$values[1] = NA } else { self$values[1] = values[1] }
                                        if( is.null(values[2]) ) { self$values[1] = NA } else { self$values[2] = values[2] }
                                        
                                    }
                                    
                                    if( is.null(self$values[1]) ) { 
                                        self$values[1] = NA
                                        self$values[2] = NA
                                    }
                                    
                                    if( is.null(self$values[2]) ) { self$values[2] = NA }
                                    
                                    # Add target plots >>
                                    
                                    xintercept = self$values[1] # << these variables are essential, because if we don't do this and pass self$values[1] to geom_hline / geom_vline directly,
                                    yintercept = self$values[2] # << the previous plot will get updated and we cannot add the previous and new one (since both will be same)
                                    
                                    hlinetype  = "solid"
                                    vlinetype  = "solid"
                                    
                                    hcolor     = self$axhline$color
                                    vcolor     = self$axvline$color
                                    
                                    private$layerCounter = length( private$plotObj$currentFig$layers )
                                    j = private$layerCounter
                                    
                                    private$counter = private$counter + 1
                                    i = private$counter
                                    
                                    if ( abs(yintercept) < 1 ) { yinterceptStr = sprintf(yintercept, fmt = '%#.4e') } else { yinterceptStr = sprintf(yintercept, fmt = '%#10.2e') }
                                    if ( abs(xintercept) < 1 ) { xinterceptStr = sprintf(xintercept, fmt = '%#.4e') } else { xinterceptStr = sprintf(xintercept, fmt = '%#10.2e') }
                                    
                                    
                                    
                                    halpha = paste0( "", j+1, ": ( --, ", yinterceptStr, ")" )
                                    valpha = paste0( "", j+2, ": (", xinterceptStr, ", -- )" )
                                    
                                    if ( self$axhline$enabled & !is.na(yintercept) ) {
                                        
                                        private$hlineLayer[[i]] = geom_hline( aes(yintercept = yintercept, alpha = halpha), linetype = hlinetype, colour = hcolor, size = 0.8, show.legend = TRUE )
                                        
                                    }
                                    
                                    if ( self$axvline$enabled & !is.na(xintercept) ) {
                                        
                                        # private$vlineLayer[[i]] = geom_vline( aes(xintercept = xintercept, alpha = valpha), linetype = vlinetype, colour = vcolor, size = 0.8, show.legend = FALSE )
                                        private$vlineLayer[[i]] = geom_vline( aes(xintercept = xintercept, alpha = valpha), linetype = vlinetype, colour = vcolor, size = 0.8, show.legend = TRUE )
                                        
                                    }
                                    
                                    # if ( self$scatter$enabled & !is.na(xintercept) & !is.na(yintercept) ) {
                                    #     targetPlot$currentFig = targetPlot$currentFig + geom_point( aes(x = xintercept, y = yintercept), colour = self$scatter$color, size = 2, na.rm = TRUE, show.legend = TRUE )
                                    # }
                                    
                                    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    # Adding hlineLayers & vlineLayers >>
                                    
                                    private$layeredPlot = private$rawPlot$currentFig
                                    
                                    for ( i in 1 : length(private$hlineLayer) ) {
                                        
                                        private$layeredPlot = private$layeredPlot + private$hlineLayer[[i]] + private$vlineLayer[[i]]
                                        
                                    }
                                    
                                    private$legPar$alphaValues = rep(1, 2*length(private$hlineLayer))
                                    # private$legPar$shapeValues = rep(1, length(private$vlineLayer))
                                    
                                    targetPlot = private$plotObj
                                    
                                    targetPlot$currentFig = private$layeredPlot
                                    
                                    if ( private$plotType != "histplot" ) {
                                        
                                        targetPlot$currentFig = targetPlot$currentFig +
                                                                guides( color              = guide_colourbar(title.position = "top") ) +           # << for colorbar
                                                                theme ( legend.key.height  = unit(4, "mm") ) +                                     # << for colorbar
                                                                theme ( legend.title       = element_text(angle = 0), legend.title.align = 0.5 ) + # << for colorbar
                                                                theme ( legend.title       = element_text(angle = 0), legend.title.align = 0.5 )   # << for colorbar
                                        
                                    }
                                    # } else if ( private$plotType == "histplot"  ) {
                                        
                                        targetPlot$currentFig = targetPlot$currentFig + theme( legend.direction = "vertical" )
                                        
                                    # }
                                    
                                    if ( Sys.getenv("RSTUDIO") == "1" ) { targetPlot$currentFig = targetPlot$currentFig + theme ( legend.position = "bottom" ); ncol = 2 } else { ncol = 1 } # << all legends at the bottom
                                    
                                    targetPlot$currentFig$show <- function() {
                                        
                                        tempPlot =  targetPlot$currentFig + 
                                                    scale_alpha_manual( name   = "hlines & vlines",
                                                                        values = private$legPar$alphaValues,
                                                                        guide  = guide_legend( override.aes = list( size     = private$getLegPar2("size"),
                                                                                                                    color    = private$getLegPar2("colour"),
                                                                                                                    linetype = private$getLegPar2("linetype") ),
                                                                                               # direction      = "verticle",
                                                                                               ncol           = ncol,
                                                                                               title.position = "top",
                                                                                               # label.position = "bottom",
                                                                                               keywidth       = unit(5, "mm"),
                                                                                               keyheight      = unit(5, "mm"),
                                                                                               label.hjust    = 0.25,
                                                                                               label.vjust    = 0.1,
                                                                                               title.theme    = element_text( size  = 15,
                                                                                                                              angle = 0 ),
                                                                                               label.theme    = element_text( size  = 9,
                                                                                                                              angle = 0 ) ) )
                                        
                                        if ( Sys.getenv("RSTUDIO") == "1" ) { plotInNewWindow( tempPlot ) }
                                        else{
                                            options(repr.plot.width = 8.5, repr.plot.height = 4.8 )
                                            print( tempPlot )
                                        }
                                        
                                        return( invisible( tempPlot ) )
                                        
                                    }
                                    
                                    # Plot plot in new window if running RStudio >>
                                    
                                    private$plotObj = targetPlot
                                    
                                    if( isGrid == FALSE ) { 
                                        if( show == TRUE ) { targetPlot$currentFig$show() }
                                    }
                                    
                                    # if( isGrid == FALSE ) { targetPlot$currentFig$show() }
                                    
                                    return( invisible( private$plotObj$currentFig ) )
                                    
                                }
                                
                            ) # << Public
                         
)
# <<
################################## Target ##########################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# TargetObj = Target$new(plotObj, plotType, histColumns = NULL)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################



