
####################################################################################################################################
################################## resetPlot #######################################################################################
# >>
resetPlot  <- function( resetType = "soft",
                        plotNames = "all",
                        isRestart = FALSE ) {
    
    # Check if plotNames are elements of private$plotTypeList >>
    
    requestedPlotTypeList = private$checkInputPlotNames( plotNames, className = class(self)[1] )
    
    # Check if resetType is "hard" >>
    
    resetTypeIsHard = private$checkResetType(resetType)
    
    # Reset plots >>
    
    for ( requestedPlotType in requestedPlotTypeList ) {
        
        plotObject = NULL
        requestedPlotTypeLower = tolower(requestedPlotType)
        
        is3d         = grepl("3",         requestedPlotTypeLower, fixed = TRUE)
        isLine       = grepl("line",      requestedPlotTypeLower, fixed = TRUE)
        isScatter    = grepl("scatter",   requestedPlotTypeLower, fixed = TRUE)
        
        isJointplot  = grepl("jointplot", requestedPlotTypeLower, fixed = TRUE)
        isHistplot   = grepl("histplot",  requestedPlotTypeLower, fixed = TRUE)
        isKdeplot1   = grepl("kdeplot1",  requestedPlotTypeLower, fixed = TRUE)
        isKdeplot2   = grepl("kdeplot2",  requestedPlotTypeLower, fixed = TRUE)
        isContourf   = grepl("contourf",  requestedPlotTypeLower, fixed = TRUE)
        isContour3   = grepl("contour3",  requestedPlotTypeLower, fixed = TRUE)
        isContour    = grepl("contour",   requestedPlotTypeLower, fixed = TRUE) & !(isContour3 | isContourf)
        
        isGridPlot   = grepl("grid",      requestedPlotTypeLower, fixed = TRUE)
        
        isLineScatterPlot = isLine | isScatter
        isDensityPlot     = isJointplot | isHistplot | isKdeplot1 | isKdeplot2 | isContourf | isContour3 | isContour
        
        if ( resetTypeIsHard == FALSE) {
            plotComponent = self$plot
            plotObject    = plotComponent$requestedPlotType
            plotObject$reset()
        }
        
        # Resetting Plots >>
        
        if ( resetTypeIsHard == TRUE ) {
            
            # Reset line / scatter plot >>
        
            if ( isLineScatterPlot ) {
                
                plotObject = LineScatterPlot$new (  plotType      = requestedPlotType,
                                                    dataFrame     = self$df,
                                                    methodName    = private$methodName,
                                                    reportEnabled = private$reportEnabled,
                                                    resetPlot     = private$resetPlot,
                                                    isRestart     = isRestart,
                                                    Err           = private$Err )
                
            }
            
            # Reset histplot >>
            
            if ( isHistplot ) {
                
                plotObject = DensityPlot1D$new( plotType      = requestedPlotType,
                                                dataFrame     = self$df,
                                                methodName    = private$methodName,
                                                reportEnabled = private$reportEnabled,
                                                resetPlot     = private$resetPlot,
                                                Err           = private$Err )
                
            }
            
            # Reset contour / contourf >>
            
            if ( isContour | isContourf) {
                
                plotObject = DensityPlot2D$new( plotType      = requestedPlotType,
                                                dataFrame     = self$df,
                                                methodName    = private$methodName,
                                                reportEnabled = private$reportEnabled,
                                                resetPlot     = private$resetPlot,
                                                Err           = private$Err )
                
            }
            
            # Reset grid >>
            
            if ( isGridPlot ) {
                
                plotObject = GridPlot$new     ( plotType      = requestedPlotType,
                                                dataFrame     = self$df,
                                                methodName    = private$methodName,
                                                reportEnabled = private$reportEnabled,
                                                resetPlot     = private$resetPlot,
                                                Err           = private$Err )
            
            }
        
        }
        
        self$plot[[requestedPlotType]] = plotObject
        
    }
    
}
# <<
################################## resetPlot #######################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# resetPlot( resetType, plotNames )
# <<
################################## Help Code #######################################################################################
####################################################################################################################################


