
####################################################################################################################################
################################## checkInputPlotNames #############################################################################
# >>
checkInputPlotNames  <- function( plotNames, className ) {
    
    plotTypeLower = tolower(plotNames)
    
    if ( is.character(plotNames) & NCOL(plotNames) == 1 ) {
        
        if ( NROW(plotNames) == 1 ) {
            if ( plotTypeLower == "all" ) {
                requestedPlotTypeList = private$plotTypeList
            } else if ( is.element(plotTypeLower, tolower(private$plotTypeList)) ) {
                requestedPlotTypeList = grep( paste0("\\b", plotTypeLower, "\\b"), private$plotTypeList, ignore.case = TRUE, value=TRUE )
            } else {
                private$reportWrongPlotName(plotNames, className)
            }
        } else {
            requestedPlotTypeList = c()
            i = 0
            for ( plotName in plotTypeLower ) {
                i = i + 1
                if ( is.element(plotName, tolower(private$plotTypeList)) ) {
                    requestedPlotTypeList[i] = grep( paste0("\\b", plotName, "\\b"), private$plotTypeList, ignore.case = TRUE, value=TRUE )
                } else {
                    private$reportWrongPlotName(plotName, className)
                }
            }
        }
        
    } else {
        
        private$reportWrongPlotName("a non-string non-1d-vector object.", className)
        
    }
    
    return( requestedPlotTypeList )
    
}
# <<
################################## checkInputPlotNames #############################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# requestedPlotTypeList = checkInputPlotNames(plotNames)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################


