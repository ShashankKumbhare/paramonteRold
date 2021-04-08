
####################################################################################################################################
################################## parseEverything #################################################################################
# >>
parseEverything  <- function( fileType, filePathList, renabled, parseContents, delimiter ) {
    
    outputList = list()
    
    lenFilePathList  = length(filePathList)
    for ( i in 1 : lenFilePathList ) { outputList[[i]] = list() }

    i = 0
    for ( file in filePathList ) {
        
        i = i + 1
        
        if ( self$reportEnabled ) {                                                                       # >> Update UserProcessing
            private$updateUserProcessing( file, fileType )
            if ( fileType == "report" ) {
                private$Err$note = paste( "parsing the ", fileType, " file contents...", "", sep = "" )
            }
        }
        
        if ( fileType == "chain" | fileType == "markovchain" | fileType == "sample" | fileType == "progress" ) {
            
            outputList[[i]] = TabularFileContents$new( filePathList[i], fileType, delimiter, private$methodName, parseContents, self$reportEnabled, private$Err )
            
        } else if ( fileType == "report" ) {
            
            outputList[[i]] = ReportFileContents$new( filePathList[i], private$methodName, self$reportEnabled, private$Err )
            
        } else if ( fileType == "restart" ) {
            
            outputList[[i]] = RestartFileContents$new( filePathList[i], private$methodName, self$reportEnabled, private$Err )
            
        }
        
        private$updateUserDone()                                                                             # >> Update User "Done"
        
        if ( fileType == "chain" | fileType == "markovchain" | fileType == "sample" ) {
            
            if ( fileType == "sample" ) { offSet = 1 }   # offSet : index of the column corresponding to the dimension 1 of the sample space
            else                        { offSet = 7 }
            
            # self$count = nrow(self$df)
            # self$ncol  = ncol(self$df)
            # 
            # self$count = nrow(self$df)
            # self$ncol  = ncol(self$df)
            
            ndim  = length( outputList[[i]]$df[1,] ) - offSet
            count = length( outputList[[i]]$df[,1] )
            
            private$Err$note = paste( "ndim = ", ndim, ", count = ", count, sep = "" )
            
        }
        
        private$Err$note = 0
    
    }
    
    return( outputList )
    
}
# <<
################################## parseEverything #################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# private$parseEverything( fileType, filePathList, renabled, parseContents, delimiter )
# <<
################################## Help Code #######################################################################################
####################################################################################################################################


