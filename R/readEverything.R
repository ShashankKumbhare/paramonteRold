
####################################################################################################################################
################################## readEverything ##################################################################################
# >>
readEverything <- function( file, arg2, arg3 = TRUE, arg4 = FALSE, delimiter, parseContents, renabled ) {

    # Get objectName & routineName >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    callerName         = getFuncName()
    private$objectName = callerName[2]                                                                               # >> objectName
    routineName        = callerName[3]                                                                              # >> routineName

    # Get & Verify fileType (based on routineName) >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    fileType           = tolower( substr( routineName, 5, 1000 ) )                                                 # >> Get fileType
    private$verifyFileType( fileType )                                                                          # >> Verify fileType

    # Set Input Argument >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    if ( missing(file) ) { file = private$setFileToRead(fileType) }                                   # >> Set fileName (if missing)

    switch( routineName,

        readChain = ,  readSample = , readProgress = , readMarkovChain = {  # >> Set delimiter, parseContents, renabled (if missing)

            if ( missing(arg2) & missing(delimiter) )     { delimiter     = private$setDelimiterToRead(fileType) }# else { delimiter     = arg2 }
            if ( missing(arg3) & missing(parseContents) ) { parseContents = TRUE }                                # else { parseContents = arg3 }
            if ( missing(arg4) & missing(renabled) )      { renabled      = FALSE }                               # else { renabled      = arg4 }

        }, readReport = , readRestart = {                                                        # >> Set renabled only (if missing)

            if ( missing(arg2) & missing(renabled) )      { renabled = FALSE }                                    # else { renabled      = arg2 }

        }, { private$Err$abort = paste0( private$methodName, " routine '", routineName, "' is incorrect. Please Verify.") }

    )

    # Check Input Argument >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    private$checkInputArg( routineName, file, renabled, delimiter, parseContents )

    # Get filePathList >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    filePathList     = getFilePathList( file, fileType, self$reportEnabled )                                       # >> filePathList
    lenFilePathList  = length(filePathList$files)

    msg = paste( lenFilePathList, " ", fileType, " files detected ", "matching the pattern: '", filePathList$pattern, "*'", sep = "" )

    if ( lenFilePathList == 0 ) { private$Err$abort = msg }                                        # >> Abort if lenFilePathList = 0

    private$Err$note = 0;
    private$Err$note = msg                                                                          # >> Print no. of files detected
    private$Err$note = 0; private$Err$note = 0

    # Set outputListName >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    outputListName   = paste0( fileType, "List" )

    # Parse Files >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    outputList       = private$parseEverything( fileType, filePathList$files, renabled, parseContents, delimiter )

    # Return outputList or Make a New Component >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    if ( renabled ) { assign( outputListName, outputList, envir=globalenv() ) }                            # >> Return as a variable

    else {                                                                                                 # >> Make a New Component

        newParaDRAMObj <<- private$ParaDRAMUnlocked$new()
        outputList     <<- outputList

        # bug: ..........

        text = paste0( "if ( !('", outputListName, "' %in% names(", private$objectName, ")) ) {",
                       # "b = setdiff( names(", private$objectName, "), c('clone', 'print') );",
                       private$objectName, "$.__enclos_env__$private$ParaDRAMUnlocked$set('public', '", outputListName, "', outputList);",
                       "newParaDRAMObj[['", outputListName, "']] = outputList; ",
                       "newParaDRAMObj$.__enclos_env__$private$ParaDRAMUnlocked = ", private$objectName, "$.__enclos_env__$private$ParaDRAMUnlocked;",
                       "assign( '", private$objectName, "', newParaDRAMObj, envir = globalenv() );",
                       "lockEnvironment(", private$objectName, ");",
                       "} else {",
                       private$objectName, "[['", outputListName, "']] = outputList; ",
                       "};",
                       "rm(newParaDRAMObj);",
                       "rm(outputList);" )

        eval( parse(text = text), envir = globalenv() )

    }

    # Update User about Success >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    private$updateUserSucess( renabled, fileType, outputListName )

    return( invisible(outputList) )

}
# <<
################################## readEverything ##################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
#
# pmpm$readChain   ( file,
#                    delimiter,
#                    parseContents = TRUE,
#                    renabled      = FALSE )
#
# pmpm$readSample  ( file,
#                    delimiter,
#                    parseContents = TRUE,
#                    renabled      = FALSE )
#
# pmpd$readReport  ( file,
#                    renabled      = FALSE )
#
# pmpd$readRestart ( file,
#                    renabled      = FALSE )
#
# pmpd$readProgress( file,
#                    delimiter,
#                    parseContents = TRUE,
#                    renabled      = FALSE )
#
# <<
################################## Help Code #######################################################################################
####################################################################################################################################




