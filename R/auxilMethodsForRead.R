
####################################################################################################################################
################################## Auxililary Methods for 'readEverything' #########################################################
####################################################################################################################################


####################################################################################################################################
################################## verifyFileType ##################################################################################
# >>
verifyFileType  <- function( fileType ) {

    if ( !is.element(fileType, list("chain", "sample", "report", "restart", "progress", "markovchain")) ) {     # >> Verify fileType

        private$website$github$issues$url = "https://www.cdslab.org/paramonte/"          # >> SET TEMPORARITY xxx NEEDS ATTENTION <<

        private$Err$abort = paste0( "Internal error occurred. The input fileType is not recognized.\n",
                                    "Please report this error at: \n \n ",
                                    private$Err$tab, private$website$github$issues$url )

    }

}
# <<
################################## verifyFileType ##################################################################################
####################################################################################################################################


####################################################################################################################################
################################## setFileToRead ###################################################################################
# >>
setFileToRead  <- function( fileType ) {

    fileSuffix = fileType

    if ( is.null(self$spec$outputFileName ) ) {
        # file = getwd()
        file = ""
        if ( self$reportEnabled ) {
            private$Err$warn = 0
            private$Err$warn =  paste( "The ``file`` is neither given as input to ``read", capitalize(fileType), "()`` \n",
                                       "nor set as a simulation specification of the ", self$methodName, " object. \n",
                                       "This information is essential, otherwise how could the output files be found? \n",
                                       "All that is needed is the unique name (including path) of the simulation name \n",
                                       "shared among its output files or simply, the path to the specific ", fileSuffix, "\n",
                                       "file to be read. For now, the ", self$methodName, " sampler will search \n",
                                       "the current working directory for simulation output files that match the \n",
                                       "filename pattern of ", fileSuffix, " files.", sep = "" )

        }
    } else {
        file = self$spec$outputFileName
    }

    return(file)

}
# <<
################################## setFileToRead ###################################################################################
####################################################################################################################################


####################################################################################################################################
################################## setDelimiterToRead ##############################################################################
# >>
setDelimiterToRead  <- function( fileType ) {

    fileSuffix  = fileType

    if ( is.null(self$spec$outputDelimiter ) ) {

        delimiter = ","

        if ( self$reportEnabled ) {

            private$Err$warn = 0
            private$Err$warn = paste0( "The ", private$methodName, " input simulation specification `", private$objectName,
                                       "$spec$outputDelimiter` is not set.", "\n",
                                       "This information is essential for successful reading of the requested ", fileType, " file(s).", "\n",
                                       "Proceeding with the default assumption of comma-delimited ", fileType, " file contents..." )
        }

    }
    else {

        delimiter = self$spec$outputDelimiter

    }

    return(delimiter)

}
# <<
################################## setDelimiterToRead ##############################################################################
####################################################################################################################################


####################################################################################################################################
################################## checkInputArg ###################################################################################
# >>
checkInputArg  <- function( routineName, file, renabled, delimiter, parseContents ) {

    errFound = FALSE
    tab      = private$Err$tab

    if ( !is.character(file) ) { errFound = TRUE }                                                              # >> Check arg(file)


    switch( routineName,
            readChain = ,  readSample = , readProgress = {
                if ( !is.character(delimiter) ) { errFound = TRUE }                                             # >> Check delimiter
            }, readReport = , readRestart = {
                delimiter = ""; parseContents = logical(0) # >> setting dummy args since missing
            } )

    if ( errFound ) {

        errMsg = paste0( "'", routineName, "' takes only two input string arguments (file, delimiter). Correct usage:", "\n\n",
                         tab, private$objectName, "$", routineName, "(file, delimiter)", "\n\n",
                         "where `file` is the name of the file to be read, `delimiter` is the delimiter used in the file." )

    }

    if ( errFound == FALSE ) {

        if ( !is.logical(parseContents) ) { errFound = TRUE; errArg = "parseContents" }                     # >> Check parseContents
        if ( !is.logical(renabled) )      { errFound = TRUE; errArg = "renabled" }                               # >> Check renabled

        if ( errFound ) { errMsg = paste0( "Input argument '", errArg, "' to routine ", routineName, " is of boolean type. Please fix and rerun." ) }

    }

    if ( errFound ) { private$Err$abort = errMsg }                                                         # >> Abort if error found

}
# <<
################################## checkInputArg ###################################################################################
####################################################################################################################################


####################################################################################################################################
################################## updateUserProcessing ############################################################################
# >>
updateUserProcessing  <- function( file, fileType ) {

    private$Err$note = paste( "processing ", fileType, " file: '", file, "'", sep = "" )

    private$Err$note = paste( "reading the file contents...", sep = "" )

}
# <<
################################## updateUserProcessing ############################################################################
####################################################################################################################################


####################################################################################################################################
################################## updateUserDone ##################################################################################
# >>
updateUserDone  <- function( ) { private$Err$note = paste( "Done.", sep = "" ) }
# <<
################################## updateUserDone ##################################################################################
####################################################################################################################################


####################################################################################################################################
################################## updateUserSucess ################################################################################
# >>
updateUserSucess  <- function( renabled, fileType, outputListName ) {

    tab = private$Err$tab

    if ( renabled ) {

        outputListFullName = outputListName

        msg = paste( "The processed ", fileType, " files are now stored in the output variable as a ", "\n",
                     "R list. For example, to access the contents of the first (or the only) ", "\n",
                     fileType, " file stored in an output variable named ", outputListFullName, ", try:", sep = "" )

    } else {

        outputListFullName = paste( private$objectName, "$", outputListName, sep = "" )

        msg = paste( "The processed ", fileType, " files are now stored in the newly-created", "\n",
                     "component `", outputListName, "` of the ", private$methodName, " object as a R list.", "\n",
                     "For example, to access the contents of the first (or the only) ", fileType, " file, try:", sep = "" )

    }

    if ( fileType == "progress" | fileType == "restart" ) {

        specials = ""

    } else {

        specials = paste( tab, outputListFullName, "[[1]]$plot$histplot$make()     # to make 2D line plots.", "\n",
                          tab, outputListFullName, "[[1]]$plot$contour$make()      # to make 2D line plots.", "\n",
                          tab, outputListFullName, "[[1]]$plot$contourf$make()     # to make 2D line plots.", "\n",
                          tab, outputListFullName, "[[1]]$plot$grid$make()         # to make GridPlot.", "\n",
                          sep = "" )                     # >> NEED TO IMPLEMENT MORE SPECIALS xxx <<

    }

    if ( fileType == "chain" | fileType == "markovchain" | fileType == "sample" | fileType == "progress" | fileType == "restart" ) {

        if ( fileType == "restart" ) {

            forRestartOnly = paste( tab, outputListFullName, "[[1]]$covMat",  "\n",
                                    tab, outputListFullName, "[[1]]$corMat",  "\n",
                                    sep = "" )

            forRestartPlot = paste( tab, outputListFullName, "[[1]]$plot$covMat2$make()      # to make proposal covariance evolution 2D plots.", "\n",
                                    tab, outputListFullName, "[[1]]$plot$corMat2$make()      # to make proposal correlation evolution 2D plots.", "\n",
                                    sep = "" )

            forAutoCorr    = ""

        } else {

            forRestartOnly = ""
            forRestartPlot = ""
            forAutoCorr    = paste( "To plot or inspect the variable autocorrelations or the correlation/covariance matrices, try:", "\n",
                                    "\n",
                                    tab, outputListFullName, "[[1]]$stats$<PRESS TAB TO SEE THE LIST OF COMPONENTS>", "\n",
                                    sep = "" )

        }

        private$Err$box  =  paste(  msg, "\n",
                                    "\n",
                                    tab, outputListFullName, "[[1]]$df", "\n",
                                    forRestartOnly,
                                    "\n",
                                    "To access the plotting tools, try:", "\n",
                                    "\n",
                                    tab, outputListFullName, "[[1]]$plot$<PRESS TAB TO SEE THE LIST OF PLOTS>", "\n",
                                    "\n",
                                    "For example,", "\n",
                                    "\n",
                                    tab, outputListFullName, "[[1]]$plot$line$make()         # to make 2D line plots.", "\n",
                                    tab, outputListFullName, "[[1]]$plot$scatter$make()      # to make 2D scatter plots.", "\n",
                                    tab, outputListFullName, "[[1]]$plot$lineScatter$make()  # to make 2D line-scatter plots.", "\n",
                                    forRestartPlot,
                                    specials,
                                    "\n",
                                    forAutoCorr,
                                    "\n",
                                    "For more information and examples on the usage, visit:", "\n",
                                    "\n",
                                    tab, private$website$home$url, sep = "" )
        private$Err$note = 0

    } else if ( fileType == "report" ) {

        private$Err$box = paste( msg, "\n",
                                 "\n",
                                 "    ", outputListFullName, "[[1]]$contents            # to print the contents of the report file.", "\n",
                                 "\n",
                                 "To access the simulation statistics and information, examine the contents of the", "\n",
                                 "components of the following structures:", "\n",
                                 "\n",
                                 tab, outputListFullName, "[[1]]$setup               # to get information about the simulation setup.", "\n",
                                 tab, outputListFullName, "[[1]]$stats$time          # to get the timing information of the simulation.", "\n",
                                 tab, outputListFullName, "[[1]]$stats$chain         # to get the statistics of the simulation output sample.", "\n",
                                 tab, outputListFullName, "[[1]]$stats$numFuncCall   # to get information about the number of function calls.", "\n",
                                 tab, outputListFullName, "[[1]]$stats$parallelism   # to get information about the simulation parallelism.", "\n",
                                 tab, outputListFullName, "[[1]]$spec                # to get the simulation specification in the report file.", "\n",
                                 "\n",
                                 "For more information and examples on the usage, visit:", "\n",
                                 "\n",
                                 tab, private$website$home$url, sep = "" )


    } else if ( fileType == "restart" ) { }

}
# <<
################################## updateUserSucess ################################################################################
####################################################################################################################################



