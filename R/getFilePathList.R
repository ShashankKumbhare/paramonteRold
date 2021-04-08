
####################################################################################################################################
################################## getFilePathList #################################################################################
# >>
getFilePathList  <- function( file, fileType, reportEnabled = TRUE ) {
    
    if ( fileType == "markovchain" ) { fileType = "chain" }
    
    suffix                 = paste("_", fileType , ".txt", sep = "")
    filePathList           = list()
    filePathList$isWebFile = FALSE
    isWebFile              = grepl("http", file, fixed = TRUE)
    
    if ( isWebFile ) {
        
        filePathList$isWebFile = TRUE
        filePathList$files     = file
        filePathList$pattern   = file
        
        
    } else {
    
        # check if the input path (`file`)
        
        isFullPathFile         = file_test( "-f", file )                            # >> check if full file path
        isFullPathFolder       = file_test( "-d", file )                            # >> check if full folder path
        isWorkingDirPathFile   = file_test( "-f", paste0( getwd(), "/", file ) )    # >> check if full working directory path file
        isWorkingDirPathFolder = file_test( "-d", paste0( getwd(), "/", file ) )    # >> check if full working directory folder file
        
        if ( isFullPathFile | isWorkingDirPathFile) {
            
            if ( isWorkingDirPathFile ) { file = paste0( getwd(), "/", file ) }
            
            if ( endsWith(file, suffix) ) { filePathList$files   = file; filePathList$pattern = file }
            else                          { filePathList$files   = NULL; filePathList$pattern = file }
            
        } else if ( isFullPathFolder | isWorkingDirPathFolder ) {
            
            if ( isWorkingDirPathFolder ) { file = paste0( getwd(), "/", file ) }
            folder       = file
            file         = ""
            filePathList = findFiles( folder, file, suffix )
            
        } else { # it is a pattern i.e. a partial file name >>
            
            argFile  = file
            
            isFolder = endsWith( argFile, "/" ) # >> check if folder
            
            if ( isFolder ) { 
                file   = ""
                folder = argFile
            } else {
                file   = tail( strsplit( argFile, '[/]' )[[1]], n = 1 )
                folder = gsub( file, "", argFile, fixed = FALSE )
            }
            
            if ( !grepl(":", argFile, fixed = TRUE) ) { folder = paste0( getwd(), "/", folder ) }
            
            filePathList = findFiles( folder, file, suffix )
            filePathList$pattern = gsub( "//", "/", filePathList$pattern, fixed = FALSE )
            
        }
        
    }
    
    return(filePathList)
    
}
# <<
################################## getFilePathList #################################################################################
####################################################################################################################################


####################################################################################################################################
################################## findFiles #######################################################################################
# >>
findFiles <- function( folder, file, suffix ) {
    
    if ( !endsWith(folder, "/") ) { slash = "/" } else { slash = "" }
    
    filesWithMatchingSuffix         = list.files( path = folder, pattern = suffix )
    filesWithMatchingNamesAndSuffix = grep( file, filesWithMatchingSuffix, value=TRUE )
    files                           = filesWithMatchingNamesAndSuffix
    
    filePathList = list()
    
    if ( length(files) == 0 ) { filePathList$files = files }
    else                      { filePathList$files = paste0( folder, slash, files ) }
    
    filePathList$pattern = paste0( folder, slash, file )
    
    return(filePathList)
    
}
# <<
################################## findFiles #######################################################################################
####################################################################################################################################



####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# filePathList = getFilePathList("Mat", "chain", TRUE)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################
