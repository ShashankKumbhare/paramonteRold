
####################################################################################################################################
################################## ReportFileContents ##############################################################################
# >>
ReportFileContents <- R6::R6Class(  "ReportFileContents",
                                    
                            inherit = OutputFileContents,
                            
                            # Private >> ###########################################################################################
                            
                            private = list(
                                
                                dsym        = '****',                                                         # >> decoration symbol
                                prefix      = NULL,
                                dsymLen     = NULL,
                                indentLen   = 8,                                                   # >> indent length of the records
                                lineCounter = NULL,
                                lineListLen = NULL,
                                lineList    = NULL,
                                
                                reportParseFailure = function(topic) {
                                    private$Err$warn = paste( "Failed to parse the record '", topic, "'. ", sep = "" )
                                    private$Err$warn = paste( "The structure of the report file appears to have been compromised. Skipping... ", sep = "" )
                                },
                                
                                parseSection = function(topic) {
                                    lineCounterLastSuccess = private$lineCounter
                                    section                = NULL
                                    topicFound             = FALSE
                                    while( TRUE ){
                                        private$lineCounter = private$lineCounter + 1
                                        if( private$lineCounter >= private$lineListLen ) { break }
                                        record = private$lineList[[private$lineCounter]]
                                        if( grepl(topic, record, fixed = TRUE) ) {
                                            topicFound = TRUE
                                            break
                                        }
                                    }
                                    if( topicFound ) {
                                        private$skipCurrentSectionHeader()
                                        lineStart = private$lineCounter
                                        private$stopBeforeNextSectionHeader()
                                        section   = paste( private$lineList[lineStart : private$lineCounter], collapse = "\n" )
                                        section = noquote( paste0( unlist( strsplit(section, "\n") ) ) )
                                    } else {
                                        private$reportParseFailure(topic);
                                        private$lineCounter = lineCounterLastSuccess
                                    }
                                    
                                },
                                
                                skipCurrentSectionHeader = function() {
                                    while (TRUE){
                                        record = private$lineList[[private$lineCounter]]
                                        
                                        if ( nchar(record) > private$dsymLen & grepl(private$dsym, substr(record,1,private$dsymLen), fixed = TRUE) ) {
                                            private$lineCounter = private$lineCounter + 1
                                            if (private$lineCounter == private$lineListLen) { break }
                                        } else {
                                            break
                                        }
                                    }
                                },
                                
                                stopBeforeNextSectionHeader = function() {
                                    record                 = private$lineList[[private$lineCounter]]
                                    isCurrentSectionHeader = nchar(record) > private$dsymLen & grepl(private$dsym, substr(record,1,private$dsymLen), fixed = TRUE)
                                    while (TRUE) {
                                        if (private$lineCounter == private$lineListLen){ break }
                                        record = private$lineList[[private$lineCounter]]
                                        if ( nchar(record) > private$dsymLen & grepl(private$dsym, substr(record,1,private$dsymLen), fixed = TRUE) ) {
                                            if (isCurrentSectionHeader) {
                                                private$lineCounter = private$lineCounter + 1
                                                next
                                            }
                                            private$lineCounter    = private$lineCounter - 1
                                            break
                                        } else {
                                            isCurrentSectionHeader = FALSE
                                            private$lineCounter    = private$lineCounter + 1
                                            next
                                        }
                                    }
                                    
                                },
                                
                                parseStats = function() {
                                    
                                    if (private$methodName == "ParaDRAM" || private$methodName == "MatDRAM") { # >>
                                        
                                        while( TRUE ) { # >>
                                            
                                            private$lineCounter = private$lineCounter + 1
                                            if (private$lineCounter > private$lineListLen) { break }
                                            item                = private$lineList[[private$lineCounter]]
                                            
                                            if( private$isstats(item) ) { # >>
                                                
                                                # parse the value >>
                                                
                                                valueIsNumeric = TRUE
                                                valueFound     = FALSE
                                                descFound      = FALSE
                                                value          = ""
                                                desc           = ""
                                                
                                                while( TRUE ) {
                                                    
                                                    private$lineCounter = private$lineCounter + 1
                                                    if (private$lineCounter > private$lineListLen) { break }

                                                    record = private$lineList[[private$lineCounter]]
                                                    
                                                    # check the record is not another item or desc is not found before value >>
                                                    
                                                    if( private$isstats(record) ) {
                                                        private$lineCounter = private$lineCounter - 1
                                                        break
                                                    }
                                                    # <<
                                                    
                                                    # parse the value/description >>
                                                    
                                                    if( nchar(record) > private$indentLen ) {
                                                        recordIsDesc = private$isdesc(record)
                                                        if (recordIsDesc) {
                                                            if(!valueFound) {
                                                                private$reportMissingValue(item)
                                                                break
                                                            }
                                                            descFound  = TRUE
                                                            desc       = paste( desc, " ", gsub( private$prefix, " ", private$lineList[[private$lineCounter]] ) ) # >> remove prefix, trim, append
                                                        } else if(!descFound) {
                                                            valueFound = TRUE
                                                            value      = paste(value, record)
                                                            if( nchar(record) == 0 ) {
                                                                valueIsNumeric = FALSE
                                                            }
                                                        }
                                                    }
                                                    # <<
                                                    
                                                } # end while
                                                
                                                if( valueFound & descFound ) {
                                                    if (valueIsNumeric) {
                                                        
                                                        valIndent = paste( rep(" ", 9), collapse = '', sep = "" )  # >> 1 + 8(indent)
                                                        value     = gsub( valIndent, "", value )
                                                        
                                                        desc      = gsub("\\\\" , "/", desc)
                                                        desIndent = paste( rep(" ", 11), collapse = '', sep = "" ) # >> 1 + 8(indent) + 2
                                                        desc      = gsub( desIndent, "", desc )
                                                        desc      = gsub( "ParaDRAM  ", "", desc )
                                                        desc      = gsub( "MatDRAM  ", "", desc )
                                                        
                                                        components = private$getComponents(item)      # >> individual splitted components in item
                                                        private$addComponents(components)             # >> adding components to self$stats
                                                        item       = private$replaceDotWithDollar(item)
                                                        
                                                        if( value == "UNDEFINED" ) { 
                                                            eval( parse( text = paste0("self$", item, "$value ='", value, "'") ) )
                                                        } else {
                                                            tryCatch (
                                                                { value = as.numeric(value) },
                                                                warning = function(w) { },                # >> Do nothing 
                                                                error = function(e) { },                  # >> Do nothing
                                                                finally = {
                                                                    if( is.numeric(value) ) {
                                                                        eval( parse( text = paste0("self$", item, "$value = value") ) )
                                                                        eval( parse( text = paste0("self$", item, "$discription = noquote( '", desc, "' )") ) )
                                                                    } else {
                                                                        eval( parse( text = paste0("self$", item, " = NULL") ) )
                                                                    }
                                                                }
                                                            ) # << try catch
                                                        } # << if
                                                    } # << if
                                                }
                                            } # << if 
                                        } # << while
                                    } # << if 
                                },
                                
                                isstats = function(record) {
                                    result = nchar(record) > 5 & substr(record,1,6) == "stats."
                                    return(result)
                                },
                                
                                isdesc = function(record) {
                                    result = grepl(private$prefix, record, fixed = TRUE)
                                    return(result)
                                },
                                
                                reportMissingValue = function(topic) {
                                    self$Err$warn  = paste("Failed to parse the value corresponding to the record '", topic, "'. ", "The structure of the report file appears to have been compromised. Skipping... ")
                                },
                                
                                replaceDotWithDollar = function(text) {
                                    splitText = strsplit(text, "")
                                    splitTextLen = 1 : length(splitText[[1]])
                                    for (i in splitTextLen) {
                                        if (splitText[[1]][i] == ".") {
                                            splitText[[1]][i] = "$"
                                        }
                                    }
                                    joinSplitText = paste(splitText[[1]], collapse = '')
                                    return(joinSplitText)
                                },
                                
                                getComponents = function(text) {
                                    splitText    = strsplit(text, "")
                                    splitTextLen = 1 : length(splitText[[1]])
                                    Components   = c()
                                    dummy        = ""
                                    k = 1
                                    for (i in splitTextLen) {
                                        if (splitText[[1]][i] == ".") {
                                            Components[k] = dummy
                                            k = k + 1
                                            dummy = ""
                                        } else {
                                            dummy = paste0(dummy, splitText[[1]][i])
                                        }
                                    }
                                    Components[k] = dummy
                                    return(Components)
                                },
                                
                                addComponents = function(components) {
                                    obj = "self"
                                    componentsLen = 1 : length(components)
                                    for (i in componentsLen) {
                                        
                                        key = components[i]
                                        if ( eval( parse( text = paste0("key %in% names(", obj, ")") ) ) ) {
                                            # print("it's there")
                                        } else {
                                            
                                            eval( parse( text = paste0(obj, "[[key]] = list()") ) )
                                            # print("Inserted new empty list")
                                        }
                                        obj = paste0(obj,"[[\"",key,"\"]]")
                                    }
                                }
                                
                            ), # << Private
                            
                            # Public >> ############################################################################################
                            
                            public = list(
                                    
                                contents    = NULL,
                                setup       = NULL,
                                stats       = NULL,
                                spec        = NULL,
                                
                                initialize = function( file, # Initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                       methodName,
                                                       reportEnabled,
                                                       Err ) {
                                    
                                    super$initialize( file, methodName, reportEnabled, Err )
                                    
                                    setup = list()
                                    stats = list()
                                    spec  = list()
                                    
                                    private$dsymLen     = nchar(private$dsym)                                 # >> Decoration symbol
                                    private$prefix      = paste(self$methodName, " - NOTE:", sep = "")
                                    
                                    self$contents       = readLines(file)                                              # >> Get Data
                                    private$lineList    = self$contents
                                    private$lineListLen = length(private$lineList)
                                    
                                    # Read the banner >>
                                    
                                    lineStartFound      = FALSE
                                    private$lineCounter = 0
                                    while (TRUE) {
                                        private$lineCounter = private$lineCounter + 1
                                        
                                        if (private$lineCounter >= private$lineListLen) {break}
                                        
                                        record = private$lineList[[private$lineCounter]]
                                        
                                        if (lineStartFound){
                                            if (!grepl(private$dsym, record, fixed = TRUE)){
                                                private$lineCounter = private$lineCounter - 1
                                                break
                                            }
                                        } else {
                                            if (grepl(private$dsym, record, fixed = TRUE)){
                                                lineStartFound     = TRUE
                                                lineStart          = private$lineCounter
                                            }
                                        }
                                    }
                                    
                                    if (lineStartFound){
                                        self$setup$library$banner = paste( private$lineList[lineStart : private$lineCounter], collapse = "\n" )
                                        self$setup$library$banner = noquote( unlist( strsplit( self$setup$library$banner, "\n" ) ) )
                                    } else {
                                        private$reportParseFailure("ParaMonte banner")
                                    }
                                    
                                    # Read the ParaMonte library interface specifications >>
                                    self$setup$library$interface = private$parseSection("ParaMonte library interface specifications")
                                    
                                    # Read the Runtime platform specifications >>
                                    self$setup$platform = private$parseSection("Runtime platform specifications")
                                    
                                    # Read the simulation environment >>
                                    self$setup$io = private$parseSection("simulation environment")
                                    
                                    # Read the simulation specifications >>
                                    self$spec = private$parseSection("simulation specifications")
                                    
                                    # Statistics: this must be always the last item to parse >>
                                    private$parseStats()
                                    
                                    self$contents = noquote(self$contents)
                                    
                                    return( self )
                                    
                                }
                                
                            ) # << Public
                            
)
# <<
################################## ReportFileContents ##############################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# ReportFileContentsObj = ReportFileContents$new( file, methodName, Err )
# <<
################################## Help Code #######################################################################################
####################################################################################################################################


