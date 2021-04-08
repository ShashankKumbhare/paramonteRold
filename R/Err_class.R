
####################################################################################################################################
################################## Err_class #######################################################################################
# >>
Err_class <- R6::R6Class(   "Err_class",

                            # Private >> ###########################################################################################

                            private = list(

                                msg        = NULL,
                                msgDash    = " - ",
                                fullprefix = "",
                                funcType   = NULL,
                                msgType    = NULL,
                                msgColon   = " : ",
                                lenOfLine  = 133,
                                boxLen     = 80,

                                checkAndWriteMsg = function() { # checkAndWriteMsg >> ##############################################
                                    private$checkMsg()
                                    private$writeMsg()
                                },

                                checkMsg = function() { # checkMsg >> ##############################################################

                                    if( is.character(private$msg) )                             { private$msgType = "character" }
                                    else if( is.double(private$msg) | is.integer(private$msg) ) { private$msgType = "double"    }
                                    else { stop( paste("Please provide either character/string or double/integer message for ", funcType, sep = ""), call. = FALSE) }

                                },

                                writeMsg = function() { # writeMsg >> ##############################################################

                                    private$msg = gsub("\n", " \n ", private$msg)

                                    if( self$prefix == "" ) {
                                        private$fullprefix = paste( self$tab, private$funcType, private$msgColon, sep = "" )
                                    } else {
                                        private$fullprefix = paste( self$tab, self$prefix, private$msgDash, private$funcType, private$msgColon, sep = "" )
                                    }

                                    if( private$msgType == "character" ) {
                                        lines = private$getListOfLines(private$msg)
                                        for( i in 1:length(lines) ) {
                                            msg   = paste(private$fullprefix, lines[i], sep = "")
                                            writeLines(msg)
                                        }
                                    }
                                    else {
                                        if( private$msg < 0 ) {
                                            stop( paste("Please provide non-negetive integer.", funcType, sep = ""), call. = FALSE)
                                        } else if( private$msg == 0 ) {
                                            writeLines("")
                                        } else {
                                            for( i in 1:private$msg ) { writeLines(private$fullprefix) }
                                        }
                                    }

                                },

                                getListOfLines = function(text) { # writeMsg >> ####################################################

                                    words = unlist( strsplit(text, " ") )
                                    lines = c()
                                    l = 0; w = 0

                                    while( w < length(words) ) {
                                        l = l + 1
                                        w = w + 1
                                        if( words[w] == "\n" ) { lines[l] = ""; next}
                                        else{ lines[l] = words[w]}
                                        while( nchar(lines[l]) <= private$lenOfLine & w < length(words) ) {
                                            w = w + 1
                                            if( words[w] == "\n" ) { break }
                                            else{ lines[l] = paste( lines[l], words[w], sep = " " ) }
                                        }
                                    }

                                    return(lines)

                                }

                            ), # << Private

                            # Active >> ############################################################################################

                            active = list(

                                note  = function(msg) { # note >> ##################################################################

                                    private$msg = msg; private$funcType = "NOTE";    private$checkAndWriteMsg()

                                },

                                warn  = function(msg) { # warn >> ##################################################################

                                    private$msg = msg; private$funcType = "WARNING"; private$checkAndWriteMsg()

                                },

                                abort = function(msg) { # abort >> #################################################################

                                    self$note = 0
                                    private$msg = msg; private$funcType = "FATAL";   private$checkAndWriteMsg()
                                    self$note = 0
                                    private$msg = "Aborting..."; private$funcType = "FATAL"; private$checkAndWriteMsg()
                                    self$note = 0
                                    self$note = 0
                                    exit()

                                },

                                box   = function(msg) { # box >> ###################################################################

                                    self$note = 0
                                    self$note = paste0( rep(">", private$boxLen), collapse = '' )
                                    self$note = 1
                                    self$note = msg
                                    self$note = 1
                                    self$note = paste0( rep("<", private$boxLen), collapse = '' )
                                    self$note = 0

                                }

                            ), # << Active

                            # Public >> ############################################################################################

                            public = list(

                                # Public fields >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                prefix     = "",
                                tab        = "    ",

                                # initialize >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                initialize = function(prefix = self$prefix, tab = self$tab) {

                                    self$prefix = prefix
                                    self$tab    = tab

                                },

                                # print >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                print = function() {

                                    self$prefix = "Err_class"
                                    tab         = self$tab

                                    self$box    = paste("Usage:", "\n",
                                                        "\n",
                                                        tab, "Instantiation:", "\n",
                                                        tab, tab, "Err = Err_class$new()", "\n",
                                                        "\n",
                                                        tab, "Set Prefix (optional):", "\n",
                                                        tab, tab, "Err$prefix = 'Shashank'", "\n",
                                                        "\n",
                                                        tab, "Methods for displaying Notes, Warnings, Fatal error", "\n",
                                                        tab, tab, "Err$note  = 'your text'", "\n",
                                                        tab, tab, "Err$warn  = 'your text'", "\n",
                                                        tab, tab, "Err$abort = 'your text'", "\n", sep = "")

                                    self$prefix = ""

                                }

                            ) # << Public

)
# <<
################################## Err_class #######################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# Err = Err_class$new()
# Err$note = "dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text dummy text "
# Err$note = "dummy text dummy text \n dummy text dummy text \ndummy text\n dummy text dummy text dummy text\ndummy text dummy text dummy text dummy text dummy text \n dummy text "
# Err$note = 0
# Err$note = 1
# Err$note = 2
# <<
################################## Help Code #######################################################################################
####################################################################################################################################

