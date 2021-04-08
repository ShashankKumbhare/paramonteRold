
####################################################################################################################################
################################## OutputFileContents ##############################################################################
# >>
OutputFileContents <- R6::R6Class(  "OutputFileContents",
                            
                            # Private >> ###########################################################################################
                            
                            private = list(
                                
                                reportEnabled = NULL,
                                methodName    = NULL,
                                timer         = NULL,
                                Err           = NULL,
                                
                                updateProgress = function(fraction, ...) { # updateProgess >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                    
                                    if ( private$reportEnabled ) {
                                        
                                        chars    = c('|', '/', '-', '\\')
                                        tab = private$Err$tab
                                        gap = paste0( rep(tab, 14), collapse = '' )
                                        # bigSpace = paste0( tab, tab, tab, tab, tab, tab, tab, tab, tab )
                                        
                                        if ( !exists('clockCounter') ) {
                                            clockCounter <<- 1
                                        } else {
                                            clockCounter <<- clockCounter %% 4 + 1
                                        }
                                        
                                        if ( fraction < 1 ) { clockTick = chars[clockCounter] }
                                        else {
                                            eval( parse( text = 'rm(clockCounter)' ), envir = .GlobalEnv )
                                            clockTick = chars[1]
                                            gap = paste0( rep(tab, 8), collapse = '' )
                                            msg = paste0(  gap, sprintf('%6.0f',100*fraction), "%" )
                                            private$Err$note = msg
                                            return(invisible(NULL))
                                        }
                                        
                                        msg = paste0( gap, clockTick, sprintf('%3.0f',100*fraction), "%" )
                                        # Sys.sleep(0.05)
                                        cat(msg, " \r")
                                        
                                        # private$Err$note = msg
                                        # flush.console()
                                        
                                    }
                                }
                                
                            ), # << Private
                            
                            # Public >> ############################################################################################
                            
                            public = list( # Public >>
                                
                                file       = NULL,
                                
                                initialize = function( file, # initialize >> #######################################################
                                                       methodName,
                                                       reportEnabled,
                                                       Err ) {
                                    
                                    self$file             = file
                                    private$Err           = Err
                                    private$methodName    = methodName
                                    private$reportEnabled = reportEnabled;
                                }
                                
                            ) # << Public
                            
)
# <<
################################## OutputFileContents ##############################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# OutputFileContentsObj = OutputFileContents$new(file , methodName, Err)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################


