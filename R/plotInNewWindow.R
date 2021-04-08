
####################################################################################################################################
################################## plotInNewWindow #################################################################################
# >>
plotInNewWindow  <- function(plot) {
    
    if( is.null(dev.list()) ) { 
        
        # print("No Active Dev")
        # print("Activating rStudioDev")
        dev.new()
        # print("rStudioDev Activated sucessesfully...")
        # print( "Creating new Dev for new Plot" )
        dev.new()
        # print( "New Dev created sucessfully for new Plot" )
        print( plot )
        # print( "Setting rStudioDev as current Dev" )
        message = dev.set( 2 )
        # print("rStudioDev is current Dev now.")
        
    } else {
        
        devList = dev.list()
        # print( paste0( "Opening RStudioDev if RStudioDev is not open" ) )
        if ( !is.element( 2, devList ) ) { dev.new() }
        # print( paste0( "RStudioDev has been opened" ) )
        dev.set( devList[ length(devList) ] )
        # print( paste0( "The current Dev is ", dev.cur() ) )
        # print( "Creating new Dev for new Plot" )
        dev.new()
        # print( "New Dev created sucessfully for new Plot" )
        print( plot )
        # print( "Setting rStudioDev as current Dev" )
        message = dev.set( 2 )
        # print("rStudioDev is current Dev now.")
        
    }
    
}
# <<
################################## plotInNewWindow #################################################################################
####################################################################################################################################

# 1 -> noActiveDev
# 2 -> rStudioDev
# 3 -> ----------
# 4 -> popUpDev
# 5 -> popUpDev

####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# plotInNewWindow(plot)
# <<
################################## Help Code #######################################################################################
####################################################################################################################################


