
####################################################################################################################################
################################## getDevSize ######################################################################################
# >>
getDevSize  <- function() {
    
    if( is.null(dev.list()) ) { 
        
        # print("No Active Dev")
        # print("Activating rStudioDev")
        dev.new()
        # print("rStudioDev Activated sucessesfully...")
        # print( "Creating new Dev to get its size" )
        dev.new()
        # print( "New Dev created sucessfully to get its size" )
        devSize = dev.size()
        # print( "devSize has been taken" )
        # print( "Now closing new Dev" )
        dev.off()
        # print( "Setting rStudioDev as current Dev" )
        dev.set( 2 )
        # print("rStudioDev is current Dev now.")
        
    } else {
        
        devList = dev.list()
        dev.set( devList[ length(devList) ] )
        # print( paste0( "The current Dev is ", dev.cur() ) )
        # print( "Creating new Dev for new Plot" )
        dev.new()
        # print( "New Dev created sucessfully for new Plot" )
        devSize = dev.size()
        # print( "devSize has been taken" )
        # print( "Now closing new Dev" )
        dev.off()
        # print( "Setting rStudioDev as current Dev" )
        dev.set( 2 )
        # print("rStudioDev is current Dev now.")
        
    }
    
    return( devSize )
    
}
# <<
################################## getDevSize ######################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# devSize = getDevSize()
# <<
################################## Help Code #######################################################################################
####################################################################################################################################


