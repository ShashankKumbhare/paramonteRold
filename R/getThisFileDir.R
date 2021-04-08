
####################################################################################################################################
################################## thisDir #########################################################################################
# >>
getThisFileDir  <- function() {
    frame_files = lapply(sys.frames(), function(x) x$ofile)
    frame_files = Filter(Negate(is.null), frame_files)
    dir         = dirname(frame_files[[length(frame_files)]])

    frame_files = lapply(sys.frames(), function(x) x$ofile)
    frame_files = Filter(Negate(is.null), frame_files)
    PATH        = dirname(frame_files[[length(frame_files)]])

    return(PATH)
}
# <<
################################## thisDir #########################################################################################
####################################################################################################################################


