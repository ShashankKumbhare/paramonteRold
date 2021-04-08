
####################################################################################################################################
################################## getLogLinSpace ##################################################################################
# >>
getLogLinSpace  <- function( base, skip = 1, lowerLim = 1, upperLim = nrow(self$rows) ) {
    
    LogLinSpace = c()
    
    i = 1
    LogLinSpace[i] = lowerLim
    while ( TRUE ) {
        i = i + 1
        if ( lowerLim + base^(i-1) > upperLim ) {
            break
        } else {
            LogLinSpace[i] = floor( lowerLim + base^(i-1) )
        }
    }
    
    LogLinSpace = unique(LogLinSpace)
    
    LogLinSpace = LogLinSpace[ seq(1,length(LogLinSpace),skip) ]
    
    # a = lowerLim
    # b = upperLim
    # 
    # LogLinSpace    = c()
    # LogLinSpace[1] = b
    # 
    # i = 1
    # while ( b > a ) {
    #     
    #     i = i + 1
    #     
    #     LogLinSpace[i] = ceiling( (b-a) / base )
    #     
    #     b = LogLinSpace[i]
    #     
    # }
    # 
    # LogLinSpace = rev(LogLinSpace)
    # 
    # LogLinSpace = LogLinSpace[ seq(1,length(LogLinSpace),skip) ]
    
    return( LogLinSpace )
    
}
# <<
################################## getLogLinSpace ##################################################################################
####################################################################################################################################


####################################################################################################################################
################################## Help Code #######################################################################################
# >>
# LogLinSpace = getLogLinSpace( base, lowerLim, upperLim )
# <<
################################## Help Code #######################################################################################
####################################################################################################################################

# clear a
# base = 5;
# i    = 1;
# a    = zeros(10,1);
# a(i) = 1;
# 
# while true
#     i = i + 1;
#     if base^(i-1) > 10000, break; else, a(i) = base^(i-1); end
# end
# 
# 
# 
# clear a
# base = 10;
# i    = 1;
# a    = zeros(10,1);
# a(i) = 1;
# 
# while true
# i = i + 1;
# if base^(i-1) > 10000, break; else, a(i) = base^(i-1); end
# end
# 
# 
# 
# 
# clear a
# base = 1.1;
# i    = 0;
# a    = zeros(10,1);
# 
# while true
# i = i + 1;
# if base^(i-1) > 100, break; else, a(i) = ceil( base^(i-1) ); end
# end
# 
# a = unique(a')

