#==================================================================
# Determining the data type
#==================================================================
ag_datatype_vector <- function ( y )
{  # Masanao's routine, as is
  # should be changed so that it  returns an object of type 'data_type'.
  #
    if ( is.null ( y ) ) { "NULL" } 
    else {
        if (all(as.integer(y)==y, na.rm=TRUE)) return("discrete")
        values <- as.numeric ( names ( table ( y ) ) )
        len    <- length ( values )
        if ( len == 1 )      { "fixed" }
        else if ( len == 2 ) { "dichotomous" }
        else {
            if ( is.numeric ( y ) == TRUE ) {
                if ( len > 2 & len <= 5 ) { "ordered-categorical" }
                else if ( len > 5 & all ( values > 0 ) ) { "logscale-continuous" }
                else if ( len > 5 & "0" %in% values ) { "mixed" }
                else { "continuous" }
            }
            else { "unordered-categorical" }
        }
    }
}
