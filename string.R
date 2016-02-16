strsplit0 <- function(strVec, split, simplify = TRUE) {
    # Split a string vector with multiple split
    #
    # Args:
    #   strVec: One vector whose elements are to be split.
    #   split: character vector containing regular expression(s) to use for splitting.
    #   simplify: If TRUE, returns character vector; if not, list; Default is TRUE.
    #
    # Returns:
    #   vector or list of split characters.
    if (simplify == TRUE) {
        strsplit0 <- c()
        for (sp in split) {
            strVec <- strVecSplit(strVec, sp)
        }
        strsplit0 <- strVec
        return(strsplit0)
    } else {
        strsplit0 <- list()
        for (str in strVec) {
            for (sp in split) {
                str <- strVecSplit(str, sp)
            }
            strsplit0[[length(strsplit0) + 1]] <- str
        }
        return(strsplit0)
    }
}
strVecSplit <- function(strVec, split) {
    strVecSplit <- c()
    for (str in strVec) {
        unlist      <- unlist(strsplit(str, split = split))
        strVecSplit <- c(strVecSplit, unlist)
    }
    return(strVecSplit)
}  # Used by strsplit0()
