##' Extract only part of a string, given a separator
##'
##' Given a string with a separator (e.g., "Subject 001", where the separator is a space), this function can be used to extract only whatever follows the separator (in this case, "001").
##' It is often used when data comes in with a conglomorated identifier (such as case-matchNumber-drawNumber-Month).
##' @title Extract only part of a string
##' @param string a vector of strings
##' @param sep the separator that separates the parts of the strings
##' @param position the position of the element you wish to extract
##' @param flexible Force the function to find the string "sep"? See details
##' @return the element the user wishes to extract
##' @details The "flexible" function is for instances where a particular string (i.e., \code{sep}) is found in only some elements of \code{string}. For example,
##' if the string is c("...call..ID", "...call..Gender", "ethnicity"), if \code{flexible} is false, it will search for the string "...call.." in the string "ethnicity", and, not 
##' finding it, will return NA. To overcome this, \code{flexible} tells the function to only perform the operation on those parts of the vector that contain the string \code{sep}.
##' @author Dustin Fife
##' @export
##' @examples
##' barcode = c("Case-001-B", "Control-001-A", "Case-002-A", "001")
##' subsetString(barcode, sep="-", position=2, flexible=TRUE)
##' subsetString(barcode, sep="-", position=3, flexible=TRUE)
##' subsetString(barcode, sep="-", position=3, flexible=FALSE)
subsetString = function(string, sep=" ", position=3, flexible=FALSE){
	if (flexible){
		which.strings = grep(sep, string, fixed=TRUE)
	} else {
		which.strings = 1:length(string)
	}
	string[which.strings] = unlist(lapply(string[which.strings], FUN=function(x){unlist(strsplit(x, sep, fixed=TRUE))[position]}))
	string	
}