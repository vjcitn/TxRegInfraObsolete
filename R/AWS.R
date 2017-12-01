#' return mongodb URL for working mongo server
#' @examples
#' URL_txregInAWS
#' @export
URL_txregInAWS = function() 
 "mongodb://ec2-34-238-234-228.compute-1.amazonaws.com:27017"

#' return mongolite connection for working mongo server
#' @param \dots passed to mongo()
#' @examples
#' if (interactive()) {
#' txregRemote()
#' }
#' @export
txregRemote = function(...) {
 mongo(db="txregnet", url=URL_txregInAWS(), ...)
}
