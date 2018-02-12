#' return mongodb URL for working mongo server
#' @examples
#' URL_txregInAWS
#' @export
URL_txregInAWS = function() 
   "mongodb://ec2-34-198-105-83.compute-1.amazonaws.com:27017"

#' return mongolite connection for working mongo server
#' @param \dots passed to mongo()
#' @examples
#' if (interactive()) {
#' txregRemote()
#' }
#' @export
txregRemote = function(...) {
 url=URL_txregInAWS()
 new("mongoliteCon", con=mongo(db="txregnet", url=url, ...), url=url, 
     db="txregnet")
}

#' local mongodb txregnet
#' @export
URL_txregLocal = function()
  "mongodb://127.0.0.1:27017"
