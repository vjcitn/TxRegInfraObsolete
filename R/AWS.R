#' return mongodb URL for working mongo server
#' @return character(1) URL for a hosted resource
#' @examples
#' URL_txregInAWS
#' @export
URL_txregInAWS = function() "mongodb+srv://user:user123@txregnet-kui9i.mongodb.net/txregnet"

# #' return mongolite connection for working mongo server #' @param \dots passed
# to mongo() #' @return an instance mongoliteCon of mongo from mongolite #' @note
# Want to minimize role of this.  Don't get in the #' way of mongolite.  #'
# @examples #' if (interactive()) { #' txregRemote() #' } #' @export txregRemote
# = function(...) { url=URL_txregInAWS() new('mongoliteCon',
# con=mongo(db='txregnet', url=url, ...), url=url, db='txregnet') }

#' local mongodb txregnet
#' @return a string with 127.0.0.1 instead of localhost, useful on macosx
#' @export
URL_txregLocal = function() "mongodb://127.0.0.1:27017"
