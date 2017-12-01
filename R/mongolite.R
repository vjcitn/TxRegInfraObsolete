# tools for working with mongolite
# NB on macosx must specify localhost explicitly as 127.0.0.1

#' check for accessible local mongodb
#' @import mongolite
#' @param url character(1) defining mongodb server
#' @examples
#' verifyRunningMongodb()
#' @export
verifyRunningMongodb = function(url="mongodb://127.0.0.1") {
  requireNamespace("mongolite")
  ans = try(mongo(url=url))
  class(ans)[1] == "mongo" # will return FALSE if try results in try-error
}

#' list all collections in a database, using command-line interface
#' @param url character(1) mongodb URL
#' @param db character(1) mongodb database name
#' @examples
#' if (verifyRunningMongodb()) listAllCollections()
#' @export
listAllCollections = function(url="mongodb://127.0.0.1:27017", db="test") {
   lis = system(sprintf("mongo %s/%s --eval 'db.getCollectionNames()'",
                  url, db), intern=TRUE)
    rjson::fromJSON(paste0(lis[-c(1:3)], collapse=""))
}

setOldClass("mongo")
setClass("mongoliteCon", representation(con="mongo", db="character",
    collection="ANY", url="character"))
setMethod("show", "mongoliteCon", function(object) {
 cat(sprintf("mongolite connection for db %s, coll. %s\n", object@db,
    object@collection))
 cat("URL: ", object@url, "\n")
})
#' constructor for S4 representation of mongolite dbconnection
#' @param url character(1) mongodb reference
#' @param db character(1) name of database in use
#' @param collection character(1) collection for queries
#' @examples
#' if (interactive()) {
#'    tcon = mongoliteCon(url=URL_txregInAWS(), db="txregnet", collection="CD14_DS17215_hg19_FP")
#'    tcon
#' }
#' @export
mongoliteCon = function(url, db, collection="test") {
 con = mongo(url=url, db=db, collection=collection)
 new("mongoliteCon", con=con, db=db, collection=collection, url=url)
}
