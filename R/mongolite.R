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
  ans = try(mongo())
  class(ans)[1] == "mongo"
}

#' list all collections in a database, using command-line interface
#' @param db character(1) mongodb database name
#' @examples
#' if (verifyRunningMongodb()) listAllCollections()
#' @export
listAllCollections = function(db="test") {
   lis = system(sprintf("mongo %s --eval 'db.getCollectionNames()'",
                  db), intern=TRUE)
    rjson::fromJSON(paste0(lis[-c(1:3)], collapse=""))
}

