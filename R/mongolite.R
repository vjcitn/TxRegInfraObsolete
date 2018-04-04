# tools for working with mongolite
# NB on macosx must specify localhost explicitly as 127.0.0.1

#' check for accessible local mongodb
#' @import mongolite
#' @param url character(1) defining mongodb server
#' @return logical(1)
#' @examples
#' if (interactive()) verifyRunningMongodb()
#' @export
verifyRunningMongodb = function(url="mongodb://127.0.0.1") {
  requireNamespace("mongolite")
  ans = try(mongo(url=url))
  class(ans)[1] == "mongo" # will return FALSE if try results in try-error
}

#' check for existence of 'mongo' command, for db.getCollectionNames etc.
#' @param cmd character(1) either 'mongo' or 'mongoimport'
#' @note we use mongoimport command to import tsv files; mongolite import 'method' not immediately useful for this
#' @return logical(1)
#' @examples
#' if (interactive()) verifyHasMongoCmd()
#' @export
verifyHasMongoCmd = function(cmd="mongo") {
 mcmd = try(system2(cmd, args="--help", stdout=TRUE, stderr=TRUE))
 !inherits(mcmd, "try-error")
}

#' list all collections in a database, using command-line interface
#' @param url character(1) mongodb URL
#' @param db character(1) mongodb database name
#' @return vector of strings
#' @examples
#' if (verifyRunningMongodb()) listAllCollections()
#' @export
listAllCollections = function(url="mongodb://127.0.0.1:27017", db="test") {
   lis = system(sprintf("mongo %s/%s --eval 'db.getCollectionNames()'",
                  url, db), intern=TRUE)
    rjson::fromJSON(paste0(lis[-c(1:3)], collapse=""))
}

