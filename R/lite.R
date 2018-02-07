# i don't want to wrap mongolite in anything
# we'll assume all access is through mongolite

#' list collections in AWS mongo server for txregnet
#' @param ignore integer vector telling which lines of mongo db.getCollectionNames() result should be ignored
#' @param url a valid mongodb URL
#' @param db character(1) db name
#' @examples
#' txregCollections()[1:5]
#' @export
txregCollections = function(ignore=1:3, url=URL_txregInAWS(), db="txregnet") {
  lis = system(sprintf("mongo %s/%s --eval 'db.getCollectionNames()'",
                  url, db), intern=TRUE)
  rjson::fromJSON(paste0(lis[-c(ignore)], collapse=""))
}

#' get names of fields in a collection in remote txregnet
#' @param coll character(1) name of collection
#' @param check logical(1) if TRUE will verify that \code{coll} is present
#' @param url character(1) mongodb url
#' @param db character(1) mongodb db name
#' @param limitn numeric(1) number of records to probe to get field names
#' @examples
#' getFieldNames("CD34_DS12274_hg19_FP")
#' @export
getFieldNames = function(coll, check=TRUE, 
    url=URL_txregInAWS(), db="txregnet", limitn=1) {
 if (check) {
   allcoll = txregCollections()
   stopifnot(coll %in% allcoll)
 }
 ans = mongo(url = url, db=db, collection=coll)$find('{}',
    limit=limitn)
 names(ans)
}

#' operate on a character vector to derive a DataFrame, splitting on a tokenand retrieving first and last split fragments as 'base' and 'type' fields
#' @param x character vector
#' @param spltok token to use in strsplit
#' @examples
#' 	some = c("Adipose_Subcutaneous_allpairs_v7_eQTL",
#'	"CD14_DS17215_hg19_FP",
#'	"CD19_DS17186_hg19_FP",
#'	"ENCFF001WGV_hg19_HS",
#'	"ENCFF994OCD_hg19_HS")
#' basicFormatter(some)
#' @export
basicFormatter = function(x, spltok="_") {
 xs = strsplit(x, spltok)
 type = sapply(xs, tail, 1)
 base = sapply(xs, "[", 1)
 mid = sapply(xs, function(z) paste(z[-c(1, length(z))], collapse="_"))
 DataFrame(base=base, type=type, mid=mid)
}

#' generate a colData component corresponding to a mongodb 
#' @param url character(1) url for mongodb
#' @param db character(1) database name
#' @param formatter a function that takes in a character vector and
#' returns a DataFrame with number of rows equal to the length of input
#' @examples
#' makeColData()
#' @export
makeColData = function(url=URL_txregInAWS(), db="txregnet",
  formatter = basicFormatter) {
  basicFormatter( txregCollections( url=url, db=db ) )
}
