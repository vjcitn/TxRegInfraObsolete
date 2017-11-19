# we accommodate both RMongo and mongolite as of 19 Nov 2017 but
# may not always accommodate both

.queryBedInMongoRMongo = function( con, collectionName, queryGRange,
       queryGen = grConverter, ... ) {
    quer = queryGen( queryGRange )
    RMongo::dbGetQuery(con, collectionName, quer, ... )
}

setGeneric("queryBedInMongo", function( con, collectionName, queryGRange,
    queryGen, ...) standardGeneric("queryBedInMongo"))

#' simple retrieval of documents representing bed ranges
#' @rdname queryBedInMongo
#' @importFrom RMongo dbGetQuery
#' @importFrom rjson toJSON
#' @param con connection to mongodb, may be RMongo instance or mongo (mongolite) instance
#' @param collectionName character(1) name of collection (will be implicit for mongolite)
#' @param queryGRange length(1) GRanges instance
#' @param queryGen a function with arguments queryGR and cfields, see \code{\link{grConverter}} for a typical example
#' @param \dots passed to RMongo::dbGetQuery
#' @note Note that a default characteristic of RMongo::dbGetQuery is to retrieve 1000 records with parameter \code{limit=1000}.  You can pass alternate
#' values of this parameter through the ... .  If you do want to use the limit parameter, in dbGetQuery,
#' you must also specify skip.
#' @aliases queryBedInMongo
#' @exportMethod queryBedInMongo
#' @examples
#' f1 = dir(system.file("bedfiles", package="TxRegInfra"), full=TRUE, patt="ENCFF971VCD")
#' chk1 = importBedToMongo(f1, "vjc1", db="txregnet")
#' stopifnot(chk1)
#' con = RMongo::mongoDbConnect("txregnet")
#' requireNamespace("GenomicRanges")
#' queryBedInMongo(con, "vjc1", GRanges("chr1", IRanges(1, 8e5))) 
#' system('mongo txregnet --eval "db.vjc1.remove({})"') # cleanup
setMethod("queryBedInMongo", 
    c("RMongo", "character", "GRanges", "function"), 
    function( con, collectionName, queryGRange, queryGen, ...) {
    .queryBedInMongoRMongo( con, collectionName, queryGRange, queryGen, ...)})
setMethod("queryBedInMongo", 
    c("RMongo", "character", "GRanges", "missing"), 
    function( con, collectionName, queryGRange, queryGen, ...) {
    .queryBedInMongoRMongo( con, collectionName, queryGRange, grConverter, ...)})

#' convert a GRanges instance to JSON suitable for RMongo::dbGetQuery
#' importFrom GenomicRanges start end
#' @param queryGRange a length 1 GRanges instance specifying interval within which records are to be retrieved
#' @param cfields a vector with named elements 'chrom', 'start', 'end' indicating how the JSON components should be named to query fields in the target mongodb document
#' @examples
#' require(GenomicRanges)
#' grConverter(GRanges("chr1", IRanges(1, 5000)))
#' @export
grConverter = function(queryGRange, 
    cfields = c(chrom="chrom", start="chromStart", end="chromEnd")) {
    stopifnot(length(queryGRange)==1)
    lis = list(chr=as.character(seqnames(queryGRange)),
                 chromStart=c("$gte"=start(queryGRange)),
                 chromEnd=c("$lte"=end(queryGRange)))
    names(lis) = cfields
    toJSON(lis)
}



