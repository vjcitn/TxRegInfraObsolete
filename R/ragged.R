##' create mongolite connection to local mongodb server
##' @param coll character(1) collection name
##' @param db character(1) database name
##' @param url character(1) mongodb URL minus port
##' @examples
##' if (verifyRunningMongodb()) print(localMongolite())
##' @export
#localMongolite = function(coll="test", db="test", url="mongodb://127.0.0.1") {
#    con = mongo(url="mongodb://127.0.0.1", db=db, collection=coll)
#    new("mongoliteCon", con=con, db=db, collection=coll, url=url)
#}
#
#
##' @exportClass RaggedMongoExpt
#setClass("RaggedMongoExpt", contains="RaggedExperiment",
#    representation=representation(con="ANY"))
## Constructor
#
##' manage ragged interval data in mongodb collections using a RaggedExperiment
##' @param con should be instance of mongoliteCon or RMongo
##' @param colData S4Vectors DataFrame instance
##' @examples
##' \dontrun{
##' if (verifyRunningMongodb()) {
##'   okdf = DataFrame(hsFiles_subset) # creates hsFiles
##'   rownames(okdf) = hsFiles[,1]
##'   rme1 = RaggedMongoExpt(con, colData=okdf)
##'   rme1
##'   }
##' }
##' @export
#RaggedMongoExpt = function( con, colData=DataFrame()) {
#   nsamp = nrow(colData)
##
## looks like you must use dummy GRanges or records will be dropped
##
#   if (nsamp > 0) {
#        dum = GRanges("chr0", IRanges(2,1))
#        initass =  GRangesList(lapply(1:nsamp, function(x) dum))
#        names(initass) = rownames(colData)
#        }
#   else initass = GRangesList()
#   ans = RaggedExperiment(initass) #
#   colData(ans) = colData #, colData=colData)
#   rownames(ans) = rownames(colData)
#   ans = new("RaggedMongoExpt", ans, con=con)
#   colData(ans) = colData
#   ans
#}
##> a1 = queryBedInMongo(con, "vjc1", GRanges("chr1", IRanges(1, 8e5)))
##> GRanges(a1$chrom, IRanges(a1$chromStart, a1$chromEnd))
#
##' @exportMethod subsetByOverlaps
#setMethod("subsetByOverlaps", c("RaggedMongoExpt", "GRanges"),
#  function(x, ranges, maxgap = -1L, minoverlap = 0L, type = c("any", 
#    "start", "end", "within", "equal"), invert = FALSE, ...) {
#  .subsetByOv( x, ranges, maxgap = -1L, minoverlap = 0L, type = c("any", 
#    "start", "end", "within", "equal"), invert = FALSE, ...) 
#})
#.subsetByOv = function(x, ranges, maxgap = -1L, minoverlap = 0L, type = c("any", 
#    "start", "end", "within", "equal"), invert = FALSE, ...) {
#    con = x@con 
#    collnames = rownames(colData(x))
#    ncoll = length(collnames)
#    dfs = lapply(1:length(collnames), function(i) 
#          queryBedInMongo(con, collnames[i], ranges))
#    names(dfs) = collnames
#    dfs
#}
#
