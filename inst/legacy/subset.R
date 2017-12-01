#mongoTxRegNetCheck = function() {
#    con = try(RMongo::mongoDbConnect("txregnet")) # defaults for local server
#    if (inherits(con, "try-error")) return(FALSE)
#    allcol = dbShowCollections(con)
#    allcol
#}
#
#.subsetByOv = function(x, ranges, maxgap = -1L, minoverlap = 0L, type = c("any", 
#    "start", "end", "within", "equal"), invert = FALSE, ...) {
#    con = mongoDbConnect(x@dbName) # defaults for local server
#    collnames = rownames(colData(x))
#    ncoll = length(collnames)
#    dfs = lapply(1:length(collnames), function(i) queryBedInMongo(con, collnames[i],
#                     ranges))
#    names(dfs) = collnames
#    dfs 
#}
#
##' extract records from mongo store based on GRanges query
##' @importFrom IRanges subsetByOverlaps
##' @import GenomicRanges
##' @param x instance of RaggedMongoExpt
##' @param ranges instance of GRanges
##' @param collectionType character(1) must be "bed" at this time
##' @param maxgap not used
##' @param minoverlap not used
##' @param type not used
##' @param invert not used
##' @param \dots not used
##' @return instance of \code{\link[RaggedExperiment]{RaggedExperiment-class}}
##' @aliases subsetByOverlaps,RaggedMongoExpt,GRanges-method
##' @examples
##' data(hsFiles)
##' okdf = DataFrame(hsFiles)
##' rownames(okdf) = hsFiles[,1]
##' rme1 = RaggedMongoExpt(dbName="txregnet", colData=okdf)
#
##' @exportMethod subsetByOverlaps
#setMethod("subsetByOverlaps", c("RaggedMongoExpt", "GRanges"),
#    function(x, ranges, collectionType,
#     maxgap = -1L, minoverlap = 0L, type = c("any", 
#     "start", "end", "within", "equal"), invert = FALSE, ...) {
#   stopifnot(collectionType=="bed")
#   ans = .subsetByOv(x, ranges)
#   ranger = function(x) {
#      ans = GRanges(x$chrom, IRanges(x$chromStart, x$chromEnd),
#         strand=ifelse(x$strand, ".", "*"))
#      x = x[, setdiff(names(x), c("strand", "chrom", "chromStart",
#               "chromEnd"))]
#      mcols(ans) = x
#      ans
#    }
#    grl = GRangesList(lapply(ans, ranger))
#    RaggedExperiment(grl, colData=colData(x))
#})
