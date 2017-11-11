.importToMongo = function(path, collectionName, fields, dbname = "db",
    type="tsv", importCmd = "mongoimport", host="127.0.0.1") {
  if (type != "tsv") stop("only handling tsv at this time")
# should check viability of importCmd, e.g., system(paste0(importCmd, "--help"), intern=TRUE) does not yield error
  cmd = paste0(importCmd, " --host ", host, " --db ", dbname, " --collection ",
                 collectionName, " --type ", type, " --fields ", fields,
                 "  --file ", path)
  chk = try(system(cmd, intern=TRUE))
  if (inherits(chk, "try-error")) return(chk)
  TRUE
}

.fieldString = function(type="broadPeak") {
  baseFields = "chrom,chromStart,chromEnd,name,score,strand,signalValue,pValue,qValue"
  if (type == "narrowPeak") 
    return(paste0(baseFields, ",peak"))
  else if (type == "broadPeak") return(baseFields)
  else if (type == "chromHMM") return("chrom,chromStart,chromEnd,state")
  else stop("type must be one of 'broadPeak', 'narrowPeak', 'chromHMM'")
}

#' arrange import to mongo using mongoimport, setting up type and fields appropriately
#' @param path path to bed file (not compressed)
#' @param collectionName name to use in mongodb
#' @param bedType one of 'narrowPeak', 'broadPeak', 'chromHMM': contact developers for other types if desired
#' @param dbname mongodb database name, used directly with system("mongoimort ...")
#' @param importCmd how to invoke 'mongoimport', default is to assume it can be found in PATH
#' @param host host identifier for mongoimport, defaults to 127.0.0.1
#' @examples
#' f1 = dir(system.file("bedfiles", package="TxRegInfra"), full=TRUE, patt="ENCFF971VCD")
#' f2 = dir(system.file("bedfiles", package="TxRegInfra"), full=TRUE, patt="E096_imp12")
#' chk1 = importBedToMongo(f1, "vjc1", db="txregnet")
#' stopifnot(chk1)
#' chk2 = importBedToMongo(f2, "vjc2", db="txregnet")
#' stopifnot(chk2)
#' system('mongo txregnet --eval "db.vjc1.remove({})"') # cleanup
#' system('mongo txregnet --eval "db.vjc2.remove({})"') # cleanup
#' @export
importBedToMongo = function( path, collectionName, 
    bedType="narrowPeak", dbname = "db",
    importCmd = "mongoimport", host="127.0.0.1" ) {
    stopifnot(any(bedType %in% c("narrowPeak", "broadPeak")))
    .importToMongo( path=path, collectionName=collectionName, 
      fields = .fieldString(type=bedType), dbname=dbname, type="tsv", importCmd=importCmd, host=host )
}

#' simple retrieval of documents representing bed ranges
#' @importFrom RMongo dbGetQuery
#' @importFrom rjson toJSON
#' @param con RMongo connection
#' @param collectionName character(1) name of collection
#' @param queryGRange length(1) GRanges instance
#' @param \dots passed to RMongo::dbGetQuery
#' @note Note that a default characteristic of RMongo::dbGetQuery is to retrieve 1000 records with parameter \code{limit=1000}.  You can pass alternate
#' values of this parameter through the ... .
#' @export
#' @examples
#' f1 = dir(system.file("bedfiles", package="TxRegInfra"), full=TRUE, patt="ENCFF971VCD")
#' chk1 = importBedToMongo(f1, "vjc1", db="txregnet")
#' stopifnot(chk1)
#' require(RMongo)
#' con = mongoDbConnect("txregnet")
#' require(GenomicRanges)
#' queryBedInMongo(con, "vjc1", GRanges("chr1", IRanges(1, 8e5))) 
#' system('mongo txregnet --eval "db.vjc1.remove({})"') # cleanup
queryBedInMongo = function( con, collectionName, queryGRange,
       queryGen = grConverter, ... ) {
    quer = queryGen( queryGRange )
    RMongo::dbGetQuery(con, collectionName, quer, ... )
}

#' convert a GRanges instance to JSON suitable for RMongo::dbGetQuery
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
