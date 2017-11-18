#' define container for mongodb backed RaggedExperiment
#' @importClassesFrom RaggedExperiment RaggedExperiment
#' @rdname RaggedMongoExpt
#' @aliases RaggedMongoExpt-class
#' @aliases RaggedMongoExpt
#' @exportClass RaggedMongoExpt
setClass("RaggedMongoExpt", contains="RaggedExperiment",
    representation=representation(dbName="character"))
# Constructor
#' define constructor for mongodb backed RaggedExperiment
#' @param dbName character(1) name used for db in mongodb
#' @param colData a \code{\link[S4Vectors]{DataFrame-class}} instance
#' @export
RaggedMongoExpt = function( dbName, colData=DataFrame()) {
   nsamp = nrow(colData)
#
# looks like you must use dummy GRanges or they will be dropped
#
   if (nsamp > 0) {
        dum = GRanges("chr0", IRanges(2,1))
        initass =  GRangesList(lapply(1:nsamp, function(x) dum))
        names(initass) = rownames(colData)
        }   
   else initass = GRangesList()
   ans = RaggedExperiment(initass, colData=colData)
   rownames(ans) = rownames(colData)
   ans = new("RaggedMongoExpt", ans, dbName=dbName)
   ans 
}

