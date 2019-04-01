# i don't want to wrap mongolite in anything we'll assume all access is through
# mongolite

#' list collections in AWS mongo server for txregnet
#' @import rjson
#' @param ignore NULL by default; otherwise an 
#' integer vector telling which lines of mongo db.getCollectionNames() result should be ignored
#' @param url a valid mongodb URL
#' @param db character(1) db name
#' @param cliparms character(1) arguments to 'mongo', defaults to '--quiet --eval'
#' @return a character vector of collection names
#' @note Different mongodb servers can have different response prologues.  The ignore
#' parameter is there to bypass some of the initial text.  However, with the --quiet option
#' this may not be needed.  We now search for "[" to start parsing the collection list output.
#' @examples
#' if (verifyHasMongoCmd()) txregCollections()[seq_len(5)]
#' @export
txregCollections = function(ignore = NULL, url = URL_txregInAWS(), db = "txregnet",
       cliparms="--quiet --eval") {
    url = gsub("test", db, url)
    #dbref = sprintf("%s/%s", url, db)
    lis = system2("mongo", args=c(url, cliparms, "'db.getCollectionNames()'"), stdout = TRUE)
    if (!is.null(ignore)) lis = lis[-ignore]
    cstart = grep("^\\[$", lis)
    if (length(cstart)==0) stop("could not find '[' isolated in the response.")
    rjson::fromJSON(paste0(lis[(cstart):length(lis)], collapse = ""))
}

#' get names of fields in a collection in remote txregnet
#' @param collection character(1) name of collection
#' @param check logical(1) if TRUE will verify that \code{coll} is present
#' @param url character(1) mongodb url
#' @param db character(1) mongodb db name
#' @param limitn numeric(1) number of records to probe to get field names
#' @return a vector of strings
#' @examples
#' getFieldNames('CD34_DS12274_hg19_FP', check=FALSE) # we know this collection is there
#' @export
getFieldNames = function(collection, check = TRUE, url = URL_txregInAWS(), db = "txregnet", 
    limitn = 1) {
    if (check) {
        allcoll = txregCollections(url = url, db = db)
        stopifnot(collection %in% allcoll)
    }
    ans = mongo(url = url, db = db, collection = collection)$find("{}", limit = limitn)
    names(ans)
}

#' operate on a character vector to derive a DataFrame, splitting on a tokenand retrieving first and last split fragments as 'base' and 'type' fields
#' @param x character vector
#' @param spltok token to use in strsplit
#' @return a DataFrame instance
#' @examples
#' some = c('Adipose_Subcutaneous_allpairs_v7_eQTL',
#'   'CD14_DS17215_hg19_FP',
#'   'CD19_DS17186_hg19_FP',
#'   'ENCFF001WGV_hg19_HS',
#'   'ENCFF994OCD_hg19_HS')
#' basicFormatter(some)
#' @export
basicFormatter = function(x, spltok = "_") {
    xs = strsplit(x, spltok)
    type = vapply(xs, tail, character(1), n = 1)
    base = vapply(xs, "[", character(1), i = 1)
    mid = vapply(xs, function(z) paste(z[-c(1, length(z))], collapse = "_"), character(1))
    DataFrame(base = base, type = type, mid = mid)
}

#' generate a colData component corresponding to a mongodb 
#' @param url character(1) url for mongodb
#' @param db character(1) database name
#' @param formatter a function that takes in a character vector and
#' returns a DataFrame with number of rows equal to the length of input
#' @return a DataFrame instance
#' @examples
#' if (verifyHasMongoCmd()) makeColData()
#' @export
makeColData = function(url = URL_txregInAWS(), db = "txregnet", formatter = basicFormatter) {
    cd = basicFormatter(tt <- txregCollections(url = url, db = db))
    rownames(cd) = tt
    cd
}

#' generate JSON to aggregate (counting records, and, by default, averaging
#' a given variable) within a collection
#' @param by character(1) telling the field for stratifying records for aggregation 
#' @param vbl character(1) telling field with numerical value for which a statistic will be computed within strata defined by 'by'
#' @param opname character(1) define the name of the aggregation
#' @param op character(1) evaluating to a mongo aggregation operator like `$avg` or `$min`
#' @return a JSON document as produced by rjson::toJSON
#' @note This produces json that can be used as an argument to m$aggregate() for m a mongolite::mongo instance
#' @examples
#' makeAggregator()
#' if (interactive() & verifyHasMongoCmd()) {
#'    remURL = URL_txregInAWS()
#'    colls = listAllCollections( url=remURL, db = 'txregnet')
#'    m1 = mongo(url = remURL, db = 'txregnet', 
#'      collection='CD14_DS17215_hg19_FP')
#' # find minimum value of statistic 'stat' per chromosome
#'    newagg = makeAggregator( by='chr',
#'      vbl='stat', op='$min', opname='min')
#'    tab = m1$aggregate( newagg )
#'    head(tab)
#'    }
#' @export
makeAggregator = function(by = "chrom", vbl = "chromStart", opname = "average", op = "$avg") {
    prependedBy = paste("$", by, sep = "")
    prependedVbl = paste("$", vbl, sep = "")
    counter = list(`_id` = prependedBy, count = list(`$sum` = 1))
    realop = list(prependedVbl)
    names(realop) = op
    opl = list(realop)
    names(opl) = opname
    rjson::toJSON(list(list(`$group` = c(counter, opl))))
}
# m1$aggregate( jq )

#' convert a GRanges to a JSON query for mongodb
#' @importFrom GenomicRanges GRanges start end
#' @param queryGRange a \code{\link[GenomicRanges]{GRanges-class}} instance of length 1
#' @param cfields a named character(3) vector with names 'chrom', 'start',
#' 'end'; the element values will be used to name document fields in the query
#' @return a JSON document generated by rjson::toJSON
#' @examples
#' gr = GenomicRanges::GRanges('chr1', IRanges(1,25000))
#' grConverter(gr, cfields=c(chrom='chr', start='start', end='end'))
#' @export
grConverter = function(queryGRange, cfields = c(chrom = "chrom", start = "chromStart", 
    end = "chromEnd")) {
    stopifnot(length(queryGRange) == 1)
    stopifnot(all(names(cfields) == c("chrom", "start", "end")))
    # lis has provisional names which will be replaced
    ss = as.character(GenomeInfoDb::seqnames(queryGRange))
    g = grep("chr", ss)  # assume mongo will use integer if NCBI annotation used, ignoring M for now
    if (length(g) == 0) 
        ss = as.integer(ss)
    lis = list(chr = ss, chromStart = c(`$gte` = start(queryGRange)), chromEnd = c(`$lte` = end(queryGRange)))
    names(lis) = cfields
    rjson::toJSON(lis)  # bplapply cannot find without ::
}

bannedColnames = function() c("seqnames", "ranges", "strand", "seqlevels", "seqlengths", 
    "isCircular", "start", "end", "width", "element")

# the purpose of this class is 1) to establish an iterator over collections S in
# X[,S] 2) to permit use of sparseAssay and compactAssay methods

setOldClass("mongo")
#' @importClassesFrom RaggedExperiment RaggedExperiment
#' @importFrom RaggedExperiment RaggedExperiment
#' @importFrom GenomicRanges GRangesList
#' @importFrom SummarizedExperiment colData 'colData<-'
setClass("RaggedMongoExpt", representation(con = "mongo"), contains = "RaggedExperiment")

#' bind colData to a mongo-based ragged-experiment incubator
#' @importFrom methods is new
#' @param con a mongolite::mongo instance
#' @param colData a DataFrame instance
#' @return instance of RaggedMongoExpt
#' @export
RaggedMongoExpt = function(con, colData) {
    nsamp = nrow(colData)
    # need a dummy GRanges
    if (nsamp > 0) {
        dum = GRanges("chr0", IRanges(2, 1))
        initass = GRangesList(lapply(seq_len(nsamp), function(x) dum))
        names(initass) = rownames(colData)
    } else initass = GRangesList()
    ans = RaggedExperiment(initass)  #
    colData(ans) = cbind(colData(ans), colData)  #, colData=colData)
#    colnames(ans) = rownames(colData) -> mergeROWS changes triggered error here 31 March 2019
    ans = new("RaggedMongoExpt", ans, con = con)
    colData(ans) = cbind(colData(ans), colData)
    ans
}
#' determine the fields present in a txregnet document
#' @param rme instance of RaggedMongoExperiment
#' @param docTypeName character(1) telling the name of the column of colData(rme) that supplies information on document type
#' @return a character vector
#' @examples
#' getDocumentFields
#' @export
getDocumentFields = function(rme, docTypeName = "type") {
    curmongo = rme@con
    conmeta = parent.env(curmongo)$orig  # risky, not in API?
    cd = colData(rme)
    stopifnot(docTypeName %in% names(cd))
    allcolls = rownames(cd)
    allty = cd$type
    utypes = unique(colData(rme)[[docTypeName]])
    inds = vapply(utypes, function(x) min(which(allty == x)), numeric(1))
    fields = lapply(inds, function(x) getFieldNames(url = conmeta$url, db = conmeta$db, 
        collection = allcolls[x], check = FALSE))
    names(fields) = utypes
    fields
}

# buildQueries = function(gr, chrtags, starttags, endtags) {
# stopifnot(length(chrtags)==length(starttags), length(chrtags)==length(endtags))
# lapply(seq_len(length(chrtags)), function(x) grConverter(gr,
# c(chrom=chrtags[x], start=starttags[x], end=endtags[x]))) }

# > getQueryFields(rme1) $eQTL [1] 'gene_id' 'variant_id' 'tss_distance'
# 'ma_samples' 'ma_count' [6] 'maf' 'pval_nominal' 'slope' 'slope_se' 'qvalue'
# [11] 'chr' 'snp_pos' 'A1' 'A2' 'build' $FP [1] 'chr' 'start' 'end' 'id' 'stat'
# $HS [1] 'chrom' 'chromStart' 'chromEnd' 'name' 'score' [6] 'strand'
# 'signalValue' 'pValue' 'qValue'

basicCfieldsMap = function() {
    list(eQTL = list(cfields = c(chrom = "chr", start = "snp_pos", end = "snp_pos")), 
        FP = list(cfields = c(chrom = "chr", start = "start", end = "end")), HS = list(cfields = c(chrom = "chrom", 
            start = "chromStart", end = "chromEnd")))
}

#' generate a list of GRanges to JSON for queries to mongo
#' @param rme RaggedMongoExperiment instance
#' @param map list of lists of named character vectors
#' @param docTypeName character(1) that identifies sample 'type'
#' @return a list of JSON documents
#' @export
makeGRConverterList = function(rme, map = basicCfieldsMap(), docTypeName = "type") {
    # return closures that will behave properly when evaluated on a GRanges input
    doctypes = colData(rme)[[docTypeName]]
    utypes = unique(doctypes)
    ans = lapply(utypes, function(x) function(gr) grConverter(gr, cfields = map[[x]]$cfields))
    names(ans) = utypes
    ans
}

#' prototype of subsetter for mongo resource
#' @importFrom BiocParallel bplapply
#' @importFrom S4Vectors 'mcols<-' tail DataFrame
#' @param rme RaggedMongoExpt instance
#' @param gr GRanges instance to subset by
#' @param map list with one element per document type telling what fields are chr, start, stop
#' @param docTypeName character(1) naming column of colData(rme) that has document type
#' @return a RaggedExperiment instance
#' @examples
#' requireNamespace('mongolite')
#' if (verifyHasMongoCmd()) {  # for makeColData, which must be able to enumerate collections,
#'                             # and thus must be able to run system (as opposed to mongolite function) 'mongo' 
#'  m1 = mongolite::mongo(url=URL_txregInAWS(), db='txregnet')
#'  #cd = makeColData(url=URL_txregInAWS(), db='txregnet')
#'  cd = TxRegInfra::basicColData
#'  rme1 = RaggedMongoExpt(m1, cd[which(cd$type=='FP'),][seq_len(8),])
#'  BiocParallel::register(BiocParallel::SerialParam())
#'  ss = sbov(rme1, GRanges('chr1', IRanges(1e6, 1.5e6)))
#' } 
#' @export
sbov = function(rme, gr, map = basicCfieldsMap(), docTypeName = "type") {
    stopifnot(is(gr, "GRanges"), length(gr) == 1)
    meta = parent.env(rme@con)$orig
    theurl = meta$url
    thedb = meta$db
    convs = makeGRConverterList(rme = rme, map = map, docTypeName = docTypeName)
    cd = colData(rme)
    allcols = rownames(cd)
    content = bplapply(seq_len(length(allcols)), function(x) {
        cat(".")
        ty = cd[[docTypeName]][x]
        conn = mongo(url = theurl, db = thedb, collection = rownames(cd)[x])
        res = conn$find(convs[[ty]](gr))
        if (nrow(res) == 0) 
            return(NULL)
        curmap = map[[ty]]$cfields
        curstrand = rep("*", nrow(res))
        if ("strand" %in% names(res)) 
            curstrand = ifelse(res$strand %in% c("+", "-"), res$strand, "*")
        gr = GRanges(res[, curmap["chrom"]], IRanges(res[, curmap["start"]], res[, 
            curmap["end"]]), strand = curstrand)
        badnm = match(names(res), bannedColnames())
        if (any(tt <- !is.na(badnm))) 
            res = res[, -which(tt)]
        mcols(gr) = res
        gr
    })
    badcon = vapply(content, is.null, logical(1))
    if (length(dr <- which(badcon)) > 0) {
        content = content[-dr]
        cd = cd[-dr, ]
    }
    names(content) = rownames(cd)
    RaggedExperiment(assay = GRangesList(content), colData = cd)
}

