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
getFieldNames = function(collection, check=TRUE, 
    url=URL_txregInAWS(), db="txregnet", limitn=1) {
 if (check) {
   allcoll = txregCollections(url=url, db=db)
   stopifnot(collection %in% allcoll)
 }
 ans = mongo(url = url, db=db, collection=collection)$find('{}',
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
  cd = basicFormatter( tt <- txregCollections( url=url, db=db ) )
  rownames(cd) = tt
  cd
}

#' generate JSON to aggregate (counting records, and, by default, averaging
#' a given variable) within a collection
#' @note This produces json that can be used as an argument to m$aggregate() for m a mongolite::mongo instance
#' @examples
#' makeAggregator()
#' if (interactive()) {
#'    remURL = URL_txregInAWS()
#'    colls = listAllCollections( url=remURL, db = "txregnet")
#'    m1 = mongo(url = remURL, db = "txregnet", 
#'      collection="CD14_DS17215_hg19_FP")
#' # find minimum value of statistic 'stat' per chromosome
#'    newagg = makeAggregator( by="chr",
#'      vbl="stat", op="$min", opname="min")
#'    tab = m1$aggregate( newagg )
#'    head(tab)
#'    }
#' @export
makeAggregator = function( by = "chrom", vbl = "chromStart",
    opname="average", op="$avg") {
   prependedBy = paste("$", by, sep="")
   prependedVbl = paste("$", vbl, sep="")
   counter = list(`_id` = prependedBy, count=list("$sum"=1))
   realop = list(prependedVbl)
   names(realop) = op
   opl = list(realop)
   names(opl) = opname
   rjson::toJSON(list(list("$group"=c(counter,opl))))
}
#     
#m1$aggregate( jq )

#' convert a GRanges to a JSON query for mongodb
#' @param queryGRange a \code{\link[GenomicRanges]{GRanges-class}} instance of length 1
#' @param cfields a named character(3) vector with names 'chrom', 'start',
#' 'end'; the element values will be used to name document fields in the query
#' @examples
#' gr = GRanges("chr1", cfields=c(chrom="chr", start="start", end="end")
grConverter = function(queryGRange, 
    cfields = c(chrom="chrom", start="chromStart", end="chromEnd")) {
    stopifnot(length(queryGRange)==1)
    stopifnot(all(names(cfields)==c("chrom", "start", "end")))
# lis has provisional names which will be replaced
    lis = list(chr=as.character(seqnames(queryGRange)),
                 chromStart=c("$gte"=start(queryGRange)),
                 chromEnd=c("$lte"=end(queryGRange)))
    names(lis) = cfields
    toJSON(lis)
}

bannedColnames = function() c("seqnames", "ranges", "strand", 
   "seqlevels", "seqlengths", "isCircular", 
    "start", "end", "width", "element")

# the purpose of this class is 
# 1) to establish an iterator over collections S in X[,S]
# 2) to permit use of sparseAssay and compactAssay methods

setOldClass("mongo")
setClass("RaggedMongoExpt", representation(con="mongo"),
     contains="RaggedExperiment")

#' @export
RaggedMongoExpt = function(con, colData) {
   nsamp = nrow(colData)
#
# need a dummy GRanges
#
   if (nsamp > 0) {
        dum = GRanges("chr0", IRanges(2,1))
        initass =  GRangesList(lapply(1:nsamp, function(x) dum))
        names(initass) = rownames(colData)
        }   
   else initass = GRangesList()
   ans = RaggedExperiment(initass) #
   colData(ans) = colData #, colData=colData)
   colnames(ans) = rownames(colData)
   ans = new("RaggedMongoExpt", ans, con=con)
   colData(ans) = colData
   ans 
}

#' @export
getDocumentFields = function(rme, docTypeName="type") {
 curmongo = rme@con
 conmeta = parent.env(curmongo)$orig # risky, not in API?
 cd = colData(rme)
 stopifnot(docTypeName %in% names(cd))
 allcolls = rownames(cd)
 allty = cd$type
 utypes = unique(colData(rme)[[docTypeName]])
 inds = vapply(utypes, function(x) min(which(allty == x)), numeric(1))
 fields = lapply(inds, function(x) getFieldNames(
     url=conmeta$url, db=conmeta$db, collection=allcolls[x],
     check = FALSE))
 names(fields) = utypes
 fields
}

#buildQueries = function(gr, chrtags, starttags, endtags) {
#  stopifnot(length(chrtags)==length(starttags), length(chrtags)==length(endtags))
#  lapply(seq_len(length(chrtags)), function(x) grConverter(gr,
#   c(chrom=chrtags[x], start=starttags[x], end=endtags[x])))
#}

#> getQueryFields(rme1)
#$eQTL
# [1] "gene_id"      "variant_id"   "tss_distance" "ma_samples"   "ma_count"    
# [6] "maf"          "pval_nominal" "slope"        "slope_se"     "qvalue"      
#[11] "chr"          "snp_pos"      "A1"           "A2"           "build"       
#
#$FP
#[1] "chr"   "start" "end"   "id"    "stat" 
#
#$HS
#[1] "chrom"       "chromStart"  "chromEnd"    "name"        "score"      
#[6] "strand"      "signalValue" "pValue"      "qValue"     

basicCfieldsMap = function() {
  list(eQTL=list(cfields=c(chrom="chr", start="snp_pos", end="snp_pos")),
       FP = list(cfields=c(chrom="chr", start="start", end="end")),
       HS = list(cfields=c(chrom="chrom", start="chromStart", end="chromEnd")))
}

#' @export
makeGRConverterList = function(rme, map=basicCfieldsMap(),
   docTypeName="type") {
# return closures that will behave properly when
# evaluated on a GRanges input
  doctypes = colData(rme)[[docTypeName]]
  utypes = unique(doctypes)
  ans = lapply(utypes, function(x) function(gr)
          grConverter(gr, cfields=map[[x]]$cfields))
  names(ans) = utypes
  ans
}

#' prototype of subsetter for mongo resource
#' @param rme RaggedMongoExpt instance
#' @param gr GRanges instance to subset by
#' @param map list with one element per document type telling what fields are chr, start, stop
#' @param docTypeName character(1) naming column of colData(rme) that has document type
#' @examples
#' requireNamespace("mongolite")
#' m1 = mongolite::mongo(url=URL_txregInAWS(), db="txregnet")
#' cd = makeColData(url=URL_txregInAWS(), db="txregnet")
#' rme1 = RaggedMongoExpt(m1, cd[which(cd$type=="FP"),][1:4,])
#' ss = sbov(rme1, GRanges("chr1", IRanges(1e6, 1.5e6)))
#' @export
sbov = function(rme, gr, map=basicCfieldsMap(), docTypeName="type") {
  stopifnot(is(gr, "GRanges"), length(gr)==1)
  meta = parent.env(rme@con)$orig
  theurl = meta$url
  thedb = meta$db
  convs = makeGRConverterList(rme=rme, map=map, docTypeName=docTypeName)
  cd = colData(rme)
  allcols = rownames(cd)
  content = lapply(seq_len(length(allcols)), function(x) {
     cat(".")
     ty = cd[[docTypeName]][x]
     conn = mongo(url=theurl, db=thedb, collection=
            rownames(cd)[x])
     res = conn$find(convs[[ty]](gr))
     if (nrow(res)==0) return(NULL)
     curmap = map[[ty]]$cfields
     curstrand = rep("*", nrow(res))
     if ("strand" %in% names(res)) curstrand = ifelse(res$strand %in% c("+", "-"), res$strand, "*")
     gr = GRanges(res[,curmap["chrom"]], IRanges(res[,curmap["start"]], res[,curmap["end"]]),
                strand=curstrand)
     badnm = match(names(res), bannedColnames())
     if (any(tt <- !is.na(badnm))) res = res[,-which(tt)]
     mcols(gr) = res
     gr
     })
  badcon = vapply(content, is.null, logical(1))
  if (length(dr <- which(badcon))>0) {
    content=content[-dr]
    cd = cd[-dr,]
    }
  names(content) = rownames(cd)
#  assaygr = lapply(content, function(x) rep(gr, nrow(x)))
#  for (i in seq_len(length(assaygr)))
#     # you need to get the content into the GRanges ... then maybe make MultiAssayExperiment?
#     mcols(assaygr[[i]]) = content[[i]]
  list( assay=content, colData=cd )
}

