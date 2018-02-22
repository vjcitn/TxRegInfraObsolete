#' utility to generate link to biocfound bucket for FIMO TFBS scores
#' @param tag character(1) token identifying TF
#' @examples
#' URL_s3_tf
#' @export
URL_s3_tf = function(tag="M3433") {
 sprintf("http://s3.amazonaws.com/biocfound-tfbs/%s_1.02sort.bed.gz",
    tag)
 }

#' utility to read FIMO outputs (after sorting and tabix indexing applied as in contents of biocfound-tfbs S3 bucket)
#' @importFrom Rsamtools scanTabix
#' @param tf character(1) file id
#' @param param a GRanges delimiting the extract
#' @param param a GRanges delimiting the extract
#' @examples
#' requireNamespace("GenomicRanges")
#' fn = URL_s3_tf(tag="M5946")
#' importFIMO(fn, param=GenomicRanges::GRanges("chr1", IRanges(1,15000)))
#' @export
importFIMO = function( tf, param, genomeTag="hg19" ) {
 stopifnot(length(tf)==1, is(tf, "character"))
 stopifnot(length(param)==1, is(param, "GRanges"))
 tmp = scanTabix( tf, param = param )
 con = textConnection(paste(tmp[[1]], collapse="\n"))
 on.exit(close(con))
 cc = c("character", "numeric", "numeric", "NULL", "numeric", "character",
   "numeric")
 ans = read.delim(con, h=FALSE, sep="\t", colClasses=cc)
#     V1    V2    V3       V5 V6       V7
#1  chr1 11496 11511 -1.55556  - 0.000592
#2  chr1 11520 11535 -3.16162  + 0.000912
 fin = GRanges(ans$V1, IRanges(ans$V2, ans$V3), strand=ans$V6,
      score=ans$V5, pvalue=ans$V7)
 genome(fin) = genomeTag
 fin
}
