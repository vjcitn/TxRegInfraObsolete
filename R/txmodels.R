#' use Gviz to render transcript models via GeneRegionTrack
#' @import Gviz
#' @import ensembldb
#' @import GenomeInfoDb
#' @param sym a gene symbol to be looked up in biovizBase::genesymbol table
#' @param gr a GRanges instance, anticipated to be length 1
#' @param edb a character(1) name of an EnsDb annotation package
#' @param plot.it a logical(1) specifying whether Gviz::plotTracks should be run
#' @param radius a numeric(1) specifying number to add to IRanges instance used to subset gene models from ensembldb::exonsBy output
#' @return an instance of Gviz::GeneRegionTrack, invisibly returned
#' @param \dots passed to Gviz::GeneRegionTrack
#' @examples
#' t0 = txmodels("ORMDL3", plot.it=TRUE, name="ORMDL3")
#' t1 = txmodels("ORMDL3", plot.it=FALSE, name="meta", collapseTranscripts="meta")
#' requireNamespace("Gviz")
#' Gviz::plotTracks(list(Gviz::GenomeAxisTrack(), t0, t1), showId=TRUE)
#' @export
txmodels = function(sym, gr,
     edb="EnsDb.Hsapiens.v75",
     plot.it=FALSE, radius=0, ...) {
 requireNamespace("biovizBase")
 requireNamespace("ensembldb")
 requireNamespace("GenomeInfoDb")
 requireNamespace("Gviz")
 if (sum(c(missing(sym), missing(gr)))!=1) stop("exactly one of sym, gr must be nonmissing")
 require(edb, character.only=TRUE) # FIXME
 if (!missing(sym)) {
  data(genesymbol, package="biovizBase")
  gr = genesymbol[sym]
  }
 seqlevelsStyle(gr) = "Ensembl"
 seq = as.character(seqnames(gr))[1]
 eg <- ensembldb::exonsBy(get(edb), by = "tx",
  filter = AnnotationFilterList(SeqNameFilter(c(seq)),
  GeneIdFilter("ENS", "startsWith")),
  columns = c("gene_biotype", "gene_name", "tx_id"))
# names(eg) = make.names(names(eg), unique=TRUE)
# stopifnot(sum(duplicated(names(eg)))==0)
 eg2 = as.data.frame(subsetByOverlaps(eg, gr+radius))
 eg2$chromosome = eg2$seqnames
 eg2$symbol = eg2$gene_name
 eg2$exon = eg2$exon_id
 eg2$transcript = eg2$transcript_id
 eg2$feature=eg2$gene_biotype
 eg2$gene = eg2$gene_name
 eg2$transcript=eg2$tx_id
 grt = GeneRegionTrack(eg2, ...)
 if (plot.it) plotTracks(list(GenomeAxisTrack(),grt), showId=TRUE)
 invisible(grt)
}
