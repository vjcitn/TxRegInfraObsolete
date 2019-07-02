#' convert sbov() output to annotated graphNEL
#' @param sbovout output of sbov() when applied to a TxRegInfra eQTL resource from mongodb
#' @param \dots not used
#' @return instance of graphNEL from Bioc graph package
#' @note can convert to igraph via igraph::igraph.from.graphNEL
#' @examples
#' sbov_to_graphNEL(demo_eQTL_granges)
#' @export
sbov_to_graphNEL = function(sbovout, ...) {
   if (!requireNamespace("graph")) stop("install graph package to use this function.")
   if (!is(sbovout, "GRanges")) stop("expecting GRanges, ensure simplify=TRUE in use of sbov()")
   stopifnot(all(c("variant_id", "symbol", "qvalue", "slope") %in%
     names(mcols(sbovout))))
   sbovout$variant_id = as.character(sbovout$variant_id)
   allg = unique(sbovout$symbol)
   alls = unique(as.character(sbovout$variant_id))
   g1 = new("graphNEL", edgemode="directed")
   g1 = graph::addNode(c(allg, alls), g1)
   for (i in 1:length(sbovout)) {
     g1 = graph::addEdge(sbovout[i]$variant_id, sbovout[i]$symbol, g1)
   }
   graph::edgeDataDefaults(g1, "qvalue") = NA
   graph::edgeDataDefaults(g1, "slope") = NA
   for (i in 1:length(sbovout)) {
     graph::edgeData(g1, sbovout[i]$variant_id, sbovout[i]$symbol, "qvalue") = sbovout[i]$qvalue
     graph::edgeData(g1, sbovout[i]$variant_id, sbovout[i]$symbol, "slope") = sbovout[i]$slope
     }
   g1
}
