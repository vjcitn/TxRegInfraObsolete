#' hsFiles: metadata about a small collection of bed files for demonstrating TxRegInfra
#' @importFrom utils data
#' @docType data
#' @format DataFrame
#' @examples
#' data(hsFiles)
#' hsFiles[1:4,1:4]
"hsFiles"
#' ragged41FP: A RaggedExperiment instance with digital genomic footprints over the coding region of ORMDL3
#' @docType data
#' @format DataFrame
#' @examples
#' data(ragged41FP)
#' ragged41FP
#' dim(ca <- compactAssay(ragged41FP,3)) # stat
#' dim(sparseAssay(ragged41FP,3)) # stat
#' opar = par(no.readonly=TRUE)
#' par(mar=c(4,11,4,3), bg="lightgray")
#' image(ca, 
#'     main="over ORMDL3", axes=FALSE)
#' labs = gsub("_DS.*._hg19_FP", "", colnames(ragged41FP))
#' axis(2, at=seq(0,1,length=41), ylab="41 tissues", 
#'     labels=labs, cex.axis=.6, las=2)
#' mtext("positions on chr17 not to scale\n(red = lower FOS = stronger binding capacity", 1, line=1)
#' \dontrun{ # if (interactive()) {
#'   m1 = mongolite::mongo(url=URL_txregInAWS(), db="txregnet")
#'   cd = makeColData(url=URL_txregInAWS(), db="txregnet")
#'   rme1 = RaggedMongoExpt(m1, cd[which(cd$type=="FP"),])
#'   raggHHIP = sbov(rme1, GRanges("chr4", IRanges(145565173, 145605173)))
#'   ca = compactAssay(raggHHIP,3)[1:200,]
#'   image(ca, main="over HHIP", axes=FALSE)
#'   labs = gsub("_DS.*._hg19_FP", "", colnames(ca))
#'   axis(2, at=seq(0,1,length=ncol(ca)), ylab=paste(ncol(ca), "tissues"), 
#'     labels=labs, cex.axis=.6, las=2)
#'   mtext("positions on chr4 not to scale\n(red = lower FOS = stronger binding capacity", 1, line=1)
#' #  }
#' }
#' par(opar)
"ragged41FP"
