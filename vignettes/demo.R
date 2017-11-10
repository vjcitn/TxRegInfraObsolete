
library(TxRegQuery)
library(TxRegInfra)
library(RMongo)
data(hsFiles)
mongoColls = mongoSource()

for(i in 1:nrow(hsFiles)){
  coll=paste0(hsFiles$File.accession[i],"_hg19_HS")
  cat(paste(coll," in db:", coll%in%mongoColls,"\n"))
}

# txRegResource -- general class

setClass("txRegResource", "VIRTUAL")
setClass("txRegMongoFamily", contains="txRegResource",
  representation(dbname = "character", meta="data.frame"))
txRegMongoFamily = function(dbname, meta) {
 new("txRegMongoFamily", dbname=dbname, meta=meta)
}
setMethod("show", "txRegMongoFamily", function(object) {
 cat("txRegMongoFamily instance with", nrow(object@meta), "collections\n")
 show(S4Vectors::DataFrame(object@meta))
})

setClass("txRegMongoDb", representation(con="ANY", dbname="character",
   host="character", port="numeric", allColl="character"))
txRegMongoDb = function(dbname, host="127.0.0.1", port=27017) {
  con = mongoDbConnect(dbName=dbname, host=host, port=port)
  allColl = dbShowCollections(con)
  new("txRegMongoDb", dbname=dbname, host=host, port=port, allColl=allColl)
}
setMethod("show", "txRegMongoDb", function(object) {
 cat("txRegMongoDb instance with", length(object@allColl), "collections\n")
 cat(Biobase::selectSome(object@allColl), "\n")
})

myTdb = txRegMongoDb(dbname="txregnet")


#con = mongo(collection = mycoll2, db = "txregnet")

myFam = txRegMongoFamily("txregnet", hsFiles)

subsetByOv = function(fam, ind=1, rng, fixup=function(x) paste0(x, "_hg19_HS")) {
  coll = mongo(fixup(fam@meta[ind,"File.accession"]),  db=fam@dbname)
  mystart=min(start(rng))
  myend=max(end(rng))
  mychr=as.character(seqnames(rng)[1])
  mylist=list(chrom=mychr, chromStart=list("$gte"=mystart), chromEnd=list("$lte"=myend))
  myquery=toJSON(mylist)
  res=coll$find(myquery)
  resRange=GRanges(res$chrom, IRanges(res$chromStart, res$chromEnd)) #, mcols=res)
  mcols(resRange) = res
  resRange
}
 
  

queryRange<-function(mycoll, myrange,window=500e3){
  mycoll2=paste0(mycoll,"_hg19_HS")
  my_collection = mongo(collection = mycoll2, db = "txregnet") # connect
  mystart=min(start(myrange))-window
  myend=max(end(myrange))+window
  mychr=as.character(seqnames(myrange)[1])
  mylist=list(chrom=mychr, chromStart=list("$gte"=mystart), chromEnd=list("$lte"=myend))
  myquery=toJSON(mylist)
  res=my_collection$find(myquery)
  resRange=GRanges(res$chrom, IRanges(res$chromStart, res$chromEnd)) #, mcols=res)
  mcols(resRange) = res
  resRange
}

library(erma)
library(rjson)
mygene=genemodel("ORMDL3")
queryRange(hsFiles$File.accession[1],mygene)

