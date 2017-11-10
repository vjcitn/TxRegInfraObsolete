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
  else stop("type must be one of 'broadPeak', 'narrowPeak'")
}

#' arrange import to mongo using mongoimport, setting up type and fields appropriately
#' @param path path to bed file (not compressed)
#' @param collectionName name to use in mongodb
#' @param bedType one of 'narrowPeak', 'broadPeak', contact developers for other types if desired
#' @param dbname mongodb database name, used directly with system("mongoimort ...")
#' @param importCmd how to invoke 'mongoimport', default is to assume it can be found in PATH
#' @param host host identifier for mongoimport, defaults to 127.0.0.1
#' @export
importBedToMongo = function( path, collectionName, 
    bedType="narrowPeak", dbname = "db",
    importCmd = "mongoimport", host="127.0.0.1" ) {
    stopifnot(any(bedType %in% c("narrowPeak", "broadPeak")))
    .importToMongo( path=path, collectionName=collectionName, 
      fields = .fieldString(type=bedType), dbname=dbname, type="tsv", importCmd=importCmd, host=host )
}


##!/usr/bin/env Rscript
#auto_import <- function(){
#  df = read.table(file = "/home/reshg/metadataEncodeDNase1.tsv", sep = '\t', header = TRUE)
#  for(i in 1:length(df$Assembly)){
#    #if(lapply(df$Assembly[i], function(x) any(x %in% "hg19")) == TRUE){
#    if(df$Assembly[i]== "hg19"){       
#      setwd("/home/reshg/BEDFILES")
#      #if(lapply(df$File.format[i], function(y) any(y %in% "bed broadPeak")) == TRUE){
#      if(df$File.format[i] == "bed broadPeak"){   
#        my_importcmd = paste0("/usr/bin/mongoimport --db txregnet --collection ",df$File.accession[i],"_hg19_HS --type tsv --fields chrom,chromStart,chromEnd,name,score,strand,signalValue,pValue,qValue --file ",df$File.accession[i],".bed")
#        #system("/usr/bin/mongoimport --db shweta --collection",df$File.accession[i], "--type tsv --fields chrom,chromStart,chromEnd,name,score,strand,signalValue,pValue,qValue --file", df$File.accession[i])
#        system(my_importcmd)
#        #my_indexcmd = paste0("db.",df$File.accession[i],"_HS.createIndex({chromStart:1, chromEnd:1})")
#        #system(my_indexcmd)
#      }
#      #else if(lapply(df$File.format[i], function(y) any(y %in% "bed narrowPeak")) == TRUE){
#      else if(df$File.format[i] == "bed narrowPeak"){ 
#        my_importcmd = paste0("/usr/bin/mongoimport --db txregnet --collection ",df$File.accession[i],"_hg19_HS --type tsv --fields chrom,chromStart,chromEnd,name,score,strand,signalValue,pValue,qValue,peak --file ",df$File.accession[i],".bed")
#        system(my_importcmd) 
#        #my_indexcmd = paste0("db.",df$File.accession[i],"_HS.createIndex({chromStart:1, chromEnd:1})")
#        #system(my_indexcmd)
#      }
#    }
#  }
#}
#auto_import()
