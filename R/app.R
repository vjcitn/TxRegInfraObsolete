
dnase_audits = list(
  audit.warning = c("inconsistent platforms",
   "mixed read lengths", "insufficient read depth", 
   "low spot score"),
  audit.internal.action = c("mismatched file status", 
   "missing derived_from", 
   "biological replicates with identical biosample", 
   "NTR biosample"),
  audit.not_compliant = c("insufficient read length", 
   "insufficient replicate concordance", 
   "insufficient spot score", 
   "unreplicated experiment"),
  audit.error = c("extremely low read depth",
   "extremely low spot score", 
   "inconsistent ontology term", 
   "inconsistent library biosample")
)

collect_tissue_options = function() {
  if (!requireNamespace("ontoProc")) stop("install ontoProc to use this function")
  uu = ontoProc::getUBERON_NE()
  orgs = uu$name[unique(grep("^UB", TxRegInfra::dnmeta$Biosample.term.id, value=TRUE))]
  cc = ontoProc::getCellOnto()
  cells = cc$name[unique(grep("^CL", TxRegInfra::dnmeta$Biosample.term.id, value=TRUE))]
  list(organs=orgs, cells=cells)
}

#' support search of hotspot/peak data from ENCODE
#' @return data.frame of metadat about selections
#' @examples
#' if (interactive()) {
#'     oask = options()$example.ask
#'     if (askYesNo("start app?"))
#'            require("shiny")
#'            dnmetaApp()
#'     options(example.ask=oask)
#'     }
#' @export
dnmetaApp = function() {
if (!requireNamespace("shiny")) stop("install shiny to use this app")
require("shiny")
output_types = c("base overlap signal", "hotspots", 
   "peaks", "raw signal", "signal", "signal of unique reads")
ui = fluidPage(
 sidebarLayout(
  sidebarPanel(
   helpText("basic ui"),
   radioButtons("assembly", "Assembly",
      choices = c("GRCh38", "hg19")),
   radioButtons("top_opts", "anatomic focus",
      choices=c("cells [CL]", "organs [UB]"), selected = "organs [UB]"),
   checkboxGroupInput("outputTypes", "output type",
      choices=output_types, selected = "hotspots"),
   radioButtons("tableDisplay", "display", choices=c("compact", "full"),
      selected = "compact", inline=TRUE),
   uiOutput("top_choice"), 
   checkboxGroupInput("auditExclusions", "audit exclusions", 
        choices = c("none", sort(unlist(dnase_audits, use.names=FALSE))), selected = "none"),
   width=3
   ),
  mainPanel(
   dataTableOutput("seldn")
   )
  )
 )
server = function(input, output) {
 output$top_choice = renderUI({
  opts = collect_tissue_options()
  chk = ifelse(input$top_opts == "cells [CL]", "cells", "organs")
  curopts = sort(as.character(opts[[chk]]))
  selectInput("focus", "target cell/organ", choices = curopts, selected=curopts[1])
  })
 output$seldn = renderDataTable({
  dmet = TxRegInfra::dnmeta
  ans = with(dmet, dmet[
       which(Biosample.term.name %in% input$focus & 
         Output.type %in% input$outputTypes &
         Assembly == input$assembly 
         ), ])
  audex = input$auditExclusions
  audex = setdiff(audex, "none")
  if (length(audex) > 0) {
    subtab = ans[, grep("Audit", names(ans))]
    inds = lapply(subtab, function(x)   # because fields have entries joined by commas, must use grep
        unlist(lapply(audex, function(z) grep(z, x))) )
    drop = setdiff(unlist(inds), 0)
    if (length(drop)>0) ans = ans[-drop,]
    }
  kp = 1:ncol(ans)
  if (input$tableDisplay == "compact") kp = 
     c("File.accession", "File.format", "Output.type", "Experiment.accession", 
         "Assay", "Biosample.term.id", "Biosample.term.name", "Biosample.type", 
         "Biosample.life.stage", "Biosample.sex", "Biosample.Age", "Biosample.organism")
  ans[,kp]
  }) 
 }
runApp(list(ui=ui, server=server))
}
  
#   
#c("File.accession", "File.format", "Output.type", "Experiment.accession", 
#"Assay", "Biosample.term.id", "Biosample.term.name", "Biosample.type", 
#"Biosample.life.stage", "Biosample.sex", "Biosample.Age", "Biosample.organism", 
#"Biosample.treatments", "Biosample.subcellular.fraction.term.name", 
#"Biosample.phase", "Biosample.synchronization.stage", "Experiment.target", 
#"Antibody.accession", "Library.made.from", "Library.depleted.in", 
#"Library.extraction.method", "Library.lysis.method", "Library.crosslinking.method", 
#"Library.strand.specific", "Experiment.date.released", "Project", 
#"RBNS.protein.concentration", "Library.fragmentation.method", 
#"Library.size.range", "Biological.replicate.s.", "Technical.replicate", 
#"Read.length", "Mapped.read.length", "Run.type", "Paired.end", 
#"Paired.with", "Derived.from", "Size", "Lab", "md5sum", "dbxrefs", 
#"File.download.URL", "Assembly", "Platform", "Controlled.by", 
#"File.Status", "Audit.WARNING", "Audit.INTERNAL_ACTION", "Audit.NOT_COMPLIANT", 
#"Audit.ERROR")
#
