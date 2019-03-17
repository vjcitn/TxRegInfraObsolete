
dnmetaApp = function() {
output_types = c("base overlap signal", "hotspots", 
   "peaks", "raw signal", "signal", "signal of unique reads")
ui = fluidPage(
 sidebarLayout(
  sidebarPanel(
   helpText("basic ui"),
   radioButtons("top_opts", "anatomic focus",
      choices=c("cells [CL]", "organs [UB]"), selected = "organs [UB]"),
   checkboxGroupInput("outputTypes", "output type",
      choices=output_types, selected = "hotspots"),
   uiOutput("top_choice"), width=3
   ),
  mainPanel(
   dataTableOutput("seldn")
   )
  )
 )
server = function(input, output) {
 if (!exists("dnmeta")) load("dnmeta.rda")
 output$top_choice = renderUI({
  opts = collect_tissue_options()
  chk = ifelse(input$top_opts == "cells [CL]", "cells", "organs")
  curopts = sort(as.character(opts[[chk]]))
  selectInput("focus", "target cell/organ", choices = curopts, selected=curopts[1])
  })
 output$seldn = renderDataTable({
  dnmeta[ which(dnmeta$Biosample.term.name %in% input$focus
          & dnmeta$Output.type %in% input$outputTypes), ]
  }) 
 }
runApp(list(ui=ui, server=server))
}
  
   
