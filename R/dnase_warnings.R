dnase_warnings = list(
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
                                                                                   3 
