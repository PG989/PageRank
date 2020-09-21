#setwd('p:/_Users/_PGabi/PageRank/')

library(igraph)
library(jsonlite)
library(miniCRAN)
library(magrittr)
#readed_data <- fromJSON("test.json")

# GitHub  ----------------------------------

PageRank <- function(file, ...){

  
   pdb <- as.data.frame(fromJSON(file))
   colnames(pdb) <- c("Package", "Version", "Priority",  "Depends", "Imports", "LinkingTo", "Suggests", "Enhances",  "License", "License_is_FOSS", "License_restricts_use", "OS_type", "Archs", "MD5sum", "NeedsCompilation", "File", "Repository")
   rownames(pdb) <- pdb$Package
   g <- pdb[, 1] %>%
     makeDepGraph(availPkgs = pdb, suggests=FALSE, enhances=TRUE, includeBasePkgs = FALSE)
     
     pr <- g %>%
       page.rank(directed = FALSE) %>%
       use_series("vector") %>%
       sort(decreasing = TRUE) %>%
       as.matrix %>%
       set_colnames("page.rank")
     
     pr <- as.data.frame(pr)
     pr$Package <- rownames(pr)

  return(toJSON(pr[1:10,]))          
}
