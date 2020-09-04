PageRank <- function(pdb = ""){
  #install.packages('igraph')
  #library(igraph)
  
  # pdb <- fromJSON(jsonInput)
  # g <- pdb[, "Package"] %>%
  #   makeDepGraph(availPkgs = pdb, suggests=FALSE, enhances=TRUE, includeBasePkgs = FALSE)
  #   
  #   pr <- g %>%
  #     page.rank(directed = FALSE) %>%
  #     use_series("vector") %>%
  #     sort(decreasing = TRUE) %>%
  #     as.matrix %>%
  #     set_colnames("page.rank")
    
    
    message = paste(pdb[1,1])
    
}