#mapping ms-omics compound names to KEGG
library(tidyverse)

replace_x <- function(x, replacement = NA_character_) {
  if (length(x) == 0 || length(x[[1]]) == 0) {
    replacement
  } else {
    x
  }
}

map_metaboanalyst_compounds = function(names_vector, verbose = T){
  query_results = names_vector %>% 
    paste(collapse = ";") %>%
    list(queryList = ., inputType = "name") %>%
    httr::POST(url = "http://api.xialab.ca/mapcompounds", body = ., encode = "json") 
  
  if(!query_results$status_code==200){stop("Something went wrong\nThe query did not go through.")}
  
  query_results %>% 
    httr::content(x = ., "text", encoding = "UTF-8") %>%
    gsub(pattern = "null", replacement = "\"NA\"") %>%
    RJSONIO::fromJSON(content = ., flatten = TRUE) %>% 
    purrr::modify_depth(2, replace_x) %>%
    cbind.data.frame() %>%
    tibble() %>%
    {.} -> out_df
  if(verbose){
    cat("Query completed.")
    cat("\n")
    if(sum(out_df$Match == "NA") > 0){
      message((paste(sum(out_df$Match == "NA"), "queried compounds returned no results.")))
      cat("\n")
      cat(paste("This includes the following compounds:", paste(head(out_df$Query[out_df$Match == "NA"]), 
                                                                  collapse = ",\n"), sep = "\n"))
      message("\nThis does not mean they do not exist, it would be wise to check this by hand.")
    }
  }
  return(out_df)
}
