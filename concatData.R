set.seed(36490)
library(quanteda)
library(sentimentr)
library(tidyverse)
library(gsubfn)
file_list = list.files(
  path="/Users/znovack/Desktop/Git/36490Local",
  pattern="_corp.rds",
  full.names=TRUE,
  recursive=FALSE
)

init_corp = read_rds(file_list[1])
docnames(init_corp) = docvars(init_corp)$speech_id
for (i in 2:length(file_list)) {
  cat(i, "\n")
  add_corp = read_rds(file_list[i])
  docnames(add_corp) = docvars(add_corp)$speech_id
  int_sect = intersect(docnames(init_corp), docnames(add_corp))
  if (length(int_sect) > 0){
    add_corp = corpus_subset(add_corp, !(speech_id %in% int_sect))
  }
  init_corp = init_corp + add_corp
}

pat <- "(?<=^| )(\\S{1,6})(?=$| )"
texts(init_corp) = gsub(
  "[.]", " . ", 
  gsubfn(pat, ~ gsub("[.]", "", ..1), texts(init_corp), perl = TRUE)
)
saveRDS(init_corp, "/Users/znovack/Box/36490/ChinaSubTexts.rds")
