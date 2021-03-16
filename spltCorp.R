full_corp = readRDS("/Users/znovack/Desktop/Git/36490Local/congressall_corpus2.rds")
set.seed(36490)
library(quanteda)
library(sentimentr)
library(tidyverse)
taiwan_dict = read.table('/Users/znovack/Desktop/Git/36490Local/TaiwanDictA.txt', stringsAsFactors = FALSE)
china_dict = read.table('/Users/znovack/Desktop/Git/36490Local/ChinaDictA.txt', stringsAsFactors = FALSE)
garbo_words = c("senator", "will", "president", "yield", "gentleman", "bill", "vote", "amendment", "senate", "house", "unanimous", "may", "mr", "pass", "chairman", "time", "speaker", "states", "now", "one", "committee", "object", "support", "am", "pm", "proceed", "hearing", "revise", "subcommittee", "hearings", "consent", "unanim", "unanimity", "clerk", "order", "follow", "meet", "meeting", "move", "amend", "yielding", "further", "mon", "tue", "wed", "thu", "fri", "sat", "propose", "table", "year", "program", "provide", "can", "new", "legislation", "member", "go", "think")

china_patt = paste(as.character(china_dict$V1), collapse = "(?:^|\\W)|(?:^|\\W)")
china_patt = paste('(?:^|\\W)', china_patt, '(?:^|\\W)', sep = '')
taiwan_patt = paste(as.character(taiwan_dict$V1), collapse = "(?:^|\\W)|(?:^|\\W)")
taiwan_patt = paste('(?:^|\\W)', taiwan_patt, '(?:^|\\W)', sep = '')

ids = docvars(full_corp)$speech_id
full_stops = c(setdiff(stopwords("english"),lexicon::hash_valence_shifters$x), garbo_words)


index.sub = function(corpus, start, stop){
  return (corpus_subset(corpus, speech_id %in% ids[start:stop]))
}

window.truth = function(truth_vect, wind){
  windowed_vect = rep(FALSE, length(truth_vect))
  for (i in 1:length(windowed_vect)){
    # Calculate window range to look at
    window_range = max(i-wind, 1):min(i+wind, length(windowed_vect))
    
    # Apply OR function to window
    windowed_vect[i] = reduce(truth_vect[window_range], function(x, y){x | y})
  }
  return(windowed_vect)
}

parse.dfm.dict.corpus = function(in_dfm, in_corpus,...){
  # Combine multiple dicts if supplied and name them
  filter_dicts = lapply(list(...), unlist)
  names(filter_dicts) = seq(1, length(filter_dicts))
  
  # COnvert dicts to formal dictionary type
  dict = dictionary(filter_dicts)
  
  # Select Subset of dfm corresponding to dictionary
  dfm_dict_mut = dfm_select(in_dfm, dict)
  
  # Find rows with at least one dictionary word
  dfm_row_sum = apply(dfm_dict_mut, 1, sum)
  
  # Get corresponding speech ids
  filt_speeches = docvars(dfm_dict_mut[dfm_row_sum > 0,])$speech_id
  
  # Return filtered corpus
  return (corpus_subset(in_corpus, speech_id %in% filt_speeches))
}

gen.rel.sentences = function(corp, wind, patt){
  out_sent = rep(0.0, length(corp))
  for (i in 1:length(corp)){
    # Get sentences for current speech
    sents = get_sentences(texts(corp)[i])
    
    # Find which sentences contain dictionary words
    truth_vect = grepl(patt, tolower(sents[[1]]))
    
    # Window the sentences
    windowed_vect = window.truth(truth_vect, wind)
    sents[[1]] = tolower(sents[[1]])[windowed_vect]
    wind_speech = paste(sents[[1]], collapse=" ")
    filt_words = strsplit(wind_speech, " ")[[1]]
    texts(corp)[i] = paste(filt_words[!(filt_words %in% full_stops)], sep = " ")
  }
  return (corp)
}




subset_vec = seq(1,1890903,20000)

for (i in 1:length(subset_vec)) {
  cat(i)
  if (i != length(subset_vec)){
    corpus = index.sub(full_corp, subset_vec[i], subset_vec[i+1])
  } else {
    corpus = index.sub(full_corp, subset_vec[i], 1890903)
  }
  dfm_samp = dfm(
    corpus, remove = c(stopwords("english"), garbo_words), remove_punct = TRUE
  )
  china_corp = parse.dfm.dict.corpus(dfm_samp, corpus, china_dict)
  use_corp = gen.rel.sentences(china_corp, 2, china_patt)
  saveRDS(use_corp, paste("subset", i, "corp.rds", sep = "_"))
}