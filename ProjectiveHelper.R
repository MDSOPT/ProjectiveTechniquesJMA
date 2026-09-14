#ProjectiveHelper.R
#This includes helper functions for the quantitative analyses for the replication of the paper
#Perspectives on Projective Techniques in Consumer Research: A Review, Primary Analysis, and Methodological Guidance
#It includes the topic modeling, chi-squared tests, and correspondence analysis for the word association and sentence completion data.
#Copyright Stephen France 2026: sfrance@business.msstate.edu
#Please see license.txt in Github for the license for this code and the data files.  

if (!require("tidytext")) install.packages("tidytext")
library(tidytext)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("stopwords")) install.packages("stopwords")
library(stopwords)
if (!require("reshape2")) install.packages("reshape2")
library(reshape2)
if (!require("quanteda")) install.packages("quanteda")
library(quanteda)
if (!require("stm")) install.packages("stm")
library(stm)

STMTopicModel<-function(MeltData,Maxk=16)
{

  
  # Starting point:
  # MeltData2 has columns doc_id and text
  
  corp <- corpus(MeltData, text_field = "Sentence", docid_field = "IDCity")
  
  toks <- tokens(
    corp,
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE
  )
  
  toks <- tokens_tolower(toks)
  
  # Remove stopwords before forming bigrams
  toks <- tokens_remove(toks, stopwords("en"))
  
  # Create unigrams + bigrams
  toks_ngrams <- tokens_ngrams(toks, n = 1:2)
  
  dfm_ngrams <- dfm(toks_ngrams)
  
  # Optional trimming
  # Since you said you may want rare tokens, keep this gentle
  dfm_ngrams <- dfm_trim(
    dfm_ngrams,
    min_termfreq = 2
  )
  
  # Convert quanteda dfm to stm format
  stm_input <- convert(dfm_ngrams, to = "stm")

  docs  <- stm_input$documents
  vocab <- stm_input$vocab
  meta  <- stm_input$meta
  
  set.seed(588)
  
  stm_fit <- stm(
    documents = docs,
    vocab = vocab,
    K = Maxk,
    data = meta,
    prevalence = ~ City,
    init.type = "Spectral"
  )
  return(list(stm_input=stm_input,stm_fit=stm_fit))
  
}


STMTopicModelSearch<-function(MeltData,kVector)
{
  
  
  # Starting point:
  # MeltData2 has columns doc_id and text
  
  corp <- corpus(MeltData, text_field = "Sentence", docid_field = "IDCity")
  
  toks <- tokens(
    corp,
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE
  )
  
  toks <- tokens_tolower(toks)
  
  # Remove stopwords before forming bigrams
  toks <- tokens_remove(toks, stopwords("en"))
  
  # Create unigrams + bigrams
  toks_ngrams <- tokens_ngrams(toks, n = 1:2)
  
  dfm_ngrams <- dfm(toks_ngrams)
  
  # Optional trimming
  # Since you said you may want rare tokens, keep this gentle
  dfm_ngrams <- dfm_trim(
    dfm_ngrams,
    min_termfreq = 2
  )
  
  # Convert quanteda dfm to stm format
  stm_input <- convert(dfm_ngrams, to = "stm")
  
  docs  <- stm_input$documents
  vocab <- stm_input$vocab
  meta  <- stm_input$meta
  
  set.seed(588)
  
  k_result <- searchK(
    documents = docs,
    vocab = vocab,
    K = kVector,
    data = meta,
    prevalence = ~ City,
    init.type = "Spectral"
  )
  
  return(k_result)
  
}

PlotTopicResults<-function(k_result,ChooseMetrics=NULL){
  k_long_all <- k_result$results[,1:5] %>%
    pivot_longer(
      cols = -K,
      names_to = "Measure",
      values_to = "Value"
    ) %>%
    mutate(
      K = as.numeric(unlist(K)),
      Value = as.numeric(unlist(Value))
    )
  if (!is.null(ChooseMetrics))
  {
    k_long_all<-k_long_all %>%
      filter(Measure %in% ChooseMetrics)
  }
  
  k_long_all <- k_long_all %>%
    mutate(
      MeasureLabel = recode(
        Measure,
        semcoh   = "Semantic coherence\nHigher is better",
        exclus   = "Exclusivity\nHigher is better",
        residual = "Residuals\nLower is better",
        heldout  = "Held-out likelihood\nHigher is better",
        bound    = "Bound\nHigher is better",
        lbound   = "Lower bound\nHigher is better",
        em.its   = "EM iterations\nDiagnostic only"
      )
    )
  
  ResPlot<-ggplot(k_long_all, aes(x = K, y = Value)) +
    geom_point(size = 2) +
    geom_line() +
    facet_wrap(~ MeasureLabel, scales = "free_y") +
    scale_x_continuous(
      breaks = sort(unique(k_long_all$K))
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size=11),
      axis.text.y = element_text(size=11),
      axis.title.x = element_text(size=12,face="bold"),
      strip.text = element_text(size=12,face="bold")
    ) +
    labs(
      x = "Number of Topics (K)",
      y = NULL
    )
  ResPlot
  return(ResPlot)
}

clean_mojibake <- function(x) {
  x <- gsub("â€œ|â€|â€\u009d", "\"", x)
  x <- gsub("â€˜|â€™", "'", x)
  x <- gsub("â€“", "-", x)
  x <- gsub("â€”", "-", x)
  x <- gsub("â€¦", "...", x)
  x <- gsub("â€", "\"", x, fixed = TRUE)
  x
}
