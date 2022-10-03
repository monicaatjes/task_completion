### sentences that describe the reason why not
TC_NL_IN_why_not <- TC_NL_IN %>%
  dplyr::select(id, reason_why) 

first_letter_upper=function(x) ifelse(is.na(x)==TRUE, NA, paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))))

TC_NL_IN_why_not$reason_why <-  sapply(as.character(TC_NL_IN_why_not$reason_why), str_squish) 
TC_NL_IN_why_not$reason_why <-  sapply(TC_NL_IN_why_not$reason_why, first_letter_upper)

sentences <- as.character(TC_NL_IN_why_not$reason_why)




## text analysis prep
sentences_new <- sentences %>% 
  sapply(., str_squish) %>%
  sapply(., first_letter_upper) %>%
  na.omit %>%
  data.frame(stringsAsFactors = FALSE) %>%
  dplyr::filter(str_detect(. , ""))

### authenticate & translate
gl_auth("~/Documents/task_completion/keen-shape-354712-2b896bfa9c7c.json")
translation_sentences <- gl_translate(
  sentences_new$.,
  target = "en",
  format = c("text"),
  source = "")

### TEXT PROCESSING

### put translation in seperate file
translation1 <- translation_sentences %>%
  dplyr::rename(
    "reason_why" =text,
    "translatedText_why" =translatedText,
    "detectedSourceLanguage_why"= detectedSourceLanguage
  )

### re name variable in file with row number to connect it back to original source


### and connect with original 

## check ff of translated text is not already in the file
s <- dplyr::left_join(translation1, TC_NL_IN_why_not ,  by=c("reason_why"))
p1 <- dplyr::left_join(TC_NL_IN, s, by=c("id"))  


############################## TEXT PROCESSING - prepare ####################################

p2 <- p1 %>%
  dplyr::select(country, date, task, reason_why.x,
                pilot_scope, success, satisfaction, id, translatedText_why,
                detectedSourceLanguage_why) %>%
  dplyr::filter(!is.na(translatedText_why))

### Remove stop words and data prep

data(stop_words)

s2 <-p2 %>%
  dplyr::select(translatedText_why, id) %>%
  unnest_tokens(word, translatedText_why) %>%
  anti_join(stop_words) %>%
  as.data.frame()

############################## TEXT PROCESSING - in order to get the stats [1/4] ############


## word count
text_count <- s2 %>%
  count(word, sort = TRUE) 

resp_counts <- s2 %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

## visualize most used words
plot <- s2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs()

############################## TEXT PROCESSING - sentiment table [2/4] #####################
nrc <- get_sentiments("nrc") 

try <- s2 %>%
  dplyr::inner_join(nrc) %>%
  dplyr::group_by(sentiment) %>%
  tally()

############################## TEXT PROCESSING - topic modelling [3/4] #####################

topic_unique = unique(s2$word)
for (i in 1:row_number(s2)){
  A = as.integer(s2$word == topic_unique[i])
  s2[topic_unique[i]] = A
}

#create DTM document term matrix
dtm <- CreateDtm(s2$word, 
                 doc_names = s2$id, 
                 ngram_window = c(1, 2))
#explore the basic frequency
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)
# Eliminate words appearing less than 2 times or in more than half of the
# documents
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
dtm = dtm

k_list <- seq(1, 20, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)
model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines

#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")

model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]
model$top_terms <- GetTopTerms(phi = model$phi, M = 20)
top20_wide <- as.data.frame(model$top_terms)

model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
plot(model$hclust)

## top terms
final_summary_words <- data.frame(top_terms = t(model$top_terms))

####STRAIGHT FORWARD TEXT ANALYSIS USING NGRAMS (==frequently used words in pairs) [4/4]####



############################## TEXT PROCESSING - prepare ####################################

p4 <- p1 %>%
  dplyr::select(country, date, task,
                pilot_scope, success, satisfaction, id, reason_why.y, translatedText_why,
                detectedSourceLanguage_why) %>%
  dplyr::filter(!is.na(translatedText_why))


## creating two words that are used together often and connect it back to the origin
p3 <- p4 %>%
  dplyr::mutate(
    text = translatedText_why
  ) %>%
  unnest_tokens(reason_why.y, text, token = "ngrams", n = 2)

p3 <- p3 %>%
  separate(reason_why.y, c("word1", "word2"), sep = " ")

p3_filtered <- p3 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# ngrams count
p3_counts <- p3_filtered %>% 
  count(word1, word2, sort = TRUE)

p_why_not <- p3 %>%
  distinct(id, .keep_all = T) 

# Connect to the fail tasks

TC_NL_INz <- dplyr::left_join(TC_NL_IN, p_why_not, by=c("id"))

## Show fail reason per task

fail_list <- TC_NL_INz %>%
  dplyr::select(date.x, reason_why, task.x, country.x, pilot_scope.x, success.x, satisfaction.x, id, 
                translatedText_why, detectedSourceLanguage_why, word1.y, word2.y) %>%
  dplyr::filter(success.x =="Nee") %>%
  dplyr::mutate(
    fail_reason = case_when(
      str_detect(translatedText_why, "find|looking|finding|Can not find|Can't find|
                 I am looking |I am missing") ~ "cannot find",
      str_detect(translatedText_why, "expensive|cheaper elsewhere|too high") ~ "too expensive",
      str_detect(translatedText_why, "app hanging|system|unavailable|not visible
                 |blank screen|possible|failed|bad|broken|waited 18 min|inaccessible|work|
                 error message|wrong|error") ~ "technical issue",
      str_detect(translatedText_why, "app|hangs") ~ "problem with app",
      str_detect(translatedText_why, "bugging|asked for an appointment|office holder
                 |stopped changing|is not possible|in an agency|sell funds
                 |new version is really not intuitive") ~ "bad service",
      str_detect(translatedText_why, "scoorter|Adjustment|no select options|s not stated") ~ "option not available",
      TRUE ~ "other"
    )
  )

fail_list_overview <- fail_list %>%
  dplyr::group_by(task.x, fail_reason) %>%
  tally() %>%
  tidyr::spread(task.x, n)



