### Play script to get text analysis working



### other bucket: filter out the answers + give it an unique ID
TC_BE_other <- TC_BE %>%
  dplyr::mutate(other = case_when(
    str_detect(task, "namelijk") ~ "other",
    TRUE ~ "list"
  )) %>%
  dplyr::mutate(
    id = as.numeric(row_number())
  ) %>%
  dplyr::select(id, other, task) %>%
  dplyr::mutate(other_task= case_when(
    other =="other" ~ sub(".*:", "", task),
    TRUE ~ "list"
  )) %>%
  dplyr::filter(other =="other") %>%
 dplyr::select(other_task, id) 

TC_BE_other2 <- TC_BE %>%
  dplyr::mutate(other = case_when(
    str_detect(task, "namelijk") ~ "other",
    TRUE ~ "list"
  )) %>%
  dplyr::mutate(
    id = as.numeric(row_number())
  ) %>%
  dplyr::mutate(other_task= case_when(
    other =="other" ~ sub(".*:", "", task),
    TRUE ~ "list"
  )) 


### remove not needed spaces
TC_BE_other$other_task <- str_squish(TC_BE_other[,1])

### clean up kapitals etc
first_letter_upper=function(x) ifelse(is.na(x)==TRUE, NA, paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))))
TC_BE_other$other_task <- first_letter_upper(TC_BE_other$other_task)

### Language detection is not great. But lets move on with FR & NL
detected_language <- as.data.frame(detect_language(TC_BE_other$other_task), as.character(TC_BE_other$other_task)) %>%
  dplyr::group_by(`detect_language(TC_BE_other$other_task)`) %>%
  tally() 

### authenticate & translate
gl_auth("~/Documents/task_completion/keen-shape-354712-2b896bfa9c7c.json")
translation <- gl_translate(
    TC_BE_other$other_task,
    target = "en",
    format = c("text"),
    source = "")

### put translation in seperate file
translation1 <- translation %>%
  dplyr::rename(
    "other_task" =text
  )

### re name variable in file with row number to connect it back to original source
TC_BE_other <- TC_BE_other %>%
  dplyr::mutate(
  other_task= sub(".*:", "", other_task)
)
TC_BE_other$other_task <- str_squish(TC_BE_other[,1])

### and connect with original
p <- dplyr::left_join(TC_BE_other, translation1)
p1 <- dplyr::left_join(TC_BE_other2, p, by=("id"))  

### sentences that describe the reason why not
sentences <- as.character(TC_BE[,15])
first_letter_upper=function(x) ifelse(is.na(x)==TRUE, NA, paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))))

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



## From here text processing -> use translated column as input
text_df <- tibble(line = 1:nrow(sentences_new), text = sentences_new$.) %>%
  dplyr::mutate(
    id = as.numeric(row_number())
  ) 



## Remove stop words
data(stop_words)

text_df <- text_df %>%
  dplyr::select(text, id) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  as.data.frame()



### text analysis - in order to get the stats [1/3]

## word count
text_count <- text_df %>%
  count(word, sort = TRUE) 

resp_counts <- text_df %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

## visualize most used words
plot <- text_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

### create a sentiment table [2/3]
nrc <- get_sentiments("nrc") 

try <- text_df %>%
  dplyr::inner_join(nrc) %>%
  dplyr::group_by(sentiment) %>%
  tally()

### topic moddeling [3/3]


topic_unique = unique(text_df$word)
for (i in 1:row_number(text_df)){
  A = as.integer(text_df$word == topic_unique[i])
  text_df[topic_unique[i]] = A
}



#create DTM document term matrix
dtm <- CreateDtm(text_df$word, 
                 doc_names = text_df$id, 
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


#visualising topics of words based on the max value of phi
set.seed(1234)
final_summary_words <- data.frame(top_terms = t(model$top_terms))
final_summary_words$topic <- rownames(final_summary_words)
rownames(final_summary_words) <- 1:nrow(final_summary_words)
final_summary_words <- final_summary_words %>% melt(id.vars = c("topic"))
final_summary_words <- final_summary_words %>% rename(word = value) %>% select(-variable)
final_summary_words <- left_join(final_summary_words,allterms)
final_summary_words <- final_summary_words %>% group_by(topic,word) %>%
  arrange(desc(value))
final_summary_words <- final_summary_words %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
  ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
word_topic_freq <- left_join(final_summary_words, original_tf, by = c("word" = "term"))
pdf("cluster.pdf")
for(i in 1:length(unique(final_summary_words$topic)))
{  wordcloud(words = subset(final_summary_words ,topic == i)$word, freq = subset(final_summary_words ,topic == i)$value, min.freq = 1,
             max.words=200, random.order=FALSE, rot.per=0.35, 
             colors=brewer.pal(8, "Dark2"))}
dev.off()


allterms <- as.data.frame(text_df$word) %>%
  dplyr::mutate(
    word =text_df$word
  ) %>%
  dplyr::select(word)






