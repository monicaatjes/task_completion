### TRANSLATION ###


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

############################## TEXT PROCESSING - prepare ####################################

p2 <- p1 %>%
  dplyr::select(country, date, task, reason_why,
                pilot_scope, success, satisfaction, other, id, other_task.x, translatedText,
                detectedSourceLanguage) %>%
  dplyr::filter(!is.na(translatedText))

### Remove stop words and data prep

data(stop_words)

p2 <-p2 %>%
  dplyr::select(translatedText, id) %>%
  unnest_tokens(word, translatedText) %>%
  anti_join(stop_words) %>%
  as.data.frame()

############################## TEXT PROCESSING - in order to get the stats [1/4] ############

## word count
text_count <- p2 %>%
  count(word, sort = TRUE) 

resp_counts <- p2 %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

## visualize most used words
plot <- p2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

############################## TEXT PROCESSING - sentiment table [2/4] #####################
nrc <- get_sentiments("nrc") 

try <- p2 %>%
  dplyr::inner_join(nrc) %>%
  dplyr::group_by(sentiment) %>%
  tally()

############################## TEXT PROCESSING - topic modelling [3/4] #####################

topic_unique = unique(p2$word)
for (i in 1:row_number(p2)){
  A = as.integer(p2$word == topic_unique[i])
  p2[topic_unique[i]] = A
}

#create DTM document term matrix
dtm <- CreateDtm(p2$word, 
                 doc_names = p2$id, 
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

### and connect with original
p <- dplyr::left_join(TC_BE_other, translation1)
p1 <- dplyr::left_join(TC_BE_other2, p, by=("id"))  

############################## TEXT PROCESSING - prepare ####################################

p4 <- p1 %>%
  dplyr::select(country, date, task, reason_why,
                pilot_scope, success, satisfaction, other, id, other_task.x, translatedText,
                detectedSourceLanguage) %>%
  dplyr::filter(!is.na(translatedText))


## creating two words that are used together often and connect it back to the origin
p3 <- p4 %>%
  dplyr::mutate(
    text = translatedText
  ) %>%
  unnest_tokens(other_task, text, token = "ngrams", n = 2)

p3 <- p3 %>%
  separate(other_task, c("word1", "word2"), sep = " ")

p3_filtered <- p3 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# ngrams count
p3_counts <- p3_filtered %>% 
  count(word1, word2, sort = TRUE)

#### TRY TO CALCULATE SUCCESS SCORE AND SATISFACTION FOR OTHER TASK ####

p_other_task <- p3 %>%
  distinct(id, .keep_all = T) %>%
  dplyr::mutate(
    task_other = case_when(
      word1 =="account" & word2 =="statement" | word2 =="statement2" ~ "Een bericht lezen of een document downloaden",
      word1 =="find" & word2 =="the" ~ "Een bericht lezen of een document downloaden",
      word1 =="get" & word2 =="statements" ~ "Een bericht lezen of een document downloaden",
      word1 =="download" | word2 =="statement" ~ "Een bericht lezen of een document downloaden",
      word1 =="upload" ~ "Een bericht lezen of een document downloaden",
      word1 =="export"  ~ "Een bericht lezen of een document downloaden",
      word1 =="print"  ~ "Een bericht lezen of een document downloaden",
      word1 =="printing"  ~ "Een bericht lezen of een document downloaden",
      word1 =="sign"  ~ "Een bericht lezen of een document downloaden",
      word1 =="certificate" ~ "Een bericht lezen of een document downloaden",
      word1 =="check" & word2 =="payment" ~ "Een betaling controleren",
      word1 =="check" & word2 =="payments" ~ "Een betaling controleren",
      word1 =="search"~ "Een betaling controleren",
      word1 =="see"~ "Een betaling controleren",
      word1 =="view"~ "Een betaling controleren",
      word1 =="verify"~ "Een betaling controleren",
      word1 =="check" & word2 =="if" ~ "Klachtafhandeling",
      word1 =="address" & word2 =="change" ~ "Een wijziging aanbrengen in persoonsgegevens",
      word1 =="update|updating" ~ "Een wijziging aanbrengen in persoonsgegevens",
      word1 =="change" & word2 =="the" ~ "Een wijziging aanbrengen in persoonsgegevens",
      word1 =="apply" & str_detect(translatedText, "credit card") ~ "Een credit card aanvragen",
      str_detect(translatedText, "credit card") ~ "Een credit card aanvragen",
      str_detect(translatedText, "closing") ~ "Een rekening opzeggen",
      word1 =="activate" & word2 =="app" ~ "Mijn app activeren",
      word1 =="activate" & word2 =="my" ~ "Een wijziging aanbrengen aan een rekening of kaart",
      word1 =="adjust" & word2 =="direct" ~ "Een wijziging aanbrengen aan een rekening of kaart",
      word1 =="adjust" & word2 =="payment" ~ "Een wijziging aanbrengen aan een rekening of kaart",
      word1 =="change" & word2 =="standing" ~ "Een wijziging aanbrengen aan een rekening of kaart",
      word1 =="account" & word2 =="searches" ~ "Een betaling controleren",
      word1 =="applicatio" | word1=="irregularity"  ~ "Probleem met app",
      word1 =="fear"  ~ "Fishing vermoeden",
      word1 =="cancel"  ~ "Een afspraak afzeggen",
      word1 =="cardreader" ~ "Card reader activeren",
      word1 =="applying" & word2 =="for" ~ "Een hypotheek aanvragen",
      word1 =="bank" & word2 =="transfer" ~ "Een betaling doen",
      word1 =="bank" & word2 =="account" ~ "Een bankrekening aanvragen voor mijn kind",
      word1 =="block"  ~ "Een rekening of kaart blokkeren",
      word1 =="bank" & word2 =="transfer" ~ "Een betaling doen",
      word1 =="check" | word1 =="checking" ~ "Een betaling controleren",
      word1 =="close" ~ "Een rekening sluiten",
      word1 =="consult" & word2 =="insurance" ~ "Mijn belegging(en) / verzekering(en) / lening(en) raadplegen",
      word1 =="consult" | word1 =="consultation" ~ "Mijn saldo checken",
      word1 =="find" | word1 =="consultation" ~ "Mijn saldo checken",
      word1 =="invoice" ~ "Een betaling controleren",
      word1 =="report" ~ "Een overlijden rapporteren",
      word1 =="look"| word1 =="looking"  ~ "Een betaling controleren",
      word1 =="modification"| word1 =="modifier" | word1 =="modify"  ~ "Een wijziging aanbrengen aan een rekening of kaart",
      str_detect(translatedText, "phone") ~ "Mijn app activeren",
      str_detect(translatedText, "cancel") ~ "Een rekening of kaart blokkeren",
      str_detect(translatedText, "customer service") ~ "Klachtafhandeling",
      str_detect(translatedText, "info about a loan") ~ "Informatie opzoeken over een product of dienst",
      str_detect(translatedText, "Make a paymennt") ~ "Een betaling doen",
      str_detect(translatedText, "make|Make") ~ "Een afspraak maken",
      str_detect(translatedText, "phone application|app") ~ "Probleem met app",
      str_detect(translatedText, "Sell funds") ~ "Manage my invedtments"
    )
  )
  
