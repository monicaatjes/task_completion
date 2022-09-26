
first_letter_upper=function(x) ifelse(is.na(x)==TRUE, NA, paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2))))


sentences <- as.character(TC_BE[,15])

## text analysis prep
sentences_new <- sentences %>% 
  sapply(., str_squish) %>%
  sapply(., first_letter_upper) %>%
  na.omit %>%
  data.frame(stringsAsFactors = FALSE) %>%
  dplyr::filter(str_detect(. , ""))

detected_language <- sentences_new %>% 
  sapply(., map_chr, detect_language) %>% 
  data.frame(check.names = FALSE)




## from fr to eng
for (i in 1:ncol(sentences_new)){
  # we replace all the elements in sentences_new that are =="en":
  sentences_new[,i][detected_language[,i] =="fr" & !is.na(detected_language[,i])] <- 
    # with their translation (gl_translate) to target="it" then we transform it to a dataframe and makes sure all missing values are in NA form:
    data.frame(gl_translate(sentences_new[,i][detected_language[,i] =="fr" & !is.na(detected_language[,i])], target = "en"))[,1]
  
  sentences_new[,i][sentences_new[,i] %in% c("NA", "N/a", "N/A", "Na", "na", "n/a", "not applicable")] <- NA
}

## from nl to eng
for (i in 1:ncol(sentences_new)){
  # we replace all the elements in sentences_new that are =="en":
  sentences_new[,i][detected_language[,i] =="nl" & !is.na(detected_language[,i])] <- 
    # with their translation (gl_translate) to target="it" then we transform it to a dataframe and makes sure all missing values are in NA form:
    data.frame(gl_translate(sentences_new[,i][detected_language[,i] =="en" & !is.na(detected_language[,i])], target = "en"))[,1]
  
  sentences_new[,i][sentences_new[,i] %in% c("NA", "N/a", "N/A", "Na", "na", "n/a", "not applicable")] <- NA
}