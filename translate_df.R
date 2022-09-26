translate_df <- function(dataset = NULL,
                         #column.name= NULL,
                         target.lang = "EN",
                         add.source.lang = FALSE,
                         url = "https://api.deepl.com/v1/translate?text=",
                         auth_key = NULL
) {
  
  
  if(is.null(auth_key)){cat("You need an API key. See https://www.deepl.com/api-contact.html.")}else{
    # INPUT: Dataframe with text in column ####
    if(inherits(dataset,"data.frame")==TRUE&!is.null(column.name)){
      dataset2 <- dataset %>% dplyr::pull(column.name) %>% as.character()
      responses <- NULL
      source_lang <- NULL
      z <- 0
      for(i in dataset2){
        svMisc::progress(z, max.value = length(dataset2))
        z <- z+1
        i <- stringr::str_replace(gsub("\\s+", "%20", stringr::str_trim(i)), "B", "b")
        # Source language: "detect" vs. "X"
        if(source.lang=="detect"){
          response.i <- httr::GET(paste(url,
                                        i,
                                        "&target_lang=", target.lang,
                                        "&auth_key=", auth_key
                                        , sep = ""))
        }else{
          response.i <- httr::GET(paste(url,
                                        i,
                                        "&source_lang=", source.lang,
                                        "&target_lang=", target.lang,
                                        "&auth_key=", auth_key
                                        , sep = ""))
        }
        respcontent.i <- httr::content(response.i, as="text", encoding = "UTF-8")
        #print(respcontent.i)
        
        result.i <- jsonlite::fromJSON(respcontent.i)$translations$text
        # Source language: "detect" vs. "X"
        if(source.lang == "detect"){
          source.lang.i <- jsonlite::fromJSON(respcontent.i)$translations$detected_source_language
        }else{
          source.lang.i <- jsonlite::fromJSON(respcontent.i)$translations$detected_source_language
        }
        #print(result.i)
        responses <- c(responses, result.i)
        #print(responses)
        source_lang <- c(source_lang, source.lang.i)
      }
      column.name.new <- paste0(column.name, "_", target.lang)
      dataset <- dplyr::bind_cols(dataset, newtranslation = responses)
      names(dataset)[names(dataset)=="newtranslation"] <- column.name.new
      # OUPUT

      if(add.source.lang == TRUE){
        return(data.frame(cbind(dataset, source_lang)))
      }else{
        return(dataset)
      }
    }else{
      cat("The input is not of class() dataframe or you forgot to specify the name of the column that shall be translated.")
    }
  } # API KEY
} # End of function


translated <- TC_BE_other %>%
  dplyr::mutate(
    other_task = as.character(other_task)
  ) %>%
  dplyr::mutate(
    language = case_when(
      detect_language(other_task) =="nl" ~ "nl",
      detect_language(other_task) =="fr" ~ "fr",
      detect_language(other_task) =="en" ~ "en",
      is.na(detect_language(other_task)) ~ "en",
    )
  ) %>% 
  dplyr::filter(!is.na(language)) 
p<-   sapply(translate_df(translated$other_task), target ="en", source="",




