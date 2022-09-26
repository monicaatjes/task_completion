## Specific script for NL Insurances 

TC_NL_IN$reason_why <- TC_NL_IN[,15]

#TC_BE$task <- TC_BE$`Quelle.est.la.raison.principale.pour.laquelle.vous.venez.d.utiliser.Home.Bank.`
TC_NL_IN$task <- TC_NL_IN[,12]

TC_NL_IN$country <- "The Netherlands"
TC_NL_IN$pilot_scope <- "ing.nl_insurances"
TC_NL_IN$success <-  TC_NL_IN[,13]


### a bit of tranformation  
TC_NL_IN <- TC_NL_IN %>%
  dplyr::mutate(
    satisfaction = case_when(
      TC_NL_IN[,14] =="Heel erg ontevreden" ~ 1,
      TC_NL_IN[,14] =="Ontevreden" ~ 2,
      TC_NL_IN[,14] =="Neutraal" ~ 3,
      TC_NL_IN[,14] =="Tevreden" ~ 4,
      TC_NL_IN[,14] =="Heel tevreden" ~ 5,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::mutate(
    satisfaction = as.numeric(as.character(satisfaction)),
    # success = as.numeric(as.character(success)),
    task = as.character(task)
  ) %>%
  dplyr::mutate(
    date = sub("\\T.*", "", Date)
  ) %>%
  dplyr::mutate(
    Date = as.Date(Date)
  ) %>%
  dplyr::mutate(
    id = as.numeric(row_number())
  ) 


### aggregation closed questions
TC_NL_IN_SUCCESS <- TC_NL_IN %>%
  dplyr::filter(success !="Je vraagt het te vroeg") %>%
  dplyr::group_by(task, success) %>%
  dplyr::tally() %>%
  dplyr::ungroup() %>%
  tidyr::spread(success, n) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))  %>%
  dplyr::mutate(
    total = Ja + Nee
  ) %>%
  dplyr::mutate(
    success_score = Ja / total
  )

TC_NL_IN_SUM <- TC_NL_IN %>%
  dplyr::filter(success !="Je vraagt het te vroeg") %>%
  dplyr::group_by(task) %>%
  dplyr::summarize(
    satisfaction = mean(satisfaction, na.rm=T)
  ) %>%
  dplyr::ungroup() 

TC_NL_IN_SUCCESS <- dplyr::left_join(TC_NL_IN_SUCCESS, TC_NL_IN_SUM)

