#### main ###

### open libaries ###
library(haven)
library(tidyr)
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(psych)
library(GPArotation)
library(lm.beta)
library(mctest)
library(plotly)
library(dplyr)
library(stringr)
#library(ingmarkdowntemplates)
library(ggplot2)
library(ggflags)
library(lubridate)
library(png)
library(ggimage)
library(readxl)
library(tidyxl)
library(data.table)
library(zoo)
library(shiny)
library(corrr)
library(tidyverse)
library(caret)
library(car)
library(gt)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(shiny)
library(shinydashboard)
library(cld3)
library(Rcpp)
library(tidytext)
library(textdata)
library(sentimentr)
library(topicmodels)
library(translateR)
library(float)
library(textmineR)
library(text2vec)
library(googleLanguageR)
library(googleAuthR)
library(devtools)
library(deeplr)
library(dplyr)
library(tidytext)
library(janeaustenr)

#install.packages("devtools")
#devtools::install_github("paulcbauer/deeplr")
#install.packages("remotes")
#remotes::install_github("paulcbauer/deeplr", force = T)

### Open data
#16-6 t/m 15-7
TC_BE <- read.csv("~/Documents/task_completion/data/Export_3bfb0b6a-9842-449b-8d31-d1a2a657eab7 (14).csv" , sep=";")
#15-7 t/m 25-7
TC_BE2 <- read.csv("~/Documents/task_completion/data/Export_3bfb0b6a-9842-449b-8d31-d1a2a657eab7 (16).csv" , sep=";")


# test

TC_BE_try1 <- read.csv("~/Documents/task_completion/data/Export_3bfb0b6a-9842-449b-8d31-d1a2a657eab7 (18).csv" , sep=";")
TC_BE_try2 <- read.csv("~/Documents/task_completion/data/Export_3bfb0b6a-9842-449b-8d31-d1a2a657eab7 (19).csv" , sep=";")

# NL Insurance all in one flow 10/7- 9/8
TC_NL_IN <- read.csv("~/Documents/task_completion/data/Export_56467b14-488c-46d2-93f5-c6cd49fb0b66.csv" , sep=";")


### First change column names as of column 12

#TC_BE2 <- read_excel("~/Documents/task_completion/data/Export_3bfb0b6a-9842-449b-8d31-d1a2a657eab7 (10).xlsx")

TC_BE2 <- TC_BE2 %>%
  dplyr::select(Date, Operating.system, Browser, Browser.version, Page.URL, Audience, Language,
                Device.OS.version, Device.Manufacturer, Device.Model, App.version, 
                Dans.quelle.mesure.êtes.vous.satisfait.de.votre.visite.dans.Home.Bank.,
                Quelle.est.la.raison.principale.pour.laquelle.vous.venez.d.utiliser.Home.Bank., 
                Avez.vous.réussi.à.effectuer.cette.opération..Avez.vous.trouvé.ce.que.vous.cherchiez.,
                Pourriez.vous.nous.dire.pourquoi.vous.n.êtes.pas.satisfait.de.votre.visite.dans.Home.Bank.,
                Vervelend.dat.het.niet.gelukt.is.) %>%
  dplyr::rename(
    #"Dans.quelle.mesure.êtes.vous.satisfait.de.votre.visite.dans.Home.Bank." =Hoe.tevreden.ben.je.over.je.bezoek.aan.Home.Bank.,
    "Hoe.tevreden.ben.je.over.je.bezoek.aan.Home.Bank." =Dans.quelle.mesure.êtes.vous.satisfait.de.votre.visite.dans.Home.Bank.,
    #"Quelle.est.la.raison.principale.pour.laquelle.vous.venez.d.utiliser.Home.Bank." =Wat.is.de.belangrijkste.reden.waarom.je.zonet.Home.Bank.hebt.gebruikt.,
    "Wat.is.de.belangrijkste.reden.waarom.je.zonet.Home.Bank.hebt.gebruikt." =Quelle.est.la.raison.principale.pour.laquelle.vous.venez.d.utiliser.Home.Bank.,
    "Is.dit.gelukt." =Avez.vous.réussi.à.effectuer.cette.opération..Avez.vous.trouvé.ce.que.vous.cherchiez.,
    #"Avez.vous.réussi.à.effectuer.cette.opération..Avez.vous.trouvé.ce.que.vous.cherchiez." =Is.dit.gelukt.,
    "Waarom.ben.je.ontevreden.over.je.bezoek.aan.Home.Bank.."= Pourriez.vous.nous.dire.pourquoi.vous.n.êtes.pas.satisfait.de.votre.visite.dans.Home.Bank.,
    #"Pourriez.vous.nous.dire.pourquoi.vous.n.êtes.pas.satisfait.de.votre.visite.dans.Home.Bank."= Waarom.ben.je.ontevreden.over.je.bezoek.aan.Home.Bank.,
    "Nous.sommes.navrés.de.l.entendre."= Vervelend.dat.het.niet.gelukt.is.
    #"Vervelend.dat.het.niet.gelukt.is."= Nous.sommes.navrés.de.l.entendre.
    #,
    #"Operating.system" = `Operating system`,
    #"Browser.version" = `Browser version`,
    #"Page.URL" = `Page URL`,
    #"Device.OS.version" = `Device OS version`,
    #"Device.Manufacturer" = `Device Manufacturer`,
    #"Device.Model" = `Device Model`,
    #"App.version" = `App version`
  )

TC_BE <- dplyr::full_join(TC_BE, TC_BE2)


#TC_BE$reason_why <- TC_BE$ `Vervelend dat het niet gelukt is.`
TC_BE$reason_why <- TC_BE[,16]

#TC_BE$task <- TC_BE$`Quelle.est.la.raison.principale.pour.laquelle.vous.venez.d.utiliser.Home.Bank.`
TC_BE$task <- TC_BE[,13]

TC_BE$country <- "Belgium"
TC_BE$pilot_scope <- "ing.be"
TC_BE$success <-  TC_BE[,14]


### a bit of tranformation  
TC_BE <- TC_BE %>%
  dplyr::mutate(
    satisfaction = case_when(
      TC_BE[,12] =="mad" ~ 1,
      TC_BE[,12] =="sad" ~ 2,
      TC_BE[,12] =="neutral" ~ 3,
      TC_BE[,12] =="happy" ~ 4,
      TC_BE[,12] =="love" ~ 5,
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
    language = case_when(
      TC_BE[,7] == "nl-BE" ~ "NL",
      TC_BE[,7] == "fr-BE" ~ "FR",
      TC_BE[,7]== "en-GB" ~ "EN",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::mutate(
    id = as.numeric(row_number())
  ) 




### aggregation closed questions
TC_BE_SUCCESS <- TC_BE %>%
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

TC_BE_SUM <- TC_BE %>%
  dplyr::group_by(task) %>%
  dplyr::summarize(
    satisfaction = mean(satisfaction, na.rm=T)
  ) %>%
  dplyr::ungroup() 

TC_BE_SUCCESS <- bind_cols(TC_BE_SUCCESS, TC_BE_SUM)

TC_BE_SUCCESS <- TC_BE_SUCCESS %>%
  dplyr::filter(!str_detect(task...1, "Een andere reden")) %>%
  dplyr::select(task...1, Ja, Nee, total, success_score, satisfaction)

### open fields

### Calculate success score again with the other bucket included

# first connect labelled categories back 2 orgin
TC_BE <- dplyr::left_join(TC_BE, p_other_task) %>%
  dplyr::mutate(task = case_when(
    str_detect(task, "Een andere reden") ~ task_other,
    TRUE ~ task)
  )

TC_BE_SUCCESS2 <- TC_BE %>%
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

TC_BE_SUM2 <- TC_BE %>%
  dplyr::filter(!is.na(satisfaction)) %>%
  dplyr::group_by(task) %>%
  dplyr::summarize(
    satisfaction = mean(satisfaction, na.rm=T)
  ) %>%
  dplyr::ungroup() 

TC_BE_SUCCESS2 <- dplyr::left_join(TC_BE_SUCCESS2, TC_BE_SUM2) 

TC_BE_SUCCESS2_continues <- TC_BE_SUCCESS2 %>%
  dplyr::filter(!is.na(satisfaction)) %>%
  dplyr::mutate(
    index_satisfaction = satisfaction / max(satisfaction) *100,
    index_success = success_score *100
  )


#update.packages("Rcpp")
#detected_language <- sentences_new %>% 
#  sapply(., map_chr, detect_language) %>% 
#  data.frame(check.names = FALSE)

