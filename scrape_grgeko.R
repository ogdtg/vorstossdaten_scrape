
library(rlang)
library(rvest)
library(dplyr)
library(stringr)

scrape_grgeko <- function(legislatur = 2020) {
  
  # Variablen Laden um Felder zu identifizieren
  variables <- readRDS("variables.rds")
  
  # Datensatz mit Abkürzungen der einzelnen Geschaefte für späteren Join laden
  join_ga <- readRDS("kennung.rds")
  
  # Listen initialisieren
  
  document_list <- list() # Dokumente
  data_list <- list() # Daten zu Pol. Geschaefte
  names_list <- list() # VorstoesserInnen
  
  # Counter für Nummern ohne Dokument (relevant für Abbruchbedingung)
  nodoc_counter <- 0
  
  
  for (i in 1:10000) {
    print(i)
    # Seite lesen, wenn möglich
    html <- tryCatch({
      rvest::read_html(glue::glue("https://grgeko.tg.ch/view?legislatur={legislatur}&grgnum={i}"))
    },
    error=function(cond) {
      NA
    })
  
    if(is.na(html)) {
      print(glue::glue("Kein Dokument mit grgr-Nummer {i}"))
      nodoc_counter <- nodoc_counter+1
      print(paste0("Bisher ",nodoc_counter," aufeinanderfolgende fehlende grg Nummern"))
      
    }

    # Wenn mehr als 20 aufeinander folgende Nummern kein Dokument liefern sind alle Dokumente gescraped -> Funktion bricht ab
    if (nodoc_counter == 40) {
      print("keine weiteren Eintragungen")
      break
    }
    
    # Zur nächsten Nummer, wenn keine Seite existiert
    if (is.na(html)) {
      next
    } 
    
    # Counter nullen wenn Seite gefunden -> zahelung startet von vorne
    nodoc_counter <-0
    
    
    # Alle Felder
    divs = html %>%
      html_nodes(xpath = "//div[contains(@class, 'ui-g-')]")
    
    text = divs %>%
      html_text() %>%
      gsub("\\n", "", .) %>%
      str_trim()
    
    # Wenn keine Felder -> next
    if (length(text) == 0) {
      next
    }
    
    names = html %>%
      html_nodes("div.tg-bc-contact") %>%
      html_nodes("div.ui-g")
    
    # VorstösserInnen extrahieren
    nam_list = lapply(names, function(x) {
      temp <- x %>%
        html_nodes("div.ui-g-2") %>%
        html_text() %>%
        gsub("\\n", "", .) %>%
        gsub("Vorstösser/Vorstösserin:", "", .) %>%
        str_trim()
      
      temp[temp == ""] = NA
      return(temp)
      
    })
    
    names_df = do.call(rbind, nam_list) %>%
      data.frame() %>%
      select(-X1) %>%
      setNames(.[1, ]) %>%
      filter(!row_number() %in% c(1))
    
    # Daten extrahieren
    temp_list = list()
    for (index in 3:(length(text) - 1)) {
      first <- text[index]
      second <- text[index + 1]
      
      if (!first %in% variables) {
        next
      }
      if (second %in% variables) {
        temp_list[[first]] = c(NA)
        next
      } else {
        temp_list[[first]] = c(second)
        
      }
      
      
    }
    
    # Datensatz erstellen
    temp_df <- temp_list %>% bind_rows()
    
    # Titel definieren
    title <- temp_df[variables[10]][[1]]
    
    temp_df$Titel <- title
    
    names_df$Titel <- title
    
    # Dokumente und Links extrahieren
    docs <- html %>%
      html_element("dl.ui-datalist-data") %>% 
      html_elements("a")
    
    doc_title <-
      docs %>% 
      html_text()
    
    if (length(doc_title) == 0) {
      doc_df = data.frame(doc_title = NA,
                          doc_link = NA,
                          titel = title)
      document_list[[title]] <- doc_df
      data_list[[title]] <- temp_df
      next
    }
    
    doc_link <- docs %>%
      html_attr("href") %>% 
      paste0("https://grgeko.tg.ch",.)
    
    doc_df = data.frame(doc_title, doc_link, titel = title)
    
    # Einzelne Datensätze zu Liste hinzufügen
    document_list[[title]] <- doc_df
    data_list[[title]] <- temp_df
    names_list[[title]] <- names_df
    Sys.sleep(.2)
  }
  
  # Zusammenhängende Datensätze erstellen
  documents <- document_list %>% bind_rows() %>% janitor::clean_names()
  data_df <- data_list %>% bind_rows() %>% janitor::clean_names()
  names_df <- names_list %>% bind_rows() %>% janitor::clean_names()

  
  data_df$legislatur_nr = legislatur
  data_df <- data_df %>% 
    left_join(join_ga) %>% 
    mutate(registraturnummer  = paste0(legislatur_nr,"/",kennung," ",laufnummer,"/",grg_nummer))
  
  join_reg <- data_df %>% 
    select(registraturnummer,titel)
  
  names_df <- names_df %>% 
    left_join(join_reg) %>% 
    tidyr::separate_rows(nachname,vorname,partei,ort, sep = ", ")
  
  documents <- documents %>% 
    left_join(join_reg)
  
  # saveRDS(names_df,paste0("Y:\\SK\\SKStat\\R\\Prozesse\\pol\\Parlamentsdienste\\vorstoesse_gr\\data\\neu\\vorstoesser_",legislatur,".rds"))
  # saveRDS(data_df,paste0("Y:\\SK\\SKStat\\R\\Prozesse\\pol\\Parlamentsdienste\\vorstoesse_gr\\data\\neu\\geschaefte_",legislatur,".rds"))
  # saveRDS(documents,paste0("Y:\\SK\\SKStat\\R\\Prozesse\\pol\\Parlamentsdienste\\vorstoesse_gr\\data\\neu\\dokumente_",legislatur,".rds"))

  message(paste0("Dokumente gespeichert unter ",getwd()))
  
  # Datensätze in Liste gepackt zurückgeben
  return(list(data_df,names_df,documents))
}
