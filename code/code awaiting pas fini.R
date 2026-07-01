
#Fichier 
chemin_local <- "C:/Users/Stagiaire_InesDris/Documents/Projet 2/transfer_13050661_files_4874e782/awaiting_assessment.csv"


df <- read.csv(chemin_local, stringsAsFactors = FALSE, sep = ",")


summary(df)
str(df)
dim(df)
head(df)



df_propre <- subset(df, doi != "" & !is.na(doi))
echantillon_test <- head(df_propre, 50)
dois_test <- echantillon_test$doi

ma_cle_api <- "4C45E82714C2440E8BE9696A9A4A1B4E"

reponse_auth <- POST("https://app.dimensions.ai/api/auth.json", body = list(key = ma_cle_api), encode = "json")
mon_token <- content(reponse_auth)$token

liste_resultats_dimensions <- list()
taille_lot <- 10
lots <- split(dois_test, ceiling(seq_along(dois_test) / taille_lot))

for (i in 1:length(lots)) {
  lot_actuel <- lots[[i]]
  dois_formates <- paste0('"', lot_actuel, '"', collapse = ", ")
  requete_dsl <- paste0(
    "search publications where doi in [", dois_formates, "] ",
    "return publications[doi + issn + journal + open_access + publisher + pmcid + pmid + clinical_trial_ids + supporting_grant_ids + type + year + arxiv_id + date + funders]"
  )
  
  reponse_api <- POST(
    url = "https://app.dimensions.ai/api/dsl/v2",
    add_headers(Authorization = paste("JWT", mon_token)),
    body = requete_dsl
  )
  
  res_dimensions <- fromJSON(content(reponse_api, "text"))$publications
  if (is.null(res_dimensions) || nrow(res_dimensions) == 0) next
  
  doi_val       <- if("doi" %in% names(res_dimensions)) res_dimensions$doi else NA
  publisher_val <- if("publisher" %in% names(res_dimensions)) res_dimensions$publisher else NA
  pmcid_val     <- if("pmcid" %in% names(res_dimensions)) res_dimensions$pmcid else NA
  pmid_val      <- if("pmid" %in% names(res_dimensions)) res_dimensions$pmid else NA
  type_val      <- if("type" %in% names(res_dimensions)) res_dimensions$type else NA
  year_val      <- if("year" %in% names(res_dimensions)) res_dimensions$year else NA
  date_val      <- if("date" %in% names(res_dimensions)) res_dimensions$date else NA
  journal_val   <- if("journal" %in% names(res_dimensions)) res_dimensions$journal$title else NA
  issn_val      <- if("issn" %in% names(res_dimensions)) sapply(res_dimensions$issn, paste, collapse = "; ") else NA
  oa_val        <- if("open_access" %in% names(res_dimensions)) sapply(res_dimensions$open_access, paste, collapse = "; ") else NA
  grants_val    <- if("supporting_grant_ids" %in% names(res_dimensions)) sapply(res_dimensions$supporting_grant_ids, paste, collapse = "; ") else NA
  trials_val    <- if("clinical_trial_ids" %in% names(res_dimensions)) sapply(res_dimensions$clinical_trial_ids, paste, collapse = "; ") else NA
  funders_val   <- if("funders" %in% names(res_dimensions)) sapply(res_dimensions$funders, function(f) if(is.data.frame(f)) paste(f$name, collapse = "; ") else NA) else NA
  
  df_lot <- data.frame(
    doi                          = doi_val,
    issn                         = issn_val,
    journal_title                = journal_val,
    open_access                  = oa_val,
    publisher                    = publisher_val,
    dimensions_pmcid             = pmcid_val,
    dimensions_pmid              = pmid_val,
    dimensions_clinical_trial_id = trials_val,
    supporting_grant_ids         = grants_val,
    dimensions_type              = type_val,
    dimensions_year              = year_val,
    dimensions_date              = date_val,
    dimensions_funders           = funders_val,
    stringsAsFactors             = FALSE
  )
  
  liste_resultats_dimensions[[i]] <- df_lot
  Sys.sleep(1)
}

echantillon_dimensions <- bind_rows(liste_resultats_dimensions)

get_crossref_funders <- function(doi) {
  if (is.na(doi) || doi == "") return(NA_character_)
  url <- paste0("https://api.crossref.org/works/", doi, "?mailto=inesdris@icloud.com")
  reponse <- GET(url, timeout(3))
  if (status_code(reponse) != 200) return(NA_character_)
  
  json <- fromJSON(content(reponse, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  financeurs <- json$message$funder
  if (is.null(financeurs) || length(financeurs) == 0) return(NA_character_)
  
  noms <- sapply(financeurs, function(f) f$name)
  return(paste(noms, collapse = "; "))
}

get_europe_pmc_funders <- function(pmcid) {
  if (is.na(pmcid) || pmcid == "") return(NA_character_)
  url <- paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=", pmcid, "&resultType=core&format=json")
  reponse <- GET(url, timeout(3))
  if (status_code(reponse) != 200) return(NA_character_)
  
  json <- fromJSON(content(reponse, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  if (json$hitCount == 0) return(NA_character_)
  
  subventions <- json$resultList$result[[1]]$grantsList$grant
  if (is.null(subventions) || length(subventions) == 0) return(NA_character_)
  
  agences <- sapply(subventions, function(g) g$agency)
  return(paste(agences, collapse = "; "))
}

echantillon_funding <- echantillon_dimensions
echantillon_funding$crossref_funders   <- NA_character_
echantillon_funding$europe_pmc_funders <- NA_character_
echantillon_funding$pubpeer_url        <- NA_character_

for (i in 1:nrow(echantillon_funding)) {
  doi_actuel   <- echantillon_funding$doi[i]
  pmcid_actuel <- echantillon_funding$dimensions_pmcid[i]
  
  echantillon_funding$crossref_funders[i]   <- get_crossref_funders(doi_actuel)
  echantillon_funding$europe_pmc_funders[i] <- get_europe_pmc_funders(pmcid_actuel)
  echantillon_funding$pubpeer_url[i]        <- paste0("https://pubpeer.com/publications/", doi_actuel)
  
  Sys.sleep(0.2)
}

dossier_sjr <- "C:/Users/Stagiaire_InesDris/Documents/Projet 2/sjr fichier"
fichiers_csv <- list.files(path = dossier_sjr, pattern = "\\.csv$", full.names = TRUE)
df_sjr_global <- data.frame()

for (fichier in fichiers_csv) {
  donnees_annee <- read.csv(fichier, sep = ";", stringsAsFactors = FALSE)
  annee_extrait <- as.numeric(gsub("[^0-9]", "", basename(fichier)))
  donnees_annee$year_sjr <- annee_extrait
  
  donnees_annee_cles <- donnees_annee %>% 
    select(issn_sjr = Issn, sjr_score = SJR, year_sjr)
  
  df_sjr_global <- bind_rows(df_sjr_global, donnees_annee_cles)
}

df_sjr_propre <- df_sjr_global %>%
  separate_rows(issn_sjr, sep = ", ") %>%
  mutate(issn_clean = gsub("[^0-9X]", "", issn_sjr))

df_echantillon_pret <- echantillon_funding %>%
  mutate(
    issn_premier = sub(";.*", "", issn),
    issn_clean   = gsub("[^0-9X]", "", issn_premier)
  )

echantillon_final_avec_sjr <- df_echantillon_pret %>%
  left_join(df_sjr_propre, by = "issn_clean", relationship = "many-to-many") %>%
  mutate(ecart_annee = abs(dimensions_year - year_sjr)) %>%
  group_by(doi) %>%
  arrange(ecart_annee, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(-issn_premier, -issn_clean, -issn_sjr, -ecart_annee, -year_sjr)

dois_totaux <- unique(df_propre$doi)
taille_lot_total <- 100
lots_totaux <- split(dois_totaux, ceiling(seq_along(dois_totaux) / taille_lot_total))
liste_resultats_dimensions_total <- list()

for (i in 1:length(lots_totaux)) {
  lot_actuel <- lots_totaux[[i]]
  dois_formates <- paste0('"', lot_actuel, '"', collapse = ", ")
  requete_dsl <- paste0(
    "search publications where doi in [", dois_formates, "] ",
    "return publications[doi + issn + journal + open_access + publisher + pmcid + pmid + clinical_trial_ids + supporting_grant_ids + type + year + arxiv_id + date + funders]"
  )
  
  reponse_api <- POST(
    url = "https://app.dimensions.ai/api/dsl/v2",
    add_headers(Authorization = paste("JWT", mon_token)),
    body = requete_dsl
  )
  
  if (status_code(reponse_api) == 401) {
    reponse_auth <- POST("https://app.dimensions.ai/api/auth.json", body = list(key = ma_cle_api), encode = "json")
    mon_token <- content(reponse_auth)$token
    reponse_api <- POST(
      url = "https://app.dimensions.ai/api/dsl/v2",
      add_headers(Authorization = paste("JWT", mon_token)),
      body = requete_dsl
    )
  }
  
  res_dimensions <- fromJSON(content(reponse_api, "text"))$publications
  if (is.null(res_dimensions) || nrow(res_dimensions) == 0) next
  
  doi_val       <- if("doi" %in% names(res_dimensions)) res_dimensions$doi else NA
  publisher_val <- if("publisher" %in% names(res_dimensions)) res_dimensions$publisher else NA
  pmcid_val     <- if("pmcid" %in% names(res_dimensions)) res_dimensions$pmcid else NA
  pmid_val      <- if("pmid" %in% names(res_dimensions)) res_dimensions$pmid else NA
  type_val      <- if("type" %in% names(res_dimensions)) res_dimensions$type else NA
  year_val      <- if("year" %in% names(res_dimensions)) res_dimensions$year else NA
  date_val      <- if("date" %in% names(res_dimensions)) res_dimensions$date else NA
  journal_val   <- if("journal" %in% names(res_dimensions)) res_dimensions$journal$title else NA
  issn_val      <- if("issn" %in% names(res_dimensions)) sapply(res_dimensions$issn, paste, collapse = "; ") else NA
  oa_val        <- if("open_access" %in% names(res_dimensions)) sapply(res_dimensions$open_access, paste, collapse = "; ") else NA
  grants_val    <- if("supporting_grant_ids" %in% names(res_dimensions)) sapply(res_dimensions$supporting_grant_ids, paste, collapse = "; ") else NA
  trials_val    <- if("clinical_trial_ids" %in% names(res_dimensions)) sapply(res_dimensions$clinical_trial_ids, paste, collapse = "; ") else NA
  funders_val   <- if("funders" %in% names(res_dimensions)) sapply(res_dimensions$funders, function(f) if(is.data.frame(f)) paste(f$name, collapse = "; ") else NA) else NA
  
  df_lot <- data.frame(
    doi                          = doi_val,
    issn                         = issn_val,
    journal_title                = journal_val,
    open_access                  = oa_val,
    publisher                    = publisher_val,
    dimensions_pmcid             = pmcid_val,
    dimensions_pmid              = pmid_val,
    dimensions_clinical_trial_id = trials_val,
    supporting_grant_ids         = grants_val,
    dimensions_type              = type_val,
    dimensions_year              = year_val,
    dimensions_date              = date_val,
    dimensions_funders           = funders_val,
    stringsAsFactors             = FALSE
  )
  
  liste_resultats_dimensions_total[[i]] <- df_lot
  Sys.sleep(1)
}

df_dimensions_total <- bind_rows(liste_resultats_dimensions_total)

df_funding_total <- df_dimensions_total
df_funding_total$crossref_funders   <- NA_character_
df_funding_total$europe_pmc_funders <- NA_character_
df_funding_total$pubpeer_url        <- NA_character_

for (i in 1:nrow(df_funding_total)) {
  doi_actuel   <- df_funding_total$doi[i]
  pmcid_actuel <- df_funding_total$dimensions_pmcid[i]
  
  df_funding_total$crossref_funders[i]   <- get_crossref_funders(doi_actuel)
  df_funding_total$europe_pmc_funders[i] <- get_europe_pmc_funders(pmcid_actuel)
  df_funding_total$pubpeer_url[i]        <- paste0("https://pubpeer.com/publications/", doi_actuel)
  
  Sys.sleep(0.2)
}

df_total_pret <- df_funding_total %>%
  mutate(
    issn_premier = sub(";.*", "", issn),
    issn_clean   = gsub("[^0-9X]", "", issn_premier)
  )

df_total_avec_sjr <- df_total_pret %>%
  left_join(df_sjr_propre, by = "issn_clean", relationship = "many-to-many") %>%
  mutate(ecart_annee = abs(dimensions_year - year_sjr)) %>%
  group_by(doi) %>%
  arrange(ecart_annee, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(-issn_premier, -issn_clean, -issn_sjr, -ecart_annee, -year_sjr)

df_final_complet <- df %>%
  left_join(df_total_avec_sjr, by = "doi")

write.csv(df_final_complet, "C:/Users/Stagiaire_InesDris/Documents/Projet 2/transfer_13050661_files_4874e782/awaiting_assessment_ENRICHI.csv", row.names = FALSE)