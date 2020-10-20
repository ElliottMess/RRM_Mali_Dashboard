


frenchieStrClean <- function(string){
  string <- gsub("[()]| ","_", string)
  string <- gsub("__", "_", string)
  string <- gsub("[0-9]|[=:/.,?']","", string)
  string <- gsub("_$","",string)
  string <- str_squish(string)
  return(string)
}


loadAlerts <- function(dataPath, pcodes_adm2){

# STEP 2 - LOAD ALERTS INTO A DATAFRAME

# 2.1 Raw Data
 
AlertsRaw<-read_excel(dataPath,sheet="VEILLE",col_names = TRUE,skip=6, na=c("N/A","NA"))

pcodes_adm2 <- pcodes_adm2


# 2.2 Clean Dates

namesAlert <- names(AlertsRaw)
coltypes <- namesAlert
coltypes[startsWith(namesAlert, "Date")] <- "date"
coltypes[!startsWith(namesAlert, "Date")] <- "guess"

AlertsRaw<-read_excel(dataPath,sheet="VEILLE",col_names = TRUE,col_types=coltypes, skip=6, na=c("N/A","NA", "RAS"))%>%
  mutate_at(vars(starts_with("Date")), str_replace, "ç", "9")%>%
  mutate_at(vars(starts_with("Date")), str_replace, ":", "/")%>%
  mutate_at(vars(starts_with("Date")), as_date)

# 2.3 Clean AlertsRaw
#Rename headers

HeadersRaw <- colnames(AlertsRaw)
HeadersRaw <- iconv(HeadersRaw, from = 'UTF-8', to = 'ASCII//TRANSLIT')
HeadersRaw <- unlist(lapply(list(HeadersRaw), frenchieStrClean))

colnames(AlertsRaw)<-HeadersRaw


#Rename Circles to match it with the PCodes, according to OCHA's description of the country

AlertsRaw<-AlertsRaw%>%
  dplyr::mutate(Cercle_Accueil = str_replace(Cercle_Accueil, "ABEIBARA", "Abeibara"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "ANDERAMBOUKANE_CER", "Anderamboukane"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "ANSONGO", "Ansongo"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "BAFOULABE", "Bafoulabe"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "BAMAKO", "Bamako"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "BANAMBA", "Banamba"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "BANDIAGARA", "Bandiagara"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "BANKASS", "Bankass"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "BAROUELI", "Baroueli"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "BLA", "Bla"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "BOUGOUNI", "Bougouni"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "BOUREM", "Bourem"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "DIEMA", "Diema"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "DIOLILA", "Dioila"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "DIRE", "Dire"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "DJENNE", "Djenne"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "DOUENTZA", "Douentza"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "GAO_CER", "Gao"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "GOUNDAM", "Goundam"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "GOURMA_RHAROUS", "Gourma-Rharous"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "INEKAR_CER", "Inekar"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "KADIOLO", "Kadiolo"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "KANGABA", "Kangaba"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "KATI", "Kati"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "KAYES", "Kayes"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "KENIEBA", "Kenieba"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "KIDAL_CER", "Kidal"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "KITA", "Kita"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "KOLOKANI", "Kolokani"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "KOLONDIEBA", "Kolondieba"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "KORO", "Koro"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "KOULIKORO", "Koulikoro"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "KOUTIALA", "Koutiala"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "MACINA", "Macina"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "MENAKA_CER", "Menaka"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "MOPTI_CER", "Mopti"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "NARA", "Nara"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "NIAFUNKE", "Niafunke"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "NIONO", "Niono"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "NIORO", "Nioro"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "SAN", "San"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "SEGOU", "Segou"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "SIKASSO", "Sikasso"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "TENENKOU", "Tenenkou"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "TESSALIT", "Tessalit"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "TINDERMENE_CER", "Tidermene"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "TIN-ESSAKO", "Tin-Essako"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "TAOUDENI_CER", "Tombouctou"), #Taoudeni is part of Tombouctou in OCHA maps
    Cercle_Accueil = str_replace(Cercle_Accueil, "TOMBOUCTOU_CER", "Tombouctou"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "TOMINIAN", "Tominian"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "YANFOLILA", "	Yanfolila"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "YELIMANE", "Yelimane"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "YOROSSO", "Yorosso"),
    Cercle_Accueil = str_replace(Cercle_Accueil, "YOUWAROU", "Youwarou"))%>%
  left_join(pcodes_adm2, by= c("Cercle_Accueil"="Admin2_Nam"))%>%
  mutate(Pcode_Ad_1 = str_sub(Pcode_Ad_2, end = -3))

Alerts<-AlertsRaw%>%
  dplyr::filter(!is.na(Pcode_Ad_2))

return(Alerts)
}


