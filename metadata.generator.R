library(xtable)
library(htmlTable)
library(magrittr)



add.viaf.link <- function(name, viaf){
  if( ( ! is.na(name)) & ( ! is.na(viaf)) ){
    paste0("<a href=https://viaf.org/viaf/", viaf, " target=_blank>", name,"</a>")
  } else{ name }
}

add.orcid.logo <- function(name, orcid){
  if( ( ! is.na(name)) & ( ! is.na(orcid)) ){
    name <- paste0(
      name,
      " <a href=https://orcid.org/", orcid, " target=_blank>",
      "<img src='https://info.orcid.org/wp-content/uploads/2020/12/orcid_16x16.gif' width=13 height=13>",
      "</a>"
      )
  }  
  name
}


metadata.generator <- function(df, site.name, lang){
  
  df <- df[df$site.name == site.name, ]
  df[df == ""] <- NA
  
  df.save <- df

  # add viaf link
  df$excavator1 <- add.viaf.link(df$excavator1, df$excavator1.viaf)
  df$excavator2 <- add.viaf.link(df$excavator2, df$excavator2.viaf)
  df$excavator3 <- add.viaf.link(df$excavator3, df$excavator3.viaf)
  df$data.creator1 <- add.viaf.link(df$data.creator1, df$data.creator1.viaf)
  df$data.creator2 <- add.viaf.link(df$data.creator2, df$data.creator2.viaf)
  df$data.creator3 <- add.viaf.link(df$data.creator3, df$data.creator3.viaf)
  df$data.editor1 <- add.viaf.link(df$data.editor1, df$data.editor1.viaf)  
  
  # add orcid logo
  df$excavator1 <- add.orcid.logo(df$excavator1, df$excavator1.orcid)
  df$excavator2 <- add.orcid.logo(df$excavator2, df$excavator2.orcid)
  df$excavator3 <- add.orcid.logo(df$excavator3, df$excavator3.orcid)
  df$data.creator1 <- add.orcid.logo(df$data.creator1, df$data.creator1.orcid)
  df$data.creator2 <- add.orcid.logo(df$data.creator2, df$data.creator2.orcid)
  df$data.creator3 <- add.orcid.logo(df$data.creator3, df$data.creator3.orcid)
  df$data.editor1 <- add.orcid.logo(df$data.editor1, df$data.editor1.orcid)
  
  # site name ----
  if( ! is.na(df$site.wikidata)){
    df$site.name <- paste0("<a href=https://www.wikidata.org/wiki/",
                            df$site.wikidata,
                            " target=_blank>",
                            df$site.name, "</a>")
  }
  
  # excavators names ----
  df$excavator3 <- gsub("et al.", "<i>et al.</i>",  df$excavator3)
  
  excavators <- df[c("excavator1", "excavator2", "excavator3")]
  excavators <- excavators[ ! is.na(excavators)]
  df$excavators <- paste(excavators, sep = "", collapse = ", ")
  
  # creators names ----
  df$data.creator3 <- gsub("et al.", "<i>et al.</i>",  df$data.creator3)
  
  data.creators <- df[c("data.creator1", "data.creator2", "data.creator3")]
  data.creators <- data.creators[ ! is.na(data.creators)]
  df$data.creator <- paste(data.creators, sep = "", collapse = ", ")

  
  # editors names ----
  df$data.editor <- df$data.editor1
  
  # periods ----
  df$period <- paste0("<a href=", df$period1.uri,
                      " title='Click to go to the PACTOLS concept' target=_blank>",
                         df$period1, "</a>")
  
  if( ! is.na(df$period3)){
    df$period <- paste0(
      df$period, 
      " → <a href=", df$period3.uri,
      " title='Click to go to the PACTOLS concept' target=_blank>", 
      df$period3, "</a>")
  } else if( ! is.na(df$period2)){
    df$period <- paste0(
      df$period, 
      " → <a href=", df$period2.uri,
      " title='Click to go to the PACTOLS concept' target=_blank>", 
      df$period2, "</a>")
  }
  
  # dataset PID ----
  df$data.identifier.url <- "–"
  
  if(! is.na(df$data.identifier.type)){
    if(df$data.identifier.type == "doi"){
      df$data.identifier.url <- paste0("doi: <a href=https://doi.org/",
                                             df$data.identifier.value, " target=_blank>",
                                             df$data.identifier.value, "</a>")
    } else if (df$data.identifier.type == "url"){
      df$data.identifier.url <- paste0("<a href=", df$data.identifier.value,
                                       " target=_blank>website</a>")
    } 
  }
  # workflow PID ----
  df$workflow.identifier.url <- "–"
  
  if(! is.na(df$workflow.identifier.type)){
    if(df$workflow.identifier.type == "doi"){
      df$workflow.identifier.url <- paste0("doi: <a href=https://doi.org/",
                                       df$workflow.identifier.value, " target=_blank>",
                                       df$workflow.identifier.value, "</a>")
    } else if (df$workflow.identifier.type == "url"){
      df$workflow.identifier.url <- paste0("<a href=", df$workflow.identifier.value,
                                       " target=_blank>website</a>")
    } 
  }
  
  #  lat lon ----
  df$latlon <- paste0("<a href=https://www.openstreetmap.org/?mlat=",
                            df$lat, "&mlon=", df$lon,
                            " target=_blank>",
                            df$lat, ", ", df$lon, "</a>")
  # location ----
  if( df$site.location %in% c(NA, "", " ")){
    df$location <- "–"
  } else{
    df$location <- paste0("<a href=",
                        df$site.location.pid,
                        " target=_blank>",
                        df$site.location, "</a>")
  }
  
  # publication date ----
  df$archeoviz.pub.date <- as.Date(as.character(df$archeoviz.pub.date),
                                   format = "%Y%m%d")
  df$archeoviz.pub.year <- format(df$archeoviz.pub.date, format = "%Y")
  df$archeoviz.pub.date <- format(df$archeoviz.pub.date, format = "%d-%m-%Y")

  # update date ----
  df$archeoviz.update.date <- as.Date(as.character(df$archeoviz.update.date),
                                   format = "%Y%m%d")
  # df$archeoviz.update.date <- format(df$archeoviz.update.date, format = "%Y")
  df$archeoviz.update.date <- format(df$archeoviz.update.date, format = "%d-%m-%Y")
  
  
  # Switch linguistique   ----
  if(lang == "en"){
    val.names <- c("Site name", "Site coordinates", "Site altitude (m)", "Site location", "Excavator", "Excavation date", "Data creator", "Data publication date", "Period",  "Nr of objects", "Nr of refitting relationships", "Nr of refitting objects", "Nr of sites", "Nr of dates", "Dataset", "License", "Data editor", "Reprocessing code", "archeoViz publication date", "archeoViz update date")
  } else if(lang == "fr"){
    val.names <- c("Nom du site", "Coordonnées", "Altitude (m)", "Commune", "Fouilleur", "Date des fouilles", "Créateur des données", "Date de publication des données", "Période chronologique", "N objets", "N remontages", "N objets remontables", "N sites", "N dates", "Jeu de données", "Licence", "Éditeur des données", "Code d'édition", "Date de publication archeoViz", "Date de mise à jour archeoViz")
  } 
  
  # Sélection des variables :
  val <- unlist(df[c("site.name", "latlon", "altitude", "location", "excavators", "excavation.date",  "data.creator", "data.publication", "period", "n.objects", "n.refits", "n.objects.in.refitting.set", "n.sites", "n.dates", "data.identifier.url", "data.license", "data.editor", "workflow.identifier.url",  "archeoviz.pub.date", "archeoviz.update.date")])
  
  if(df$excavators == ""){
    val.names <- val.names[- which(names(val) %in% c("excavators", "excavation.date"))  ]
    val <- val[- which(names(val) %in% c("excavators", "excavation.date"))  ]
  }
  
  if(df$n.objects > 0){
    val.names <- val.names[- which(names(val) %in% c("n.sites", "n.dates"))  ]
    val <- val[- which(names(val) %in% c("n.sites", "n.dates"))  ]
  }
  
  if(df$n.sites > 0){
    val.names <- val.names[- which(names(val) %in% c("n.objects", "n.refits", "n.objects.in.refitting.set", "n.dates"))  ]
    val <- val[- which(names(val) %in% c("n.objects", "n.refits", "n.objects.in.refitting.set", "n.dates"))  ]
  }
  
  if(df$n.dates > 0){
    val.names <- val.names[- which(names(val) %in% c("n.objects", "n.refits", "n.objects.in.refitting.set", "n.sites"))  ]
    val <- val[- which(names(val) %in% c("n.objects", "n.refits", "n.objects.in.refitting.set", "n.sites"))  ]
  }
  
  if(is.na(df$archeoviz.update.date)){
    val.names <- val.names[- which(names(val) == "archeoviz.update.date")  ]
    val <- val[- which(names(val) == "archeoviz.update.date")  ]
  }
  
  # add html tags
  val.names <- paste0("<font face=courier>", val.names, "</font>")
  
  df2 <- data.frame("   " = val.names, " " = val, check.names = F)
  
  df.html <- df2 %>% 
    addHtmlTableStyle(align = "ll", align.header="ll", 
                      css.cell = "padding-top: 0em; padding-bottom: 0em; padding-left: 1em; padding-right: 1em;",
                      css.header = "font-family: courier",
                      col.rgroup = c("none", "#F7F7F7")) %>% 
    htmlTable(rnames=F, cnames=F) 
  
  
  citation.title <- c("Cite this application")
  if(lang == "fr"){ citation.title <- c("Citer cette application") }
  
  # Citation ----
  
  if(is.na(df$data.editor) | df$data.editor == "–"){
    df$data.editor <- ""
  } else{
    df$data.editor <- paste0(", ", df$data.editor)
  }
  
  df$period.citation <- df$period1
  if( ! is.na(df$period3)){
    df$period.citation <- paste0(df$period1, "-", df$period3)
  } else if( ! is.na(df$period2)){
    df$period.citation <- paste0(df$period1, "-", df$period2)
  }
  
  if(df.save$data.editor1 %in% c(df.save$data.creator1, df.save$data.creator2, df.save$data.creator3)){
    df$data.editor <- ""
  }
  
  citation <- paste0("<h2>", citation.title, "</h2>",
                    "archeoViz platform maintainers, ",
                    df$data.creator,
                    df$data.editor, 
                    ". ",
                    "<b>", df$archeoviz.pub.year, "</b>. ",
                    "<i>Online data visualisation of: ",
                    site.name, " (",
                    df$period.citation, 
                    ") using the archeoViz web application</i>, ",
                    "<a href=",
                    df$archeoviz.url,
                    " target=_blank>", 
                    df$archeoviz.url, "</a>, <",
                    "<a href=https://hal.science/",
                    df$hal,
                    " target=_blank>", 
                    df$hal, "</a>>,
                    hdl: <a href=https://hdl.handle.net/",
                    df$hdl,
                    " target=_blank>",
                    df$hdl, "</a>.")
  
  paste0(df.html,
      citation)
}


# metadata2$void <- " "
# metadata2 <- metadata2[, c(1,3,2)]
# colnames(metadata2) <- c("Metadata", "", "Value")

# metadata2 <- data.frame("" = rownames(metadata2), " x" = metadata2)

# metadata2.html <-  print.xtable(xtable(metadata2, align=c("ll")),
#                                 sanitize.text.function = function(x){x},
#                                 type = "html", include.colnames=T,
#                                 html.table.attributes = "border=0")
