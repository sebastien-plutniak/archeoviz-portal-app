library(dplyr, include.only  = c("mutate", "group_by"),
        quietly = T, verbose = F, warn.conflicts = F)

# scan the log folder:
site.list <- list.files("../log/") # NB: le chemin est adapté aux nécessités du déploiement

site.list <- data.frame("site" = gsub("(.*)-archeoviz.*", "\\1",
                                      site.list, perl=T),
                        "date" = gsub(".*?-.*?-([0-9]{8}).*", "\\1",
                                      site.list, perl=T))
site.list <- table(site.list)
site.list <- data.frame(site.list)
site.list <- site.list[site.list$Freq != 0, ]

site.list$date <- as.Date(site.list$date, format = "%Y%m%d")

site.list <- site.list[order(site.list$date), ]

# tag instance types:

site.list$type <- "specific"
site.list[site.list$site %in% c("fr", "de", "it", "ro", "pt", "es", "en"), ]$type <- "generic"
site.list[site.list$site == "home",]$type <- "portal"
levels(site.list$site)[ levels(site.list$site) == "home"] <- "portal"

# reformat:
site.list <-  dplyr::group_by(site.list, site)  
site.list <-  dplyr::mutate(site.list, "visits" = cumsum(Freq))

# export
write.csv(site.list, "home/log-stats.csv", row.names = F) # NB: le chemin est adapté aux nécessités du déploiement
message("\n>>> Exec. log-analysis.R: log statistics updated!\n")
