

# find and load the tables
csv.list <-  system("find ../ -maxdepth 2 -iname '*.csv'", intern = TRUE,)
names(csv.list) <- gsub("../(.*?)/.*", "\\1", csv.list, perl = T)

tab.list <- lapply(csv.list, read.csv)

# comma.sep.idx <- sapply(res, ncol) == 1
# semicolon.sep.idx <- sapply(res, ncol) > 1
# 
# comma.sep.tabs <- lapply(csv.list[comma.sep.idx], read.csv)
# semicolon.sep.tabs <- lapply(csv.list[semicolon.sep.idx], read.csv2)
# 
# tab.list <- append(comma.sep.tabs, semicolon.sep.tabs)
# sapply(tab.list, ncol)

tab.list <- tab.list[ ! (sapply(tab.list, ncol)  < 4) ]
tab.list <- tab.list[ order(names(tab.list)) ]

# OPTION 1: extract all 'object_' values
# idx <- is.na(sapply(tab.list, function(x) which(names(x) == "object_type") ) > 0)
# tab.list2 <- tab.list[  ! idx  ]
# type.values <- sapply(tab.list2, function(x) unlist(x[ ,   grep("object_", names(x))  ]  ) )  

# OPTION 2:  extract 'object_type' values only
type.values <- sapply(tab.list, function(x)  tolower(x$object_type)  )
type.values <- type.values[ ! sapply(type.values, length) == 0 ]


# reformat
type.values <- lapply(1:length(type.values), function(x)
  data.frame("site" = names(type.values)[x], 
             "type" = type.values[[x]])
        )

type.values <- do.call("rbind", type.values)

# explore
sort(table(type.values$type))
tail(names(sort(table(type.values$type))), 200)

a <- names(sort(table(type.values$type)))
a[grep("coqui", a)]

#end explore

# recodages

type.values$type2 <- NA

type.values[ type.values$type %in% c("stone tool", "lithique", "éclat lamellaire", "retouched flake", "lamelle", "éclat laminaire", "lithique", "biface", "burin sur troncature", "grattoir", "troncature", "proxflake", "compflake", "scrapers", "casson", "lithic", "esquille", "éclat", "éclat utilisé", "lithique taillé", "quartz (taillé)"), ]$type2 <- "lithic industry"

type.values[ type.values$type %in% c("nucléus", "core platforms", "tabular core", "core", "corefrag", "livre de beurre"), ]$type2 <- "lithic core"

type.values[ type.values$type %in% c("céramique", "poterie", "ceramic", "tesson", "pot lid", "céramique non tournée", "céramique tournée", "ceramique"),]$type2 <- "pottery"

type.values[ type.values$type %in% c("industrie osseuse"),]$type2 <- "bone industry"

type.values[ type.values$type %in% c("métal", "weapons"),]$type2 <- "metal objects"

type.values[ type.values$type %in% c("marineshell", "coquille", "coquilles", "malaco", "prélèvement malaco"),]$type2 <- "shell"

type.values[ type.values$type %in% c("big eggshell", "small eggshell", "eggshell"),]$type2 <- "eggshell"

type.values[ type.values$type %in% c("homo", "human", "human remains", "reste humain", "modern"),]$type2 <- "human bone"

type.values[ type.values$type %in% c("coprolites"),]$type2 <- "coproliths"

type.values[ type.values$type %in% c("grinding stone", "macro-outillage"),]$type2 <- "millstone"

type.values[ type.values$type %in% c("faune", "elephant cloud", "cervus elaphus", "rupicapra rupicapra", "ursus sp.", "ursus spelaeus", "bovinae", "tooth",  "mammal", "faunal", "fauna", "bone", "os"),]$type2 <- "animal bone"

type.values[ type.values$type %in% c("micromammal", "small vertebrates"),]$type2 <- "microfauna"

type.values[ type.values$type %in% c("antler", "bois de cerf", "tine", "crown tine", "brow tine"),]$type2 <- "antler"



sort(table(type.values$type2), decreasing = T)



# tab <- unique(type.values[, - 2])
# tab <- tab[complete.cases(tab), ]

tab <- table(type.values[, - 2])
tab <- as.data.frame.matrix(tab)

for(x in 1:ncol(tab)) {
   tab[, x][tab[, x] > 0]  <- colnames(tab)[x]
}
tab[tab == "0"] <- ""
tab <- t(apply(tab, 1, function(x) x[order(x, decreasing = T)]  ))
tab <- as.data.frame(tab)
tab <- tab[ order(rownames(tab)), ]
write.csv(tab, "types.csv", row.names = T)
