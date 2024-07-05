try(library(ggnetwork, quietly=TRUE, verbose = FALSE), silent = T)

chrono.tab <- read.csv("home/chronology.csv")

#  : prepare graph ----
chronoGraph <- igraph::graph_from_data_frame(chrono.tab, directed = T)
igraph::V(chronoGraph)$id <- 1:igraph::gorder(chronoGraph)
igraph::V(chronoGraph)$rank <- igraph::distances(chronoGraph,
                                                 v = igraph::V(chronoGraph),
                                                 to = igraph::V(chronoGraph)[igraph::V(chronoGraph)$name == "quaternary"],
                                                 mode = "out")
igraph::V(chronoGraph)$rank <- abs(igraph::V(chronoGraph)$rank - max(igraph::V(chronoGraph)$rank))

# : add alternative period names ----
igraph::V(chronoGraph)$name2 <- ""

igraph::V(chronoGraph)[igraph::V(chronoGraph)$name == "paleolithic"]$name2 <- "palaeolithic"
igraph::V(chronoGraph)[igraph::V(chronoGraph)$name == "early stone age"]$name2 <- "ESA"
igraph::V(chronoGraph)[igraph::V(chronoGraph)$name == "later stone age"]$name2 <- "LSA"
igraph::V(chronoGraph)[igraph::V(chronoGraph)$name == "middle stone age"]$name2 <- "MSA"
igraph::V(chronoGraph)[igraph::V(chronoGraph)$name == "middle paleolithic"]$name2 <- "middle palaeolithic"
igraph::V(chronoGraph)[igraph::V(chronoGraph)$name == "lower paleolithic"]$name2 <- "lower palaeolithic"
igraph::V(chronoGraph)[igraph::V(chronoGraph)$name == "upper paleolithic"]$name2 <- "upper palaeolithic"
igraph::V(chronoGraph)[igraph::V(chronoGraph)$name == "paleoindian"]$name2 <- "paleo-indian"

igraph::V(chronoGraph)[igraph::V(chronoGraph)$name == "châtelperronian"]$name2 <- "chatelperronian"


# : prepare table ----
refTable <- data.frame("id" = igraph::V(chronoGraph)$id,
                       "name" = igraph::V(chronoGraph)$name,
                       "name" = igraph::V(chronoGraph)$name2,
                       "rank" = igraph::V(chronoGraph)$rank)
refTable <- refTable[order(refTable$rank), ]

# : extract chrono chains and clean them
retrieve.chrono.path <- function(chain, ref.table, chrono.graph){
  chain <- tolower(chain)
  
  idx <- c(grep(paste0("^", chain, "$"),   ref.table$name),
           grep(paste0("^", chain, "$"),   ref.table$name2))
  match.node.id <- ref.table[idx, ]$id
  if(length(match.node.id) == 0) return()
  
  result.path <- igraph::all_shortest_paths(chrono.graph,
                                            from = igraph::V(chrono.graph)[match.node.id],
                                            to = igraph::V(chrono.graph)[igraph::V(chrono.graph)$name == "quaternary"],
                                            mode = "out")
  res <- c(igraph::V(chronoGraph)[unlist(result.path$res)]$name,
           igraph::V(chronoGraph)[unlist(result.path$res)]$name2)
  
  unique(res)
}

 
metadata <- read.csv("home/metadata.csv",
                     colClasses=c(
                       rep("double", 4),
                       "character",
                       "double",
                       rep("character", 2),
                       rep("double", 5),
                       rep("character", 51)),
                     dec = ".")


p1.res <- sapply(metadata$period1, retrieve.chrono.path, refTable, chronoGraph)
p2.res <- sapply(metadata$period2, retrieve.chrono.path, refTable, chronoGraph)
p3.res <- sapply(metadata$period3, retrieve.chrono.path, refTable, chronoGraph)

metadata$period.keywords <- sapply(1:length(p1.res), function(x){
  res <- c(unlist(p1.res[x]), unlist(p2.res[x]), unlist(p3.res[x]))
  paste(unique(res), collapse = " ")
})

# export metadata with period.keywords
write.csv(metadata, "home/metadata-deployed.csv", row.names = FALSE)



# View(sites[, c("period1","period2","period3", "period.keywords")])

# : export graph plot ----

chronoGraph.plot <- ggnetwork(chronoGraph, 
                              layout = igraph::as_tree(
                                root = igraph::V(chronoGraph)[igraph::V(chronoGraph)$name == "quaternary"],
                                mode = "in",
                                flip.y = T) )
chronoGraph.plot <- ggplot(chronoGraph.plot,
                           aes(x, y, xend = xend, yend = yend)) +
  geom_edges(colour = "steelblue") +
  geom_nodelabel(aes(label = name), size = 3, 
                 fill=rgb(1,1,1,.5), color="grey10",
                 label.size = 0.01,) +
  coord_flip() +
  scale_y_reverse() +
  theme_blank()

ggsave("home/www/chronoGraph.jpg", chronoGraph.plot, width=12, height=7)

message("\n>>> Exec. chrono-graph.R: periods added to metadata.csv and Chrono graph generated!\n")



# Check whether term are missing ----
period.terms <- unique(unlist(metadata[, c("period1", "period2", "period3")]))
period.terms <- period.terms[period.terms != ""]
missing.terms <- period.terms[ ! tolower(period.terms) %in% igraph::V(chronoGraph)$name]

if(length(missing.terms) > 0){
  message(paste0("\n    Warning: the following terms are missing in the graph: ",
               paste0(missing.terms, collapse = ", "), "\n"))
}

