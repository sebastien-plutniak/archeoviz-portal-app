library(dplyr, include.only  = c("mutate", "group_by", "summarise", "arrange", "n"), quietly = T, verbose = F, warn.conflicts = F)
library(ggplot2, quietly = T, verbose = F, warn.conflicts = F)
library(htmlwidgets, include.only  = "saveWidget", quietly = T, verbose = F, warn.conflicts = F)
library(plotly, quietly = T, verbose = F, warn.conflicts = F)

# : format log data  ----
log.df <- read.csv( "../archeoviz-log.csv")
log.df$date <- as.Date(log.df$date)

# reformat:
log.df <- dplyr::group_by(log.df, instance, date) %>% 
  dplyr::summarise("n" = dplyr::n(), .groups = "keep") %>%
  dplyr::arrange(instance, date)  %>% 
  dplyr::mutate("visits" = cumsum(n))

# tag instances
log.df$type <- "specific"
log.df[log.df$instance %in% c("fr", "en", "it", "ro", "pt", "de", "es", "home"), ]$type <- "generic"


specific.stats <- log.df[log.df$type == "specific", ]
specific.plot <-
  ggplot(specific.stats,
         aes(x = date, y = visits, color = instance, group = instance)) +
  theme_light() +
  geom_path(show.legend = F) +
  # geom_text(data = specific.max,
  #           aes(y = max, label = site, color = site, group = site),
  #           x =  max(site.list$date) + .5,
  #           size = 4, show.legend=F, hjust = "left", check_overlap=F) +
  scale_x_date("Date", date_labels = "%Y-%m-%d",
               # limits = c(min(log.df$date), max(log.df$date) + 2)
  ) +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1, size=9)) +
  ylab("Cumulated number of visits")  +
  annotate("text", x = min(specific.stats$date) + .2,
           y = max(specific.stats$visits),
           size = 2.5, 
           label = paste("Total:", sum(specific.stats$n), "visits") )

specific.plot <- plotly::ggplotly(specific.plot, tooltip = c("visits", "date", "instance")) %>%
  plotly::config(displaylogo = FALSE,
                 modeBarButtonsToRemove = list(
                   'sendDataToCloud',
                   'select2d',
                   'resetScale2d',
                   'lasso2d',
                   'hoverClosestCartesian',
                   'hoverCompareCartesian'))


htmlwidgets::saveWidget(specific.plot, "home/www/archeoviz-stat-specific.html")



# : generic instances ----
generic.stats <- log.df[log.df$type == "generic",]

generic.plot <- ggplot(generic.stats,
                       aes(x = date, y = visits, color = instance)) +
  theme_light() +
  geom_path(show.legend = F) +
  scale_x_date("Date", date_labels = "%Y-%m-%d",
  ) +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1, size=9)) +
  ylab("Cumulated number of visits") +
  annotate("text", x = min(generic.stats$date) + .2,
           y = max(generic.stats$visits),
           size = 2.5,
           label = paste("Total:", sum(generic.stats$n), "visits") )

generic.plot <-   plotly::ggplotly(generic.plot, tooltip = c("visits", "instance", "date")) %>%
  plotly::config(displaylogo = FALSE,
                 modeBarButtonsToRemove = list(
                   'sendDataToCloud',
                   'select2d',
                   'resetScale2d',
                   'lasso2d',
                   'hoverClosestCartesian',
                   'hoverCompareCartesian'))

htmlwidgets::saveWidget(specific.plot, "home/www/archeoviz-stat-generic.html")


message("\n>>> Exec. log-analysis.R: log statistics figures generated!\n")
