library(shinythemes)
library(leaflet)
library(htmltools)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)


css <- '
.tooltip {
  pointer-events: none;
}
.tooltip > .tooltip-inner {
  pointer-events: none;
  background-color: #FFFFFF;
  color: #000000;
  border: 1px solid black;
  padding: 5px;
  font-size: 12px;
  text-align: left;
  max-width: 300px;
  content: <b>aa</b>;
}
.tooltip > .arrow::before {
  border-right-color: #73AD21;
}
'

js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"


metadata <- read.csv("metadata-deployed.csv", 
                     colClasses=c(
                       rep("double", 8),
                       "character",
                       "double",
                       rep("character", 2),
                       rep("double", 5),
                       rep("character", 53)),
                     dec = ".")

sites <- metadata[ ! metadata$site.name %in% c("Les Rivaux", "Bombrini"),]

sites$color <- "#440154FF"
sites[sites$n.dates > 0, ]$color <- "orange"
sites[sites$n.sites > 0, ]$color <- "#21908CFF"

# DEFINE UI ----
ui <- shinyUI(  
  fluidPage(
    theme = shinytheme("simplex"), 
    tags$head(
      tags$style(HTML(css)),
      tags$script(HTML(js))
    ),
    # fluidRow(
    #   column(1),
    #   column(11, align="left", # : left column 
    HTML("<div align=left>
             <h1>The <i><a href=https://github.com/sebastien-plutniak/archeoviz target=_blank>archeoViz</a></i> portal</h1>"
    ),
    # )),
    tabsetPanel(id="tabs",
                tabPanel("Home",  # : TAB home ----
                         # fluidRow(
                         #   column(1),
                         #   column(11, align="center", # : left column 
                         #          HTML("<div align=left>
                         #          <h1>The <i><a href=https://github.com/sebastien-plutniak/archeoviz target=_blank>archeoViz</a></i> portal</h1>"
                         #          ),
                         #   )),
                         fluidRow(
                           column(1),
                           column(3, align="center",
                                  tags$div(
                                    HTML(paste("<div style=width:90%;, align=left>
             <h2>Presentation</h2>
   <p> This portal gives access to archeological datasets edited online using instances of the",
                                               span(
                                                 `data-toggle` = "tooltip", `data-placement` = "bottom",
                                                 title = "Click to go to the archeoViz website and get more information.",
                                                 HTML("<i><a href=https://archeoviz.hypotheses.org/ target=_blank>archeoViz</a></i>")),
                                               " application. Each instance can be <b>cited</b> and is referenced in multiple repositories (<a href=https://hal.science/ARCHEOVIZ target=_blank>HAL</a>, <a href=https://www.base-search.net/Search/Results?lookfor=aut:'maintainers%2C+archeoViz+platform' target=_blank>BASE</a>, etc.).
    </p>              
    <p><i>archeoViz</i> is designed for <b>spatialised data</b> in archaeology generated during excavations and surveys. It allows to <b>visualise</b> a dataset, interactively <b>explore</b> it, and quickly deploy and <b>communicate</b> it on the web.
    </p>
    <p>
      It offers interactive <b>visualisations</b>,
      can generate and export <b>3D view</b>, <b>cross sections</b> and <b>maps</b> of the remains, 
      can run basic <b>spatial statistics</b> methods 
      (convex hull, regression surfaces, 2D kernel density estimation), 
      <b>export</b> data to other <b>online applications</b>,
      and display the <b>timeline</b> of an excavation.  
    </p>
    <p>
      This app is distributed as an open source <a href=https://cran.r-project.org/package=archeoViz target=_blank>R package</a>, with its code available on <a href=https://github.com/sebastien-plutniak/archeoviz target=_blank>github</a>.
    </p>
    <p>
      <i>archeoViz</i> can be used locally on a personal computer or deployed on a server.
      It can be run statically with a specific dataset or dynamically by loading data through the interface, which is available in 
      <a href=https://analytics.huma-num.fr/archeoviz/de target=_blank>German</a>, 
      <a href=https://analytics.huma-num.fr/archeoviz/en target=_blank>English</a>, 
      <a href=https://analytics.huma-num.fr/archeoviz/fr target=_blank>French</a>,  
      <a href=https://analytics.huma-num.fr/archeoviz/it target=_blank>Italian</a>,   
      <a href=https://analytics.huma-num.fr/archeoviz/ro target=_blank>Romanian</a>, and
      <a href=https://analytics.huma-num.fr/archeoviz/pt target=_blank>Portuguese</a>.
    </p>
    </div>" )) # end html
                                  ), # end div
                           ), #end column
                           column(8, align="left", # : Use cases ----
                                  HTML(paste0("<div style=width:90%;, align=left>
                  <h2>Use cases</h2>
                  </div>",
                                              "<p style=font-size:16px><b>",
                                              nrow(sites), "</b> datasets &emsp;&emsp; <b>",
                                              format(sum(sites$n.objects, na.rm = T), big.mark=","),
                                              "</b> objects &emsp;&emsp;<b>", 
                                              format(sum(sites$n.refits, na.rm = T), big.mark=","),
                                              "</b> refitting relations &emsp;&emsp;<b>", 
                                              format(sum(sites$n.sites, na.rm = T), big.mark=","),
                                              "</b> sites  &emsp;&emsp;<b>", 
                                              format(sum(sites$n.dates, na.rm = T), big.mark=","),
                                              "</b> dates",
                                              "<p/>"
                                  )),
                                  leafletOutput("map", width="90%"), # : leaflet map  ----
                                  # br(),
                                  checkboxInput("surfaces",
                                                "Show surfaces covered by the datasets"),
                                  HTML(paste0("<ul>
                    <li>Three types of datasets are referenced:
                      <ul>
                      <li>purple points: datasets about <b>objects</b>, point refer to the archaeological site's location.</li>
                      <li>turquoise points: datasets about <b>archaeological sites</b>, points correspond to the midpoint of the covered surfaces.</li>
                      <li>orange points: datasets about <b>dates</b>, points correspond to the midpoint of the covered surfaces.</li>
                      </ul></li>
                    <li><b>Surfaces</b> covered by the datasets can be shown by clicking on “Show surfaces…”.</li>  
                    <li>Click on a row of the table below to zoom the map to the dataset's location.</li>
                    <li>Use the slider to filter the datasets by site <b>altitude</b>.</li>
                    <li>Use the “Search” field to search sites by
                                              <b>site name</b>, <b>",
                                              span(
                                                `data-toggle` = "tooltip", `data-placement` = "bottom",
                                                title = "Click to display a graph of the chronological periods included.",
                                                HTML("<a href=chronoGraph.jpg target=_blank>chronological period</a></b>")),
                                              ",
                                                <b>country</b>, and ",
                                              span(
                                                `data-toggle` = "tooltip", `data-placement` = "bottom",
                                                title = "dates, lithic industry, animal bone, pottery, antler, lithic core, eggshell, millstone, microfauna, coprolites, bone industry, shell, human bone, metal objects, archaeological site",
                                                HTML("<b><a href=>remain type</a></b>.")),
                                              " </li>
                  </ul>"))
                           ) # end column
                         ), #end fluidrow
                         fluidRow(align = "left",
                                  column(1),
                                  column(11, 
                                         uiOutput("sliderAltitude"),
                                         DT::dataTableOutput("table",  width="90%"), # : table output ----
                                         br(),
                                         downloadButton("download.table", "Download the metadata table"),
                                         br(),br(),br(),br()
                                  )  # end column
                         )# end fluidrow
                ), # end tabset
                tabPanel("Statistics", # : TAB usage statistics ----
                         fluidRow(
                           column(1),
                           column(9,  
                                  h2("Usage statistics"),
                                  h3("Dataset-related instances"),
                                  plotly::plotlyOutput("specific.plot", width = "100%"),
                                  br(),
                                  h3("Generic instances and Portal"),
                                  plotly::plotlyOutput("generic.plot", width = "100%"),
                                  br()
                           ) # end column
                         ) # end fluidRow 
                ),
                tabPanel("Contribute", # : TAB contribute ----
                         fluidRow(column(1),
                                  column(7,
                                         tags$div(HTML("
                             <h2>Contact</h2>
                              Do you 
                              <ul>
                               <li>have a question?</li>
                               <li>want to reference an archeoViz instance here? </li>
                               <li> need support to create an archeoViz instance? </li>
                              </ul>
                              Feel free to write us at <i>archeoViz portal</i>'s maintainers: <a href=mailto:archeoviz-maintainers@services.cnrs.fr>archeoviz-maintainers@services.cnrs.fr</a>
               ") # end HTML
                                         ) # end div
                                  )    # end column
                         ) # end fluidrow
                ), #end tab
                tabPanel("References & Documentation", # : TAB references ----
                         fluidRow(column(1),
                                  column(7,  
                                         h2("Papers"),
                                         tags$div(HTML("
                      <ul>
              <li> Plutniak, Sébastien. 2023. “<a href=https://www.prehistoire.org/offres/doc_inline_src/515/0-BSPF_2023_1_2e_partie_Correspondance_PLUTNIAK.pdf target=_blank>Visualiser et explorer la distribution spatiale du mobilier archéologique : l’application archeoViz et son portail web</a>”, <i>Bulletin de la Société préhistorique française</i>, 120(1), p. 70–74.</li>
              <li> Plutniak, Sébastien. 2023. “<a href=https://joss.theoj.org/papers/ec7d14809161bb21d0e258742e64f131  target=_blank>archeoViz: an R package for the Visualisation, Exploration, and Web Communication of Archaeological Spatial Data</a>”, <i>Journal of Open Source Software</i>, 8(92), 5811, DOI: <a href=https://doi.org/10.21105/joss.05811 target=_blank>10.21105/joss.05811</a>.</li>
                     </ul>")),
                                         h2("Documentation"),
                                         tags$div(HTML("
                  <ul>  
                       <li> The package vignette in <a href=https://cran.r-project.org/web/packages/archeoViz/vignettes/archeoViz.html target=_blank>English</a>, <a href=https://cran.r-project.org/web/packages/archeoViz/vignettes/archeoViz-es.html  target=_blank>Spanish</a>, <a href=https://cran.r-project.org/web/packages/archeoViz/vignettes/archeoViz-fr.html target=_blank>French</a>. </li>
                       <li> For a less technical introduction, see the <a href=https://archeoviz.hypotheses.org/ target=_blank><i>archeoViz blog</i></a>.</li>
                      </ul>")),
                                         h2("Package and Programming Code"),
                                         tags$div(HTML("<ul>
                       <li> The <i>archeoViz</i> R package is available on the <a href=https://CRAN.R-project.org/package=archeoViz target=_blank>CRAN</a>.</li>
                       <li> Plutniak, Sébastien, Renata Araujo, Laura Coltofean, Nicolas Delsol, Sara Giardino, Julian Laabs. 2023. “archeoViz. Visualisation, Exploration, and Web Communication of Archaeological Excavation Data”. v1.3.5, <i>Zenodo</i>, DOI: <a href=https://doi.org/10.5281/zenodo.7460193 target=_blank>10.5281/zenodo.7460193</a>.</li>
                       <li>Plutniak, Sébastien, Anaïs Vignoles, Élisa Caron-Laviolette. 2023. The archeoViz Portal: Dissemination of Spatial Archaeological Datasets, DOI: <a href=https://doi.org/10.5281/zenodo.10251182 target=_blank>10.5281/zenodo.10251182</a> </li>
                      </ul>
                                ")),
                                         h2("Presentations slides"),
                                         tags$div(HTML("<ul>
                        <li> Plutniak, Sébastien, Anaïs Vignoles, Élisa Caron-Laviolette. 2024. “<a href=https://doi.org/10.5281/zenodo.13862828 target=_blank>Keep Control of Your Data! A Decentralized, Open-Source and Community-Driven Approach to Archaeological Data Sharing Through the archeoViz Portal</a>”.</li>
                        <li> Plutniak, Sébastien, Anaïs Vignoles. 2023. “<a href=https://hal.science/hal-04070444 target=_blank>L’application web / package archeoViz et son portail web. Une solution décentralisée d’éditorialisation de données archéologiques spatialisées</a>”.</li>
                        <li> Plutniak, Sébastien. 2023. “<a href=https://hal.science/hal-04146410 target=_blank>Fostering the Publication of Spatial Archaeological Data: a Decentralized Approach. The archeoViz Web Application and its Portal</a>”.</li>                        
                      </ul>       "
                                         )), #end div and html
                                         h2("Openness index"),
                                         tags$div(HTML("
                                         Datasets referenced on the <i>archeoViz portal</i> are described with an “Openness index” based on 5 parameters:
                    <ul>
                      <li> <b>Instance</b>: the dataset can be explored through a specific <i>archaeoViz</i> instance.</li>
                      <li> <b> Reprocessing script</b>: the code to prepare the dataset is published, providing transparency about data selection and recoding.</li>
                      <li> <b>Published dataset</b>: the dataset is published on a public repository.</li>
                      <li> <b>Open licence</b>: the dataset is under an open license.</li>
                      <li> <b>Internal linking</b>: visible records in the <i>archeoViz</i> instance are linked to their corresponding record in the public repository.</li>
                      </ul>     "
                                         )) #end div and html                                         
                                  )    # end column
                         ) # end fluidrow
                ), #end tab
                tabPanel("Contact, Credits & Acknowledgments", # : TAB credits ----
                         fluidRow(column(1),
                                  column(7,
                                         tags$div(HTML("
                             <h2>Contact</h2>
                              Do you have questions or want your data here?
                              <ul>
                               <li><i>archeoViz portal</i>'s maintainers: <a href=mailto:archeoviz-maintainers@services.cnrs.fr>archeoviz-maintainers@services.cnrs.fr</a></li>
                              <li> subscribe to the <i>archeoViz</i> <a href=https://listes.services.cnrs.fr/wws/info/archeoviz-users target=_blank>mailing list</a>
                              </ul>
                             <h2>Credits</h2>
                              <ul>
                              <li>The <i>archeoViz</i> application / R package is developed by <b>Sébastien Plutniak</b> (CNRS).</li>
                              <li>The <i>archeoViz portal</i> is maintained by <b>S. Plutniak</b>, <b>Anaïs Vignoles</b>, and <b>Élisa Caron-Laviolette</b>.
                </li></ul>
                 <h2>Acknowledgments</h2>
                <p> Arthur Coulon, Solène Denis, Olivier Marlet, and Thomas Perrin supported the project in its early stage.</p>
                <p>First contributors to the portal include: Astolfo Araujo, Jean-Pierre Chadelle, Elsa Defranoult, Solène Denis, Emmanuel Discamps, Mathieu Langlais, Maureen Le Doaré, Grégor Marchand✝, Jean-Claude Marquet, Alexandre Michel, Thomas Perrin, Anthony Sécher, Peter Tóth, Anaïs Vignoles.
                </p>
                <p>Renata Araujo, Sara Giardino, Julian Laabs, Nicolas Delsol, and Laura Coltofean translated the application into Portuguese, Italian, German, Spanish, and Romanian respectively.</p><br>
                 <div style='text-align:center'>
                    <b><i>archeoViz</i></b> and the <b><i>archeoViz Portal</i></b>
                    <br><br>
                    <table> 
                      <tr>
                        <td>are developped at: <br> <br> <a href=https://www.cnrs.fr target=_blank><img height='60px' src=logo-cnrs.png></a><a href=https://citeres.univ-tours.fr/ target=_blank><img height='60px' src=logo-citeres.png></a>  </td>
                        <td>  are hosted by: &nbsp; <br> <br> <a href=https://www.huma-num.fr/ target=_blank><img height='60px' src=logo-humanum.jpg></a></td>
                        <td> are supported by: &nbsp;  &nbsp; &nbsp; <br> <br> <a href=https://masa.hypotheses.org/archeoviz-visualisation-des-donnees-archeologiques  target=_blank><img height='60px' src=logo-masa.png></a></td>
                        <td> received the Open science prize: &nbsp;  &nbsp; &nbsp; <br> <br> <a href=https://www.ouvrirlascience.fr/remise-des-prix-science-ouverte-du-logiciel-libre-de-la-recherche-2024/  target=_blank><img height='60px' src=logo-prix-so.jpg></a></td>                    
                        <td> are referenced as: &nbsp; <br>  <br><a href=http://doi.org/10.17616/R31NJNOZ  target=_blank><img height='60px' src=logo-re3data.png></a></td>
                      </tr>
                    </table> 
                </div> 
               ") # end HTML
                                         ) # end div
                                  )    # end column
                         ) # end fluidrow
                ) #end tab
    ) # end tabsetpanel
  ) #endfluidPage
) #end  shinyUI



# DEFINE SERVER  ----    
server <- function(input, output, session) {
  
  # format links to app: ----
  sites$tab.link <- paste0("<a href=", sites$archeoviz.url, " title='Click to open this site with archeoViz' target=_blank>", sites$site.name, "</a>")
  sites$popup <- paste0("<b>", sites$tab.link, "</b><br>", sites$period)
  sites <- sites[order(sites$site.name), ]
  sites$id <- 1:nrow(sites)
  
  sites$fillOpacity <- .2
  # sites$color <- factor(sites$period, labels = rainbow(length(unique(sites$period)))  )
  
  # format periods: ----
  sites$period <-  sites$period1
  idx <- sites$period2 != ""
  sites[idx, ]$period <- paste0(sites[idx, ]$period1,
                                " → ", sites[idx, ]$period2)
  
  # openness scale: ----
  #  add tag for instance with published datasets 
  # idx <- sites$data.identifier.type %in% c("doi", "url")
  # sites[idx, ]$tab.link <- paste0(sites[idx, ]$tab.link,
  #                                 "&ensp; <img  title='The dataset edited with this instance is available in open access.' height=12px src=icon-openaccess.png>")
  
  openness.scale <- function(instance){
    
    icon.yellow <- "<img height=12px src=icon-openaccess-yellow.png>"
    icon.purple <- "<img height=12px src=icon-openaccess-purple.png>"
    
    openness.labels <- c("archeoViz instance",
                         "reprocessing script available",
                         "published dataset",
                         "dataset under open license",
                         "internal linking")
    
    ranks <- c(TRUE, 
               instance[which(names(instance) == "workflow.identifier.value")] != "–",
               instance[which(names(instance) == "data.identifier.value")] != "–",
               instance[which(names(instance) == "data.license")] != "–",
               instance[which(names(instance) == "internal.linking")] == "TRUE"
    )
    
    paste0(c("<div title='",
             sum(ranks), "/5: ",
             paste(openness.labels[ranks], collapse=", "),
             "'>",
             paste0(rep(icon.purple,
                        length(openness.labels[ranks])), 
                    collapse = ""),
             paste0(rep(icon.yellow,
                        5 - length(openness.labels[ranks])), 
                    collapse = ""),
             "</div>"),
           collapse = ""
    )
  }
  
  
  sites$openness <- apply(sites, 1, openness.scale)
  
  
  # span(
  #   `data-toggle` = "tooltip", `data-placement` = "bottom",
  #   title = "Click to display a graph of the chronological periods included.",
  #   HTML("<a href=chronoGraph.jpg target=_blank>chronological period</a></b>"))
  
  # 
  idx <- grep("object_", names(sites))
  sites$object_type.keywords <- apply(sites[, idx], 1,
                                      paste0, collapse = " ")
  
  # Table output ----
  output$table <- DT::renderDataTable({ 
    
    sites <- sites[sites$altitude >= input$altitude[1] & sites$altitude <= input$altitude[2], ]
    
    tab <- sites[ , c("tab.link", "openness", "site.country", "period.keywords", "period", "n.objects", "n.refits", "n.objects.in.refitting.set", "n.sites", "n.dates", "object_type.keywords", "color") ]
    colnames(tab) <- c("Site name", "Openness", "site.country", "period.keywords", "Period coverage", "Nr of objects", "Nr of refitting relationships", "Nr of refitting objects", "Nr of sites", "Nr of dates", "object_type.keywords", "color")
    
    DT::datatable(tab, rownames=F,  escape=F, selection = 'single',
                  options = list(lengthMenu = c(10, 20, 40), pageLength=10,
                                 orderClasses = TRUE,
                                 pageLength = nrow(tab),
                                 columnDefs = list(list(visible=FALSE,
                                                        targets = c("period.keywords", "site.country", "object_type.keywords", "color") ))))
  })
  
  # download table  ----
  output$download.table <- downloadHandler(
    filename = "archeoviz-datasets-metadata.csv",
    content = function(file){
      write.csv(metadata[, - ncol(metadata)], file, row.names = F)
    }
  )
  
  # leaflet map update  ----
  observe({
    req(input$table_rows_selected)
    
    row <- input$table_rows_selected
    sites[row, ]$fillOpacity <- 1 # selected point is plain filled
    
    leafletProxy("map")  %>% 
      clearMarkers()    %>%
      clearMarkerClusters()   %>%
      setView(lng = sites[row, ]$lon, lat = sites[row, ]$lat, zoom = 9)  %>%
      addCircleMarkers(data = sites, lng= ~lon, lat = ~lat, 
                       popup = ~popup, layerId = ~id,
                       label = ~site.name,
                       color = ~color, radius = 6,
                       fillOpacity = ~fillOpacity,
                       opacity = 0.99,
                       clusterOptions = markerClusterOptions())
  })
  
  output$map <- renderLeaflet({ # leaflet map ----
    
    map <- leaflet() %>%  
      setView(lng = 10, lat = 30, zoom = 1)  %>%
      addWMSTiles(baseUrl = 'http://ows.mundialis.de/services/service?',
                  layers = "TOPO-WMS",
                  attribution = '&copy; <a href=https://www.openstreetmap.org/copyright>OpenStreetMap</a> contributors') %>%
      # addProviderTiles(providers$Esri.WorldPhysical) %>%
      # addProviderTiles(providers$OpenTopoMap) %>%   #Esri.WorldTerrain
      #  
      # addTiles("https://tiles.stadiamaps.com/tiles/stamen_terrain_background/{z}/{x}/{y}{r}.png",
      #          attribution = '&copy; <a href=https://www.stadiamaps.com/ target=_blank>Stadia Maps</a> &copy; <a href=https://www.stamen.com/ target=_blank>Stamen Design</a> &copy; <a href=https://openmaptiles.org/ target=_blank>OpenMapTiles</a> &copy; <a href=https://www.openstreetmap.org/copyright>OpenStreetMap</a> contributors'
      #          ) %>%
      addLegend("bottomright", 
                title = "Type of data", 
                colors = c("#440154FF", "#21908CFF", "orange"),
                labels = c("objects", "sites", "dates"),
                opacity = 0.8) 
    
    if(input$surfaces){
      
      surfaces <- sites[ ! is.na(sites$bbox.lon1), ]
      surfaces$area <- (surfaces$bbox.lon2 - surfaces$bbox.lon1) *
        (surfaces$bbox.lat2 - surfaces$bbox.lat1) 
      surfaces <- surfaces[order(abs(surfaces$area), decreasing = T), ]
      
      map <- map %>%  # — add surfaces ####
      addRectangles(data = surfaces,
                    lng1 = ~bbox.lon1,
                    lat1 = ~bbox.lat1,
                    lng2 = ~bbox.lon2,
                    lat2 = ~bbox.lat2,
                    popup = ~popup,
                    color = "darkred",
                    weight = 2, fillOpacity = 0.1,
                    label = ~site.name,
                    options = pathOptions(clickable = T,
                                          interactive = TRUE),
                    popupOptions = popupOptions(closeOnClick=T)
      )
    }
    
    map %>% addCircleMarkers(data = sites[ ! is.na(sites$lat), ],
                             ~lon, ~lat,
                             popup = ~popup, layerId = ~id,
                             label = ~site.name,
                             color = ~color, radius = 6,
                             fillOpacity = ~fillOpacity,
                             opacity = 0.99,
                             options = pathOptions(clickable = T,
                                                   interactive = TRUE),
                             popupOptions = popupOptions(closeOnClick=T),
                             clusterOptions = markerClusterOptions()) 
  })
  
  # Visit stats ----
  
  # : load statistics  ----
  site.list <- read.csv("log-stats.csv")
  site.list$date <- as.Date(site.list$date)
  
  site.list.max <- site.list %>% dplyr::group_by(site, type)  %>%
    dplyr::summarize("max" = max(visits))
  
  
  output$specific.plot <- plotly::renderPlotly({
    
    specific.stats <- site.list[site.list$type == "specific", ]
    specific.max <- site.list.max[site.list.max$type == "specific", ]
    
    specific.plot <- ggplot(specific.stats,
                            aes(x = date, y = visits, color = site)) +
      theme_light() +
      geom_path(show.legend = F) +
      geom_text(data = specific.max,
                aes(y = max, label = site, color = site, group = site),
                x =  max(site.list$date) + .5,
                size = 4, show.legend=F, hjust = "left", check_overlap=F) +
      scale_x_date("Date", date_labels = "%Y-%m-%d",
                   limits = c(min(site.list$date), max(site.list$date) + 2)) +
      theme(axis.text.x = element_text(angle = 45,  hjust = 1, size=9)) +
      ylab("Cumulated number of visits") +
      annotate("text", x = as.Date("2023-01-18"),
               y = max(specific.stats$visits) - 10,
               size = 2.5,
               label = paste("Total:", sum(specific.stats$Freq), "visits") )
    
    ggplotly(specific.plot, tooltip = c("visits", "site", "date")) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'sendDataToCloud',
               'select2d',
               'resetScale2d',
               'lasso2d',
               'hoverClosestCartesian',
               'hoverCompareCartesian'))
  })
  
  output$generic.plot <- plotly::renderPlotly({
    
    generic.stats <- site.list[site.list$type %in% c("generic", "portal"),]
    generic.max <- site.list.max[site.list.max$type %in% c("generic", "portal"), ]
    
    generic.plot <- ggplot(generic.stats,
                           aes(x = date, y = visits, color = site)) +
      theme_light() +
      geom_path(show.legend = F) +
      geom_text(data = generic.max,
                aes(y = max, label = site, color = site, group = site),
                x =  max(site.list$date) + .5,
                size = 4, show.legend=F, hjust = "left", check_overlap=F) +
      scale_x_date("Date", date_labels = "%Y-%m-%d",
                   limits = c(min(site.list$date), max(site.list$date) + 2)) +
      theme(axis.text.x = element_text(angle = 45,  hjust = 1, size=9)) +
      ylab("Cumulated number of visits") +
      annotate("text", x = as.Date("2023-01-18"),
               y = max(generic.stats$visits) - 10,
               size = 2.5,
               label = paste("Total:", sum(generic.stats$Freq), "visits") )
    
    ggplotly(generic.plot, tooltip = c("visits", "site", "date")) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'sendDataToCloud',
               'select2d',
               'resetScale2d',
               'lasso2d',
               'hoverClosestCartesian',
               'hoverCompareCartesian'))
  })
  
  
  output$sliderAltitude <- renderUI({  # altitude slider ----
    sliderInput("altitude", "Filter the table by altitude (m):",  width="90%", sep = "",
                min = min(metadata$altitude, na.rm = T), 
                max = max(metadata$altitude, na.rm = T),
                round = T,
                value = c(min(metadata$altitude, na.rm = T), 
                          max = max(metadata$altitude, na.rm = T))
    )
  })
  
} # end of server.R

shinyApp(ui = ui, server = server)



