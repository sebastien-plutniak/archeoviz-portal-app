remotes::install_github("sebastien-plutniak/spatialCatalogueViewer", upgrade = "never")

log.df <- read.csv( "../../archeoviz-log.csv")
write.csv(rbind(log.df, cbind("date" = format(Sys.Date()), "instance" = basename(getwd()))), "../../archeoviz-log.csv", row.names = F)


# data preparation  ----
# load data ----

metadata <- read.csv("metadata-deployed.csv", 
                     colClasses=c(
                       rep("double", 8),
                       "character",
                       "double",
                       rep("character", 2),
                       rep("double", 5),
                       rep("character", 53)),
                     dec = ".")

sites <- metadata[ ! metadata$site.name %in% c("Roc de Combe", "Tournoisis"),]

sites$"Type of data" <- "objects"
sites[sites$n.sites > 0, ]$"Type of data" <- "sites"
sites[sites$n.dates > 0, ]$"Type of data" <- "dates"



legend.labels <- c("objects", "sites", "dates")
legend.colors <- c("#440154FF", "#21908CFF", "orange") 

# format links to app: ----
sites$tab.link <- paste0("<a href=", sites$archeoviz.url, " title='Click to open this site with archeoViz' target=_blank>", sites$site.name, "</a>")
sites$popup <- paste0("<b>", sites$tab.link, "</b><br>", sites$period)
sites <- sites[order(sites$site.name), ]
# sites$id <- 1:nrow(sites)

# sites$fillOpacity <- .2
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


# css ----
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
  font-size: px;
  text-transform: none;
  font-weight: normal;
  text-align: left;
  max-width: 300px;
  content: <b>aa</b>;
}
.tooltip > .arrow::before {
  border-right-color: #73AD21;
}
iframe {
  width: 100%;
}
'

js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"

# texts ----

text.title <- " <h1>The <i><a href=https://github.com/sebastien-plutniak/archeoviz target=_blank>archeoViz</a></i> portal</h1>"

text.left <- "<div style=width:90%;, align=left>
             <h2>Presentation</h2>
   <p> This portal gives access to archeological datasets edited online using instances of the
   <span data-toggle='tooltip' data-placement='bottom' title='Click to go to the archeoViz website and get more information.'>
   <i><a href=https://archeoviz.hypotheses.org/ target=_blank>archeoViz</a></i>
   </span> application. Each instance can be <b>cited</b> and is referenced in multiple repositories (<a href=https://hal.science/ARCHEOVIZ target=_blank>HAL</a>, <a href=https://www.base-search.net/Search/Results?lookfor=aut:'maintainers%2C+archeoViz+platform' target=_blank>BASE</a>, etc.).
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
    <p>
      <ul>
        <li>Three types of datasets are referenced:
          <ul>
          <li>purple points: datasets about <b>objects</b>, point refer to the archaeological site's location.</li>
          <li>turquoise points: datasets about <b>archaeological sites</b>, points correspond to the midpoint of the covered surfaces.</li>
          <li>orange points: datasets about <b>dates</b>, points correspond to the midpoint of the covered surfaces.</li>
          </ul></li>
        <li>Use the “Search” field to search sites by
            <b>site name</b>, <b>
            <span data-toggle='tooltip' data-placement='bottom' title='Click to display a graph of the chronological periods included.'>
            <a href=chronoGraph.jpg target=_blank>chronological period</a></b></span>, <b>continent</b>, <b>country</b>, and 
              <span data-toggle='tooltip' data-placement='bottom' title='animal bone, antler, archaeological site, bone industry, ceramic building material, coprolites, dates, eggshell, human bone, lithic core, lithic industry, metal objects, microfauna, millstone, pottery, shell.'>
              <b><a href=>remain type</a></b>.
              </span>
            </li>
       </ul>
      </p>
    </div>"



text.top <- paste0("<div style=width:90%;, align=left>
                  <h2>Use cases</h2>
                  </div>",
                   "<div style=font-size:16px><b>",
                   nrow(sites), "</b> datasets &emsp;&emsp; <b>",
                   format(sum(sites$n.objects, na.rm = T), big.mark=","),
                   "</b> objects &emsp;&emsp;<b>", 
                   format(sum(sites$n.refits, na.rm = T), big.mark=","),
                   "</b> refitting relations &emsp;&emsp;<b>", 
                   format(sum(sites$n.sites, na.rm = T), big.mark=","),
                   "</b> sites  &emsp;&emsp;<b>", 
                   format(sum(sites$n.dates, na.rm = T), big.mark=","),
                   "</b> dates",
                   "</div>"
)


stat.tab <- "
        <h2>Usage statistics</h2>
        <h3>Dataset-related instances</h3>
          <iframe src='archeoviz-stat-specific.html' title='specific-stats' style='border:none;' height=500></iframe> 
        <h3>Generic instances and Portal</h3>
          <iframe src='archeoviz-stat-generic.html' title='generic-stats' style='border:none;' height=500></iframe> 
"


contribute.tab <- "<h2>Contribute</h2>
                      Do you:
                      <ul>
                       <li>have a question?</li>
                       <li>want to reference an <i>archeoViz</i> instance here? </li>
                       <li> need support to create an <i>archeoViz</i> instance? </li>
                      </ul>
                      Feel free to write us at <i>archeoViz Portal</i>'s maintainers: <a href=mailto:archeoviz-maintainers@services.cnrs.fr>archeoviz-maintainers@services.cnrs.fr</a>
               "

references.tab <- "
                  <h2>Papers</h2>
                      <ul>
                        <li> Plutniak, Sébastien. 2023. “<a href=https://www.prehistoire.org/offres/doc_inline_src/515/0-BSPF_2023_1_2e_partie_Correspondance_PLUTNIAK.pdf target=_blank>Visualiser et explorer la distribution spatiale du mobilier archéologique : l’application archeoViz et son portail web</a>”, <i>Bulletin de la Société préhistorique française</i>, 120(1), p. 70–74.</li>
                        <li> Plutniak, Sébastien. 2023. “<a href=https://joss.theoj.org/papers/ec7d14809161bb21d0e258742e64f131  target=_blank>archeoViz: an R package for the Visualisation, Exploration, and Web Communication of Archaeological Spatial Data</a>”, <i>Journal of Open Source Software</i>, 8(92), 5811, DOI: <a href=https://doi.org/10.21105/joss.05811 target=_blank>10.21105/joss.05811</a>.</li>
                     </ul>
                     <h2>Documentation</h2>
                      <ul>  
                         <li> The package vignette in <a href=https://cran.r-project.org/web/packages/archeoViz/vignettes/archeoViz.html target=_blank>English</a>, <a href=https://cran.r-project.org/web/packages/archeoViz/vignettes/archeoViz-es.html  target=_blank>Spanish</a>, <a href=https://cran.r-project.org/web/packages/archeoViz/vignettes/archeoViz-fr.html target=_blank>French</a>. </li>
                         <li> For a less technical introduction, see the <a href=https://archeoviz.hypotheses.org/ target=_blank><i>archeoViz blog</i></a>.</li>
                      </ul>
                    <h2>Package and Programming Code</h2>
                      <ul>
                         <li> The <i>archeoViz</i> R package is available on the <a href=https://CRAN.R-project.org/package=archeoViz target=_blank>CRAN</a>.</li>
                        <li> Plutniak, Sébastien, Renata Araujo, Laura Coltofean, Nicolas Delsol, Sara Giardino, Julian Laabs. 2023. “archeoViz. Visualisation, Exploration, and Web Communication of Archaeological Excavation Data”. v1.3.5, <i>Zenodo</i>, DOI: <a href=https://doi.org/10.5281/zenodo.7460193 target=_blank>10.5281/zenodo.7460193</a>.</li>
                        <li>Plutniak, Sébastien, Anaïs Vignoles, Élisa Caron-Laviolette. 2023. The archeoViz Portal: Dissemination of Spatial Archaeological Datasets, DOI: <a href=https://doi.org/10.5281/zenodo.10251182 target=_blank>10.5281/zenodo.10251182</a> </li>
                      </ul>
                    <h2>Presentations slides</h2>
                      <ul>
                        <li> Plutniak, Sébastien, Anaïs Vignoles, Élisa Caron-Laviolette. 2024. “<a href=https://doi.org/10.5281/zenodo.13862828 target=_blank>Keep Control of Your Data! A Decentralized, Open-Source and Community-Driven Approach to Archaeological Data Sharing Through the archeoViz Portal</a>”.</li>
                        <li> Plutniak, Sébastien, Anaïs Vignoles. 2023. “<a href=https://hal.science/hal-04070444 target=_blank>L’application web / package archeoViz et son portail web. Une solution décentralisée d’éditorialisation de données archéologiques spatialisées</a>”.</li>
                        <li> Plutniak, Sébastien. 2023. “<a href=https://hal.science/hal-04146410 target=_blank>Fostering the Publication of Spatial Archaeological Data: a Decentralized Approach. The archeoViz Web Application and its Portal</a>”.</li>                        
                      </ul>
                    <h2>Openness index</h2>
                       Datasets referenced on the <i>archeoViz Portal</i> are described with an “Openness index” based on 5 parameters:
                    <ul>
                      <li> <b>Instance</b>: the dataset can be explored through a specific <i>archaeoViz</i> instance.</li>
                      <li> <b> Reprocessing script</b>: the code to prepare the dataset is published, providing transparency about data selection and recoding.</li>
                      <li> <b>Published dataset</b>: the dataset is published on a public repository.</li>
                      <li> <b>Open licence</b>: the dataset is under an open license.</li>
                      <li> <b>Internal linking</b>: visible records in the <i>archeoViz</i> instance are linked to their corresponding record in the public repository.</li>
                      </ul>     "

contact.tab <- "
               <h2>Contact</h2>
                  Do you have questions?
                  <ul>
                    <li> write to the <i>archeoViz Portal</i>'s maintainers: <a href=mailto:archeoviz-maintainers@services.cnrs.fr>archeoviz-maintainers@services.cnrs.fr</a></li>
                    <li> subscribe to the <i>archeoViz</i> <a href=https://listes.services.cnrs.fr/wws/info/archeoviz-users target=_blank>mailing list</a>
                  </ul>
               <h2>Credits</h2>
                <ul>
                  <li>The <i>archeoViz</i> application / R package is developed by <b>Sébastien Plutniak</b> (CNRS).</li>
                  <li>The <i>archeoViz Portal</i> is maintained by <b>S. Plutniak</b>, <b>Anaïs Vignoles</b>, and <b>Élisa Caron-Laviolette</b>.</li>
                </ul>
              <h2>Acknowledgments</h2>
                  <p> Arthur Coulon, Solène Denis, Olivier Marlet, and Thomas Perrin supported the project in its early stage.</p>
                  <p>First contributors to the portal include: Astolfo Araujo, Jean-Pierre Chadelle, Elsa Defranoult, Solène Denis, Emmanuel Discamps, Mathieu Langlais, Maureen Le Doaré, Grégor Marchand✝, Jean-Claude Marquet, Alexandre Michel, Thomas Perrin, Anthony Sécher, Peter Tóth, Anaïs Vignoles.</p>
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
               "

sites$resource.name <- sites$site.name

tab <- sites[ ,  c("resource.name", "lat", "lon", "bbox.lon1", "bbox.lat1", "bbox.lon2", "bbox.lat2", "tab.link", "altitude", "openness", "site.continent", "site.country", "period.keywords", "period", "n.objects", "n.refits", "n.objects.in.refitting.set", "n.sites", "n.dates", "object_type.keywords", "Type of data", "popup") ]
colnames(tab) <- c("resource.name", "lat", "lon", "bbox.lon1", "bbox.lat1", "bbox.lon2", "bbox.lat2", "Site name", "Altitude", "Openness", "site.continent", "site.country", "period.keywords", "Period coverage", "Nr of objects", "Nr of refitting relationships", "Nr of refitting objects", "Nr of sites", "Nr of dates", "object_type.keywords", "Type of data", "popup")



# exec spatialCatalogueViewer----
spatialCatalogueViewer::spatialCatalogueViewer(data = tab,   
                                               text.title = text.title,
                                               text.top = text.top,
                                               text.left = text.left, 
                                               map.provider = "Esri.WorldImagery",
                                               map.set.lon = 10, map.set.lat = 20,
                                               map.legend.variable = "Type of data",
                                               map.legend.labels = legend.labels, 
                                               map.legend.colors = legend.colors,
                                               map.height = 600,
                                               map.area.fill.color = "white",
                                               map.area.fill.opacity = .1,
                                               map.show.areas = T,
                                               map.min.zoom = 3,
                                               table.hide.columns = c("resource.name", "period.keywords", "site.continent", "site.country", "object_type.keywords", "Type of data"),
                                               table.filter = "none",
                                               table.pageLength = 15,
                                               data.download.button = TRUE,
                                               tabs.contents = list(
                                                 "Statistics" = stat.tab,
                                                 "Contribute" = contribute.tab, 
                                                 "References & Documentation" = references.tab,
                                                 "Contact, Credits & Acknowledgments" = contact.tab),
                                               css = css, js = js,
                                               theme = "simplex") 

