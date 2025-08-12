The archeoViz Portal
================

[![SWH](https://archive.softwareheritage.org/badge/origin/https://github.com/sebastien-plutniak/archeoviz-portal-app/)](https://archive.softwareheritage.org/browse/origin/?origin_url=https://github.com/sebastien-plutniak/archeoviz-portal-app)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10251182.svg)](https://doi.org/10.5281/zenodo.10251182)

## Presentation

The *archeoViz Portal* is an interactive catalogue of use-cases of the [https://github.com/sebastien-plutniak/archeoviz](*archeoViz*) application (an application for the visual and statistical exploration of archaeological spatial data). The *archeoViz Portal* is deployed since 2023 at https://analytics.huma-num.fr/archeoviz/home. These instances are documented with detailed metadata, partially aligned on standards (geonames, Pactols, ORCID, VIAF).

*archeoViz Portal* is powered by the [*spatialCatalogueViewer*](https://github.com/sebastien-plutniak/spatialCatalogueViewer/) application, allowing for browsing the catalogue through a map and a dynamic table. Using the table *archeoViz* instances can be:

* sorted by:
    * site name
    * openness index
    * chronological period
    * number of objects
    * number of refitting relationships
    * number of refitting objects
    * number of sites
    * number of dates
* retrieved by: 
    * site name
    * country
    * continent
    * altitude
    * type of remains, and 
    * chronological period (and larger period).


## metadata.generator.R

The script `metadata.generator.R` generates an HTML code to be included in the homepage of *archeoViz* instances.

``
metadata.generator(df, site.name = "", lang = "en")
``

* **df**: data frame with the metadata
* **site.name**: name of the archeoViz instance (searched in the "site.name" variable of the table given in 'df')
* **lang**: language to use ("en" for English, "fr" for French)


## chrono-graph.R

The script `chrono-graph.R` is executed when the contents of *archeoViz portal* is updated on the server.

* Read the `metadata.csv` and chronology.csv tables 
* Create a directed acyclic graph in which nodes are periods and edges are inclusion relationships.
* For each entry in metadata.csv:
    * Read the value of the "period" variable(s) (period1, period2, period3)
      For example : period1 = "Solutrean"
    * Extract a path from the root of the DAG to each value
      For example: "Quaternary Pleistocene paleolithc upper paleolithic solutrean"
    * Extract path nodes' labels and stored it as a characters string in a "period.keywords" variable added to the table.
* Write the resulting table in metadata-deployed.csv

The *archeoViz Portal* app reads `metadata-deployed.csv`. 

If a user writes a character string in the search form, the lines of `metadata-deployed.csv` containing this string are retrieved. In addition, subperiods are handled. For example, given that the "Solutrean" period is a subperiod of the "upper paleolithic" period, searching the string "upper paleolithic" will also retrieve the instances associated with the "Solutrean" period. The corresponding variables in `metadata-deployed.csv` are:
* period1 = "Solutrean"
* period.keywords = "Quaternary Pleistocene Paleolithc Upper paleolithic Solutrean"


## log-analysis.R

The script `log-analysis.R` is executed when the contents of *archeoViz portal* is updated on the server.
It reads the names of the fils in the `log` folder and generates a file named `log-stats.csv`.
