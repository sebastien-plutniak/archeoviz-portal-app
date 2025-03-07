The archeoViz Portal
================

The *archeoViz Portal* is a web application to collect and explore references of independent instances of the *[https://github.com/sebastien-plutniak/archeoviz](archeoViz)* application (an application for visual  and statistical exploration of archaeological spatial data). 
The *archeoViz portal* is deployed since 2023 at https://analytics.huma-num.fr/archeoviz/home.

Using the dynamic table,  *archeoViz* instances can be:

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
