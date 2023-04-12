
########################
## 
## DELILLE VIZ
##
########################

Data visualization for the project Reconstruire Delille (https://delille.philhist.unibas.ch) project by Prof. Hugues Marchal, University of Basel.


The visualisations were developed by the University of Basel's Research and Infrastructure Support RISE (rise@unibas.ch) in collaboration with Prof. Marchal in 2022-2023.

The data comes from the project. It has been structured and enriched in collaboration with the NIE/INE project, ended in 2020 (https://github.com/nie-ine). New data queries have been added by the University of Basel's Research and Infrastructure Support RISE (rise@unibas.ch).


###################################
##
## FILES AND FOLDERS STRUCTURE
##
###################################

The folder 'scripts' is where the R code for generating the visualisations is hosted. Each visualisation has its own folder, containing the script per se in a file called app.R (not to be renamed) and the data in one or more .Rda files.

The folder 'data' contains the data for the visualisations
- in the JSON format, to be used by the R scripts, and
- (some data) in CSV, a simple tabular format that can be opened in MS Excel or another spreadsheet editor.

The folder 'original-rdf-data-and-sparql' contains the full project data in the Notation 3 language (a superset of RDF) and the SPARQL queries to extract the data contained in the 'data' folder described above. The SPARQL queries whose file name ends in '_ES' have been added by RISE. An application (SPARQL endpoint, triplestore) is needed to run the SPARQL queries against the full data.

The folder 'ontologyVisualisation' contains the visualisation of the entire ontology, the source ontology in TTL (turtle) format and a readme file for documentation.



####################
##
## ONTOLOGIES
##
####################

The ontologies at http://e-editiones.ch/ontology/ etc. are available in the legacy repo https://github.com/nie-ine/Ontologies.
