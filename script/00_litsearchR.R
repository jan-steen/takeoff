################################################################################
######### install package via github
#install.packages("remotes")
#library(remotes)
#install_github("elizagrames/litsearchr", ref="main")
library(litsearchr)
library(ggplot2)
library(igraph)
library(dplyr)
library(ggraph)
library(synthesisr)
################################################################################

### Explanations:
#https://elizagrames.github.io/litsearchr/litsearchr_vignette.html

################################################################################
######### write naive search and import .ris files

#	(Drone OR UAV OR UAS OR unmanned) AND (biomass  OR shrub OR bush OR nitrogen OR ruderal OR disturbance OR reforestation OR grass OR drainage OR water OR moisture) AND (grassland OR heather OR bog OR fen OR steppe OR floodplain)
#
## new search: 
##( drone OR uav OR uas ) AND ( natura 2000 OR ffh OR nature AND conservation )


# Search results in Scopus: 372
# Search results in Web of Science: 254

## import both .ris files at once
naiveimport <- litsearchr::import_results("data/screening/search_1")

## remove duplicates
naiveresults <- deduplicate(naiveimport, match_by = "title", method = "exact")
### 392 unique results

################################################################################
####### identify potential keywords

#  two possibilities: use keywords used by the authors (tagged)
#  or extract potential keywords from title and abstracts (RAKE)

rakedkeywords <-
  litsearchr::extract_terms(
    text = paste(naiveresults$title, naiveresults$abstract),
    method = "fakerake",
    min_freq = 4,
    ngrams = TRUE,
    min_n = 1,
    language = "English"
  )


taggedkeywords <-
  litsearchr::extract_terms(
    keywords = naiveresults$keywords,
    method = "tagged",
    min_freq = 4,
    ngrams = TRUE,
    min_n = 1,
    language = "English"
  )

# possible changes in min_freq and min_n (How long should keywords be?)

################################################################################
######### Build the keyword co-occurrence network

# combine both lists of keywords
all_keywords <- unique(append(taggedkeywords, rakedkeywords))

# document-feature matrix 
naivedfm <-
  litsearchr::create_dfm(
    elements = paste(naiveresults$title, naiveresults$abstract),
    features = all_keywords
  )

# create co-occurrence network
naivegraph <-
  litsearchr::create_network(
    search_dfm = naivedfm,
    min_studies = 3,
    min_occ = 3
  )


################################################################################

strengths <- strength(naivegraph)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->
  term_strengths

term_strengths


########## identify change points in keyword importance
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)

cutoff_fig

### find value to cut terms based on their strength
cutoff <-
  litsearchr::find_cutoff(
    naivegraph,
    method = "cumulative",
    percent = .70,
    imp_method = "strength"
  )

cutoff_fig +
  geom_hline(yintercept=cutoff, linetype="dashed")

### reduce the search terms
reducedgraph <-
  litsearchr::reduce_graph(naivegraph, cutoff_strength = cutoff[1])

### extract keywords after cutoff
searchterms <- litsearchr::get_keywords(reducedgraph)

head(searchterms, 20)

################################################################################

### new keywords should be manually evaluated and grouped

### groups 
# 1: drone / UAV as the remote sensing method
# 2: ecosystem quality assessment (like aboveground biomass)
# 3: type of Ecosystem (everything (Offenland))
searchterms <- data.frame(searchterms)
write.csv2(searchterms, "data/screening/search_terms.csv")

# manually group terms in the csv file
grouped_terms <- read.csv("data/screening/search_terms_grouped.csv")

# 1-10 group 1
# 11-41 group 2
# 42-53 group 3

grouped_terms <-list(
  uav = grouped_terms[c(1:10),],
  quality = grouped_terms[c(11:41),],
  ecosystem = grouped_terms[c(42:53),]
)

grouped_terms

################################################################################

### writing a new search based on the new keywords


write_search(
  grouped_terms,
  languages="English",
  exactphrase=FALSE,
  stemming=FALSE,
  closure="left",
  writesearch=FALSE
)


################################################################################

### checking the new search


newimport <- litsearchr::import_results("data/screening/search_6")

newresults <- deduplicate(newimport, match_by = "title", method = "exact")
nrow(newresults)
### 1. new search 1441 unique results
### 2. new search 1401
### 3. new search 715
### 4. new search 778
### 5. new search 1053

### checking if we missed any articles from the first search with the new search terms

naiveresults %>%
  mutate(in_newresults=title %in% newresults[, "title"]) ->
  naiveresults

naiveresults %>%
  filter(!in_newresults) %>%
  select(title)


### we missed some literature...


nicht_in_newresults <- setdiff(as.character(naiveresults$title), as.character(newresults$title))
length(nicht_in_newresults)


################################################################################
################################################################################
################################################################################

### write newresults as a single bibtex

head(newresults)
### remove row n_duplicates

newresults <- newresults[,c(1:28)]

write_refs(newresults, file = "data/screening/abstract_screening.bib", format = "bib")

### has to be imported to zotero and then exported again to be able to load into 
### Rayyan 

### two articles got lost because of unknown errors

### still some duplicates in list --> deduplicate in rayyan






















