### new citations from 2025 and 2026

### i want to deduplicate the results from scopus and web of science and identify
### new records that were not part from our first search
### the new records should be written out as a .bib file for abstract screening

library(litsearchr)
library(tidyverse)
library(synthesisr)

## import both .ris files at once
naiveimport <- litsearchr::import_results("data/screening/additional_screening_for_2025_26")

## remove duplicates
naiveresults <- remove_duplicates(naiveimport, field = "title", method = "exact")



################################################################################
### importing the results until 25.04.2025 and deduplicate it

newimport <- litsearchr::import_results("data/screening/search_7")

newresults <- remove_duplicates(newimport, field = "title", method = "exact")

### filter both databases for 2025 (and 2026)
res2025 <- naiveresults |>
  filter(year == "2025" | year == "2026")

old2025 <- newresults |>
  filter(year == "2025")

### remove rows from the new dataframe that also occur in the old one to receive
### a list of articles for a second abstract and article screening step

new2025 <- res2025[!(res2025$doi %in% old2025$doi),]

### export the new list as a .bib file

#write_refs(new2025, file = "data/screening/additional_screening_for_2025_26/new_results.bib", format = "bib")











