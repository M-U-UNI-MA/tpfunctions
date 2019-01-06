library(tidyverse)
# set internal functions =================================================================
int_read <- function(file) {
  readr::read_delim(file, ";",
                    col_types = readr::cols(.default = "c"),
                    na = character())
}
int_punct_complete  <- function(string) {
  tpfuns::top_punct(
    string      = string,
    lower       = TRUE,
    punct.stand = TRUE,
    punct.rem   = "complete",
    punct.sub   = TRUE
  )
}

# read lemmas ============================================================================
lemma.ant <- int_read("data-raw/lemmas_ant.csv") %>% mutate(origin = "ant")
lemma.own <- int_read("data-raw/lemmas_own.csv") %>% mutate(origin = "own")
table_lemmas <- bind_rows(lemma.own, lemma.ant) %>% distinct(lemma, term, .keep_all = TRUE)

# standardize characters =================================================================
table_lemmas <- table_lemmas %>%
  mutate(lemma = int_punct_complete(lemma), term = int_punct_complete(term)) %>%
  distinct(lemma, term, .keep_all = TRUE)

# extract lemmas with inflectional endings ===============================================
# select only terms that have inflectional endings (entries that differ in term and lemma)
# ensure that no term is a lemma and no lemma is a term
diff <- table_lemmas %>%
  filter(!lemma == term) %>% # select only entries with inflectional endings
  filter(!lemma %in% term) %>% # ensure that no lemma is a term
  filter(!term %in% lemma) %>% # ensure that no term is a lemma
  group_by(term) %>% filter(n() == 1) %>% ungroup() # select only terms with one lemma

# put in lemma for inflectional ending terms =============================================
# for all the terms in diff, put back the same lemma-term pair
diff.same <- bind_cols(diff %>% select(lemma), diff %>% select(term = lemma, origin)) %>%
  distinct()

# combine dataframes =====================================================================
diff <- bind_rows(diff, diff.same)

# select same term-lemma pairs ===========================================================
same <- table_lemmas %>% filter(lemma == term) %>%
  filter(!term %in% diff$term) # ensure that terms are not include in 'diff'

# combine dataframes =====================================================================
# first combine the 'same' and 'diff' dataframe, then append the own lemma list
# by appending the own lemma list it is ensured that all term-lemma pairs will end up in the
# final lemma list, even though they have been removed in the prior transformations due to
# term-lemma pair ambiguity
table_lemmas <- bind_rows(diff, same) %>% # combine 'diff' and 'same'
  bind_rows(lemma.own) %>% # combine own lemmas
  arrange(lemma, term, desc(origin)) %>% # arrange so that own lemmas are prioritized
  distinct(term, .keep_all = TRUE) # only keep unique terms

write_delim(table_lemmas, "data-raw/table_lemmas.csv", ";", "")

