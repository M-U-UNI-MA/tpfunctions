library(tidyverse)
# set internal functions =================================================================
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
lemma.ant <- rop_csv("data-raw/lemmas_ant.csv", NULL, "c") %>% mutate(origin = "ant")  %>%
  mutate(lemma = int_punct_complete(lemma), term = int_punct_complete(term))

lemma.own <- rop_csv("data-raw/lemmas_own.csv", NULL, "c") %>% mutate(origin = "own") %>%
  mutate(lemma = int_punct_complete(lemma), term = int_punct_complete(term))

table_lemmas <- bind_rows(lemma.own, lemma.ant) %>% distinct(lemma, term, .keep_all = TRUE)

# extract lemmas with inflectional endings ===============================================
# select only terms that have inflectional endings (entries that differ in term and lemma)
# ensure that no term is a lemma and no lemma is a term

amb <- table_lemmas %>%
  filter(!lemma == term) %>% # select only entries with inflectional endings
  filter(lemma %in% term |term %in% lemma)

table_lemmas <- table_lemmas %>%
  anti_join(amb) %>% bind_rows(lemma.own) %>% distinct()

diff <- table_lemmas %>%
  filter(!lemma == term) %>% # select only entries with inflectional endings
  distinct(term, .keep_all = TRUE)

# put in lemma for inflectional ending terms =============================================
# for all the terms in diff, put back the same lemma-term pair
diff.same <- bind_cols(diff %>% select(lemma), diff %>% select(term = lemma, origin)) %>%
  distinct()

# combine dataframes =====================================================================
diff <- bind_rows(diff, diff.same) %>% distinct()

# select same term-lemma pairs ===========================================================
same <- table_lemmas %>% filter(lemma == term) %>%
  filter(!term %in% diff$term) # ensure that terms are not include in 'diff'

# combine dataframes =====================================================================
table_lemmas <- bind_rows(diff, same) %>% # combine 'diff' and 'same'
  arrange(lemma, term, desc(origin)) %>% # arrange so that own lemmas are prioritized
  distinct(term, .keep_all = TRUE) # only keep unique terms

write_delim(table_lemmas, "data-raw/table_lemmas.csv", ";", "")

