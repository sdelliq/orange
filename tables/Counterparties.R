#Deletes up to the 4th row and renames the columns. also deletes the 5th (name in italian) 
original.BorrowerData <- deleteXrowsAndRenameColumns(4, original.BorrowerData) #PK: NDG unique 100%
original.CoOwnersData <- deleteXrowsAndRenameColumns(4, original.CoOwnersData) # highest ratio Fiscal Code 98.07% - has NA
original.GuarantorsData <- deleteXrowsAndRenameColumns(4, original.GuarantorsData) # PK: Guarantor Id No. unique 100%
original.GuaranteesData <- deleteXrowsAndRenameColumns(4, original.GuaranteesData) #highest ratio Guarantee Id No. 82.68% - no NAs

#Creating the borrower, coOwner and guarantor tables only with id.bor and cf.piva
w_id.bor_cf.piva_borrower <- original.BorrowerData %>% 
  mutate(cf.piva = coalesce(`fiscal code`, `vat number`)) %>% select (id.bor = ndg, cf.piva)
w_id.bor_cf.piva_coOwner <- original.CoOwnersData %>% 
  mutate(cf.piva = coalesce(`co-owner fiscal code`, `co-owner vat number`)) %>% select (id.bor = ndg, cf.piva)
w_id.bor_cf.piva_guarantor <- original.GuarantorsData %>% 
  mutate(cf.piva = coalesce(`guarantor fiscal code`, `guarantor vat number`)) %>% select (id.bor = `guarantor id no.`, cf.piva)

#Creating the borrowers table
borrowers.and.coOwners <- bind_rows(w_id.bor_cf.piva_borrower, w_id.bor_cf.piva_coOwner) %>% distinct()

#Creating the counterparties table
COUNTERPARTIES <- bind_rows(
  mutate(w_id.bor_cf.piva_guarantor, role = factor('guarantor', levels = c('borrower', 'guarantor', 'other'))),
  mutate(borrowers.and.coOwners, role = factor('borrower', levels = c('borrower', 'guarantor', 'other')))
)

count_entities <- function(x) as.integer(str_count(x, ',') + 1)
# Group counterparties and create n.entities 
COUNTERPARTIES <- COUNTERPARTIES %>%
  group_by(id.bor, role) %>%
    summarize(cf.piva = paste(cf.piva, collapse = ","), n.entities = count_entities(cf.piva))
#Adding the id.counterparty column
COUNTERPARTIES$id.counterparty <- paste0("c", seq_len(nrow(COUNTERPARTIES)))    

COUNTERPARTIES <- COUNTERPARTIES %>%
  mutate(
    name= NA,
    id.group = NA,
    flag.imputed = NA
  )



#I select the columns in the order I want them to be (according to the Metadata) 
COUNTERPARTIES <- COUNTERPARTIES %>% select (id.counterparty, id.bor, id.group, role, name, n.entities, flag.imputed, cf.piva) #I'll delete the cf.piva later


# match_counts <- table(!is.na(match(w_id.bor_cf.piva_coOwner$id.bor, w_id.bor_cf.piva_borrower$id.bor)))
# print(match_counts)
      