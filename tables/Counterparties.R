#Creating the borrowers table
borrowers.and.coOwners <- bind_rows(m.Borrower, m.CoOwners) %>% select(id.bor, cf.piva) %>% distinct()

#Creating the counterparties table
COUNTERPARTIES <- bind_rows(
  mutate(m.Guarantor %>% select(id.bor, cf.piva), role = factor('guarantor', levels = c('borrower', 'guarantor', 'other'))),
  mutate(borrowers.and.coOwners, role = factor('borrower', levels = c('borrower', 'guarantor', 'other')))
) 

count_entities <- function(x) as.integer(str_count(x, ',') + 1)
# Group counterparties and create n.entities 
COUNTERPARTIES <- COUNTERPARTIES %>%
  group_by(id.bor, role) %>%
    summarize(
      cf.piva = paste(cf.piva, collapse = ","), 
      n.entities = count_entities(cf.piva), .groups = 'drop') 
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