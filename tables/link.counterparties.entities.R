link.counterparties.entities <- COUNTERPARTIES %>% select (id.counterparty, cf.piva) 
link.counterparties.entities <- divide_column_by_character(link.counterparties.entities, cf.piva, ",")
link.counterparties.entities <- link.counterparties.entities %>% 
  left_join(ENTITIES %>% select(cf.piva, id.entity), by= "cf.piva")
link.counterparties.entities <- link.counterparties.entities %>% select(-cf.piva)
COUNTERPARTIES <- COUNTERPARTIES %>% select(-cf.piva)