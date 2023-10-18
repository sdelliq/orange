link.counterparties.entities <- COUNTERPARTIES %>% 
  ungroup() %>% select (id.counterparty, cf.piva) 
link.counterparties.entities <- divide_column_by_character(link.counterparties.entities, cf.piva, ",")

link.counterparties.entities <- link.counterparties.entities %>% 
  left_join(ENTITIES %>% select(id.entity, cf.piva), by= c("cf.piva"))
link.counterparties.entities <- link.counterparties.entities %>% select(-cf.piva)
df_with_na <- link.counterparties.entities[apply(is.na(link.counterparties.entities), 1, any), ]


link.counterparties.entities <- link.counterparties.entities %>% select(-cf.piva)


