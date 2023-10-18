link.loans.counterparties <- COUNTERPARTIES %>% ungroup() %>% select (id.counterparty, id.bor) 
link.loans.counterparties <- link.loans.counterparties %>% 
  inner_join(LOANS %>% select(id.loan, id.bor), by= "id.bor")
link.loans.counterparties <- link.loans.counterparties %>% select(-id.bor)