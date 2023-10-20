#Cleaning of DFs
original.BorrowerData <- original.BorrowerData %>% mutate_all(tolower)
original.LoansData <- original.LoansData %>% mutate_all(tolower)
original.CoOwnersData <- original.CoOwnersData %>% mutate_all(tolower)
original.GuarantorsData <- original.GuarantorsData %>% mutate_all(tolower)
colnames(original.GuaranteesData) <- clean_column_names(colnames(original.GuaranteesData))

#Deletes up to the 4th row and renames the columns. also deletes the 5th (name in italian) 
original.BorrowerData <- deleteXrowsAndRenameColumns(4, original.BorrowerData) #PK: NDG unique 100%
original.CoOwnersData <- deleteXrowsAndRenameColumns(4, original.CoOwnersData) # highest ratio Fiscal Code 98.07% - has NA
original.GuarantorsData <- deleteXrowsAndRenameColumns(4, original.GuarantorsData) # PK: Guarantor Id No. unique 100%
original.LoansData <- deleteXrowsAndRenameColumns(4, original.LoansData) #PK: NDG unique 100%
original.GuaranteesData <- deleteXrowsAndRenameColumns(4, original.GuaranteesData)

#Unify columns
m.Borrower <- original.BorrowerData %>% 
  mutate(cf.piva = coalesce(`fiscal code`, `vat number`)) %>% select (-c(`fiscal code`, `vat number`))
m.CoOwners <- w_id.bor_cf.piva_coOwner <- original.CoOwnersData %>% 
  mutate(cf.piva = coalesce(`co-owner fiscal code`, `co-owner vat number`)) %>% select (-c(`co-owner fiscal code`, `co-owner vat number`))
m.Guarantor <- original.GuarantorsData %>% 
  mutate(cf.piva = coalesce(`guarantor fiscal code`, `guarantor vat number`)) %>% select (-c(`guarantor fiscal code`, `guarantor vat number`))

#Clean col names
colnames(m.Borrower) <- clean_column_names(colnames(m.Borrower))
colnames(m.CoOwners) <- clean_column_names(colnames(m.CoOwners))
colnames(m.Guarantor) <- clean_column_names(colnames(m.Guarantor))
colnames(original.LoansData) <- clean_column_names(colnames(original.LoansData)) 


#Changing the names of all DFs (but Loans)
m.CoOwners <- m.CoOwners %>% rename("id.bor" = ndg)
m.Guarantor <- m.Guarantor %>% rename("id.bor" = guarantor.id.no)
m.Borrower <- m.Borrower %>% rename(id.bor=ndg, type.subject=`registry.type`, city=`borrower.town`, province=`borrower.province`)
