#Remember to run the cleaning first

#Creates the LOANS dataframe with the columns it should have and its corresponding types
LOANS <- original.LoansData %>% select (loan.id.no, ndg, type.of.product, loan.status, gross.book.value.a.b.c, principal.a, delay.compensation.b, collection.expenses.c, intrum.acquisition.date, date.of.default)

#Rename columns to make them match the name they're supposed to have (seen on the Metadata file)
LOANS <- LOANS %>% rename("id.bor" = ndg, "id.loan" = loan.id.no, "type" = type.of.product, "status" = loan.status, "gbv.residual" = gross.book.value.a.b.c, "principal" = principal.a, "penalties" = delay.compensation.b, "expenses" = collection.expenses.c, "date.origination" = intrum.acquisition.date, "date.status" = date.of.default)

#Creates columns filled with NAs and corresponding information
LOANS <- LOANS %>%
  mutate(
    id.group = NA,
    originator = "alicudi spv",
    ptf = "negroni",
    cluster.ptf = "orange",
    gbv.original = 0,
    date.last.act = date.status,
    flag.imputed = NA,
    interest=NA
  )

#Changing types to what they should be according to the metadata
LOANS <- LOANS %>%
  mutate(status = ifelse(status == "bad loan", "bad", status))
LOANS$status <- factor(LOANS$status, levels = c('utp', 'bad', 'bonis', 'past due'))
LOANS <- LOANS %>% mutate(across(c(id.bor, id.group, originator, ptf, cluster.ptf, type, status), as.character) )
LOANS <- LOANS %>% mutate(across(c(gbv.original, gbv.residual, principal, interest, penalties, expenses), as.numeric))
LOANS <- LOANS %>% mutate(flag.imputed= as.integer(flag.imputed))
LOANS <- LOANS %>% mutate(across(c(date.origination, date.status, date.last.act), convertToDate))

# #type: Other|Credit Cards|Bank Accounts|Personal Loans|Mortgages|Mortgages (Fondiario)
# distintos <- LOANS %>% distinct(type)
# 
# # Renames the type according to what it should be - personal opinion
# LOANS$type <- LOANS$type %>% lapply(function(x){
#   if(x == "conto corrente chiro"){
#     x <- 'Bank Accounts'
#   }
#   else if(x == "mutuo ipotecario"){
#     x <- 'Mortgages'
#   }
#   else if(x == "mutuo chiro" || x == "mutuo chirografario"){
#     x <- 'Personal Loans'
#   }
#   else if(x == "credito di firma"){
#     x <- 'Other'
#   }
#   else if(str_match(x, "fondiario")){
#     x <- 'Mortgages (Fondiario)'
#   }
#   else if(str_match(x, "credit card")){
#     x <- 'Credit Cards'
#   }
#   else {
#     x <- NA
#     print("check the type column for a missing classification")
#   }
# })
# LOANS$type <- factor(LOANS$type, levels = c('Other', 'Credit Cards', 'Bank Accounts', 'Personal Loans', 'Mortgages', 'Mortgages (Fondiario)'))

