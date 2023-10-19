source("Library.R")
source("Functions.R")

#Getting the path of the file with the portfolio
paths_content <- readLines(".paths.txt")
orangePath_line <- grep("^orangePortfolioPath=", paths_content)
orangePath_value <- sub("^orangePortfolioPath=\\s*", "", paths_content[orangePath_line])

DT <- read_doc_and_save_df(orangePath_value)

#Saving the relevant sheets as dataframes
original.BorrowerData <- DT$`Borrower Data` %>% mutate_all(tolower)
original.LoansData <- DT$`Loans Data` %>% mutate_all(tolower)
original.CoOwnersData <- DT$`Co-owners Data` %>% mutate_all(tolower)
original.GuarantorsData <- DT$`Guarantors data`%>% mutate_all(tolower)

source("Cleaning.R")
source("tables/Loans.R")
# source("tables/Counterparties.R")
# source("tables/Entities.R")
# source("tables/link.counterparties.entities.R")
source("tables/Entities_Counterparties_AndLink.R")
source("tables/link.loans.counterparties.R")
