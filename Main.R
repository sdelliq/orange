source("Library.R")
source("Functions.R")

#Getting the path of the file with the portfolio
paths_content <- readLines(".paths.txt")
orangePath_line <- grep("^orangePortfolioPath=", paths_content)
orangePath_value <- sub("^orangePortfolioPath=\\s*", "", paths_content[orangePath_line])

DT <- read_doc_and_save_df(orangePath_value)

#Saving the relevant sheets as dataframes
original.BorrowerData <- DT$`Borrower Data` 
original.LoansData <- DT$`Loans Data` 
original.CoOwnersData <- DT$`Co-owners Data` 
original.GuarantorsData <- DT$`Guarantors data`
original.GuaranteesData <- DT$`Guarantees Data`

source("Cleaning.R")
source("tables/Loans.R")
# source("tables/Counterparties.R")
# source("tables/Entities.R")
# source("tables/link.counterparties.entities.R")
source("tables/Entities_Counterparties_AndLink.R")
source("tables/link.loans.counterparties.R")
