#example of dataframe for testing
#data <- data.frame(
#  Name = c("Alice", "Bob", "Charlie", "David"),
#  Age = c(25, 30, 28, 22),
#  Score = c(95, 88, 92, 78000))

# Create a workbook and add a worksheet
wb <- createWorkbook()
sheet <- addWorksheet(wb, sheetName = "Report_Loans", gridLines = FALSE)
sheetname <- "Report_Loans"

# Create styles for the title, header row, even rows, and odd rows, and for numeric values to set as k
ReportTitleStyle <- createStyle(fontSize = 20,textDecoration = c('bold','underline'))
titleStyle <- createStyle(fontSize = 14, textDecoration = "bold", border = "Bottom", borderColour = "black")
header_style <- createStyle(fontColour = "white", fgFill = "#6495ED", border = "TopBottom",
                            textDecoration = "bold", fontSize = 12)
even_row_style <- createStyle(fontSize = 12, fgFill = "#B9DDF1")
odd_row_style <- createStyle(fontSize = 12, fgFill = "#DFEBF4")
custom_format <- createStyle(numFmt = "0.0,\"k\"")


###------------------------------------------###
#---function to create table with k for miles-----
###------------------------------------------###
create_table_k_for_miles <- function(workbook, sheetname, table, titolo, col_iniziale, row_iniziale) {
  nrows <- nrow(table)
  ncols <- ncol(table)
  
  writeData(workbook, sheetname, x = titolo, startCol = col_iniziale, startRow = row_iniziale -1)
  addStyle(workbook, sheet = sheetname, style = titleStyle, rows = row_iniziale -1, cols = col_iniziale)
  
  writeDataTable(workbook, sheet = sheetname, x = table, startCol = col_iniziale, startRow = row_iniziale)
 
  for (col in 1:ncols) {
    addStyle(workbook, sheet = sheetname, style = header_style, rows = row_iniziale, cols = col + col_iniziale - 1)
    for (row in 2:(nrows + 1)) {
      cell_value <- table[row - 1, col]
      if (is.numeric(cell_value) && cell_value >= 1000) {
        addStyle(workbook, sheet = sheetname, style = custom_format, rows = row + row_iniziale - 1, cols = col + col_iniziale - 1, stack = TRUE)
      }
      if (row %% 2 == 0) {
        addStyle(workbook, sheet = sheetname, style = even_row_style, rows = row + row_iniziale - 1, cols = col + col_iniziale - 1, stack = TRUE)
      } else {
        addStyle(workbook, sheet = sheetname, style = odd_row_style, rows = row + row_iniziale - 1, cols = col + col_iniziale - 1, stack = TRUE)
      }
    }
    setColWidths(workbook, sheet = sheetname, cols = col + col_iniziale - 1, widths = "auto")
  }
}


###------------------------------------------###
#--- function to create table without k -----
###------------------------------------------###
create_table_no_k <- function(workbook, sheetname, table, titolo, col_iniziale, row_iniziale) {
  nrows <- nrow(table)
  ncols <- ncol(table)
  
  writeData(workbook, sheetname, x = titolo, startCol = col_iniziale, startRow = row_iniziale -1)
  addStyle(workbook, sheet = sheetname, style = titleStyle, rows = row_iniziale -1, cols = col_iniziale)
  
  writeDataTable(workbook, sheet = sheetname, x = table, startCol = col_iniziale, startRow = row_iniziale)
  
  for (col in 1:ncols) {
    addStyle(workbook, sheet = sheetname, style = header_style, rows = row_iniziale, cols = col + col_iniziale - 1)
    for (row in 2:(nrows + 1)) {
      cell_value <- table[row - 1, col]
      if (row %% 2 == 0) {
        addStyle(workbook, sheet = sheetname, style = even_row_style, rows = row + row_iniziale - 1, cols = col + col_iniziale - 1, stack = TRUE)
      } else {
        addStyle(workbook, sheet = sheetname, style = odd_row_style, rows = row + row_iniziale - 1, cols = col + col_iniziale - 1, stack = TRUE)
      }
    }
    setColWidths(workbook, sheet = sheetname, cols = col + col_iniziale - 1, widths = "auto")
  }
}

wb <- createWorkbook()
sheet <- addWorksheet(wb, sheetName = "Report_Loans", gridLines = FALSE)
sheetname <- "Report_Loans"

writeData(wb,"Report_Loans",'Loans Data Report',1,1)
addStyle(wb,sheet = "Report_Loans",style = ReportTitleStyle,1,1)

resume <- 'SUMMARY: \n The portfolio Orange (cluster of Project Negroni) involves +13k borrowers for a total of  +177M. \n In the first sheet we have the profile and analysis on Loan level.\n In the second sheet we have profile and analysis on Borrower level.'
lines <- unlist(strsplit(resume, "\n"))
df <- data.frame(Text = lines)
for(i in 1:nrow(df)){
  writeData(wb,"Report_Loans",df$Text[i],1,i+1)
}



#apply the functions as follows:   workbook, "sheetname", dataframe, "table title", ncol, nrow
#create_table_k_for_miles(wb, "Report", r.introductionP6, "Summary", 3, 5)
create_table_no_k(wb, "Report_Loans", r.introductionP6, "Summary", 3, 8)
create_table_no_k(wb, "Report_Loans", possible_keys_LOANS, "Possible Keys", 3, 12)
create_table_no_k(wb, "Report_Loans", Profile_LOANS, "Profile Non-numeric", 3, 19)
create_table_no_k(wb, "Report_Loans", Profile_Numeric, "Profile Numeric", 3, 35)

ggsave("Charts/gbv_residual_%_loan_size.png",plot = r.p28.g.gbvByLoanSize)
insertImage(wb,sheet = "Report_Loans","Charts/gbv_residual_%_loan_size.png",startCol = 10, startRow = 8, width = 6, height = 4.5, dpi = 300)
ggsave("Charts/gbv_by_default_vintage.png",plot = r.p29.g.gbvByDefaultVintage)
insertImage(wb,sheet = "Report_Loans","Charts/gbv_by_default_vintage.png",startCol = 3, startRow = 45, width = 6, height = 4.5, dpi = 300)
ggsave("Charts/loan_by_default_vintage.png",plot = r.p29.g.loansByDefaultVintage)
insertImage(wb,sheet = "Report_Loans","Charts/loan_by_default_vintage.png",startCol = 7, startRow = 45, width = 6, height = 4.5, dpi = 300)
ggsave("Charts/guarantee_presence.png",plot = r.p31.g.gbvByGuaranteePresence)
insertImage(wb,sheet = "Report_Loans","Charts/guarantee_presence.png",startCol = 3, startRow = 70, width = 6, height = 4.5, dpi = 300)
ggsave("Charts/gbv_%_guarantee_type.png",plot = r.p31.g.gbvByGuaranteedType)
insertImage(wb,sheet = "Report_Loans","Charts/gbv_%_guarantee_type.png",startCol = 7, startRow = 70, width = 6, height = 4.5, dpi = 300)




sheet <- addWorksheet(wb, sheetName = "Report_Borrowers", gridLines = FALSE)
sheetname <- "Report_Borrowers"
writeData(wb,"Report_Borrowers",'Borrowers Data Report',1,1)
addStyle(wb,sheet = "Report_Borrowers",style = ReportTitleStyle,1,1)

#apply the functions as follows:   workbook, "sheetname", dataframe, "table title", ncol, nrow
#create_table_k_for_miles(wb, "Report", r.introductionP6, "Summary", 3, 5)
create_table_no_k(wb, "Report_Borrowers", possible_keys_COUNTERPARTIES, "Possible Keys", 3, 6)
create_table_no_k(wb, "Report_Borrowers", Profile_COUNTERPARTIES, "Profile", 3, 12)
create_table_no_k(wb, "Report_Borrowers", r.p27.borrowersByProvince.head.support.table, "Borrowers by Province", 10, 48)

ggsave("Charts/borrowers_%_per_area.png",plot = r.p27.borrowerByArea)
insertImage(wb,sheet = "Report_Borrowers","Charts/borrowers_%_per_area.png",startCol = 3, startRow = 23, width = 6, height = 4.5, dpi = 300)
ggsave("Charts/gbv_%_per_area.png",plot = r.p27.gbvByArea)
insertImage(wb,sheet = "Report_Borrowers","Charts/gbv_%_per_area.png",startCol = 10, startRow = 23, width = 6, height = 4.5, dpi = 300)
ggsave("Charts/gbv_%_per_province.png",plot = r.p27.borrowersByProvince.top.5)
insertImage(wb,sheet = "Report_Borrowers","Charts/gbv_%_per_province.png",startCol = 3, startRow = 46, width = 6, height = 4.5, dpi = 300)


saveWorkbook(wb, "Report.xlsx", overwrite = TRUE)
