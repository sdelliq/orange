source("Functions_updated.R")
source("Library.R")

###------------------------------------------###
#---   test loans-counterparties link      -----
###------------------------------------------###
#i separate the counterparties table into two according to the role (borrower, guarantor)
TRY <- COUNTERPARTIES
TRY_bor <- TRY
TRY_gua <- TRY
TRY_bor <- TRY_bor %>%
  filter(role == "borrower")
TRY_gua <- TRY_gua %>%
  filter(role == "guarantor")
TRY_gua <- TRY_gua %>% rename("id.guar" ="id.bor")

#i merge the guarantor part with the table guarantees to get the id.loan associated to guarantors
GUARANTEES <- original.GuaranteesData %>% rename("id.loan" = 'loan id no.', "id.guar" ="guarantor id no.")

merge_guarantees <- merge(TRY_gua, 
                          GUARANTEES[,c("id.guar", "id.loan")], by = "id.guar", all.x= TRUE)
merge_guarantees <- merge_guarantees %>% rename("id.bor" ="id.guar")

#i merge the borrower part with the loans table through the id.bor to get the id.loan for the borrowers
merge_borr <- merge(TRY_bor, 
                          LOANS[,c("id.bor", "id.loan")], by = "id.bor", all.x= TRUE)

#i merge the two parts and do distinct to avoud duplicates
merge_guar_borr <- merge_borr %>% rbind(merge_guarantees)
link_count_loans <- merge_guar_borr %>% select(id.counterparty, id.loan)
link.counterparties.loans <- link_count_loans %>% distinct()




###------------------------------------------###
#---         Check primary keys       -----
###------------------------------------------###

possible_keys_LOANS <- possiblePKs(LOANS)
possible_keys_COUNTERPARTIES <- possiblePKs(COUNTERPARTIES)
print(possible_keys_LOANS)
print(possible_keys_COUNTERPARTIES)

possible_keys_LOANS <- possible_keys_LOANS %>% rename("NA Presence" = "Has_NA")
possible_keys_COUNTERPARTIES <- possible_keys_COUNTERPARTIES %>% rename("NA Presence" = "Has_NA")

#########################################
##---      Check dependencies Loans    ---##
#########################################

LOANS_matrix <- functionalDependencies_Matrix(LOANS)

#########################################
##---  Check dependencies COUNTERPARTIES  ---##
#########################################
COUNTERPARTIES_na <- COUNTERPARTIES %>%  mutate(across(everything(), ~ifelse(is.na(.), "-", .)))
COUNTERPARTIES_matrix <- find_dependencies_matrix(COUNTERPARTIES_na)
COUNTERPARTIES_matrix_rounded <- round(COUNTERPARTIES_matrix,2)

# add a row with the sum of 1 (dependencies) for each column and order the matrix for better visualization:
COUNTERPARTIES_matrix_rounded  <- COUNTERPARTIES_matrix_rounded %>% as.data.frame()
column_sums_ones <- sapply(COUNTERPARTIES_matrix_rounded, function(col) sum(col == 1))

COUNTERPARTIES_matrix_rounded <- rbind(COUNTERPARTIES_matrix_rounded, column_sums_ones) 
rownames(COUNTERPARTIES_matrix_rounded)[8] <- 'Sum_ones'
COUNTERPARTIES_matrix_rounded <- COUNTERPARTIES_matrix_rounded[c(8, 1:7), ]

COUNTERPARTIES_matrix_ordered <- as.matrix(COUNTERPARTIES_matrix_rounded)
col_index <- order(COUNTERPARTIES_matrix_ordered[1,],  decreasing = TRUE)
COUNTERPARTIES_matrix_ordered <- COUNTERPARTIES_matrix_ordered[,col_index]



#########################################
##---      Profiling     ---##
#########################################
#abbellire i profiles
#non numeric profile LOANS
Profile_LOANS <- ExpData(data=LOANS,type=2) %>% as.data.frame()
Profile_LOANS <- Profile_LOANS %>%
  filter(!grepl("numeric", Variable_Type, ignore.case = TRUE))
Profile_LOANS <- Profile_LOANS %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                          "N Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                         "% NAs" ="Per_of_Missing", "N Classes" ="No_of_distinct_values")
Profile_LOANS$`% NAs` <- paste0(Profile_LOANS$`% NAs` *100, "%")
Profile_LOANS <- Profile_LOANS %>% select(-NAs)

#numeric profile LOANS
Profile_Numeric <- ExpData(data=LOANS,type=2, fun = c("mean", "median", "var")) %>% as.data.frame()
Profile_Numeric<-Profile_Numeric[complete.cases(Profile_Numeric),]
Profile_Numeric <- Profile_Numeric %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                          "N Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                          "% NAs" ="Per_of_Missing", "N Classes" ="No_of_distinct_values",
                                          "Mean (k)" = "mean", "Median (k)" = "median", "Var (M)" = "var")
Profile_Numeric$`% NAs` <- paste0(Profile_Numeric$`% NAs` *100, "%")
Profile_Numeric$`Mean (k)` <- round(Profile_Numeric$`Mean (k)`/1000, 2)
Profile_Numeric$`Median (k)` <- round(Profile_Numeric$`Median (k)`/1000, 2)
Profile_Numeric$`Var (M)` <- round(Profile_Numeric$`Var (M)`/1000000, 2)
Profile_Numeric <- Profile_Numeric %>% select(-NAs)


# numeric variables plot (proportion of observations per money amount)
plot1 <- ExpNumViz(LOANS,target=NULL,nlim=10,Page=c(2,2),sample=4)
plot1[[1]]

#profile for COUNTERPARTIES
Profile_COUNTERPARTIES <- ExpData(data=COUNTERPARTIES,type=2) %>% as.data.frame()
Profile_COUNTERPARTIES <- Profile_COUNTERPARTIES %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                              "N Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                              "% NAs" ="Per_of_Missing", "N Classes" ="No_of_distinct_values")
Profile_COUNTERPARTIES$`% NAs` <- paste0(Profile_COUNTERPARTIES$`% NAs` *100, "%")


#########################################
##---      charts and tables    ---##
#########################################

###-----------------------------------------------------------------------###
#-----                     Page 6 report                                 -----         
###-----------------------------------------------------------------------###
#summary table for totals
r.introductionP6 <- LOANS %>% 
  summarise(
    '# Borrowers' = n_distinct(id.bor),
    '# LOANS' = n_distinct(id.loan),
    'GBV (m)' = round(sum(gbv.residual) / 1e6, 1),  # Round GBV to 2 decimal places and convert to millions
    'Average Borrower size (k)' = round(sum(gbv.residual) / n_distinct(id.bor) / 1e3, 1),  # Round average borrower size to 2 decimal places and convert to thousands
    'Average loan size (k)' = round(sum(gbv.residual) / n_distinct(id.loan) / 1e3, 1)  # Round average loan size to 2 decimal places and convert to thousands
  )

###-----------------------------------------------------------------------###
#-----                     Page 27 reports                                -----         
###-----------------------------------------------------------------------###
Borrowers <- COUNTERPARTIES %>% filter(role=='borrower') %>% distinct()
Borrowers <- left_join(Borrowers,LOANS, by = "id.bor",relationship = "many-to-many")
Borrowers <- left_join(Borrowers,link.counterparties.entities,by = "id.counterparty",relationship = "many-to-many")
Borrowers <- left_join(Borrowers,ENTITIES, by = "id.entity")

#% Borrowers by areas 
total.gbv <- sum(LOANS$gbv.residual)
r.p27.geographicalDistribution <- Borrowers %>%
  mutate(area = ifelse(area %in% c("islands", "south"), "south and islands", area)) %>%
  select(id.entity, area, gbv.residual) %>%
  group_by(area) %>%
  summarise("Borrower %" = round(n_distinct(id.entity) / total.borrowers * 100, 1),
            "GBV %" = round(sum(gbv.residual) / total.gbv * 100, 1)
            )
r.p27.borrowerByArea <- ggplot(r.p27.geographicalDistribution, aes(x = area, y = `Borrower %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "Area", y = "Borrowers %", title= "Borrowers % per Area") + 
  geom_text(aes(label = `Borrower %`), vjust = 2.5, size = 5)
r.p27.borrowerByArea

r.p27.gbvByArea <- ggplot(r.p27.geographicalDistribution, aes(x = area, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "Area", y = "GBV %", title= "GBV % per Area") + 
  geom_text(aes(label = `GBV %`), vjust = 2.5, size = 5)
r.p27.gbvByArea

#GBV by borrower province
r.p27.borrowersByProvince <- Borrowers %>% select(id.counterparty,gbv.residual,or.province) %>% distinct() %>% 
  group_by(or.province) %>% 
  summarise(sum_gbv = round(sum(gbv.residual)/ 1e6, 1), 
            N_borr = n_distinct(id.counterparty), 
            avg_size = round( sum(gbv.residual)/N_borr / 1e3, 1)) %>% 
  arrange(desc(sum_gbv))
# the top 5 are Roma (rm), Teramo(te), Pescara(pe) ,Milano (mi), Genova (ge)
r.p27.borrowersByProvince.head <- head(r.p27.borrowersByProvince, 5)



###-----------------------------------------------------------------------###
#-----                     Page 28 report                                -----         
###-----------------------------------------------------------------------###

#gbv ranges chart
gbv_ranges <- c(0, 5000, 10000, 25000, 50000, Inf)  # Define the age ranges
gbv_labels <- c("0-5k", "5k-10k", "10k-25k", "25k-50k", "+50k")  # Define labels for the ranges

#create range.gbv column, calculate the total gbv, create table with range.gbv and % of gbv per range
df <- LOANS
df$range.gbv <- cut(df$gbv.residual, breaks = gbv_ranges, labels = gbv_labels, include.lowest = TRUE)
total.gbv <- sum(df %>% group_by(range.gbv) %>% summarize(total = sum(gbv.residual)) %>% pull(total))
r.p28.gbvByLoanSize <- df %>% select(gbv.residual, range.gbv) %>% group_by(range.gbv) %>% 
  summarise("GBV %" = round(sum(gbv.residual) / total.gbv *100, 1))

r.p28.g.gbvByLoanSize <- ggplot(r.p28.gbvByLoanSize, aes(x = range.gbv, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "GBV Ranges", y = "GBV %", title= "GBV Residual % by Loan Size") + 
  geom_text(aes(label = `GBV %`), vjust = 2.5, size = 5)
r.p28.g.gbvByLoanSize


###-----------------------------------------------------------------------###
#-----                     Page 29 report                                -----         
###-----------------------------------------------------------------------###
#gbv by default vintage
bv_ranges <- c(0, 8, 11, 14, 17, 20, Inf)  # Define the age ranges
gbv_labels <- c("0-8 years", "9-11 years", "12-14 years", "15-17 years", "18-20 years", "+20 years")  # Define labels for the ranges

df <- LOANS
df <- df %>%
  mutate(vintage = floor(as.numeric(interval(date.status, as.Date("2021-11-30")) / dyears(1))))
df$range.date <- cut(df$vintage, breaks = gbv_ranges, labels = gbv_labels, include.lowest = TRUE)
r.p29.gbvByDefaultVintage <- df %>% select(gbv.residual, range.date) %>% group_by(range.date) %>% 
  summarise("GBV Total (m)" = round(sum(gbv.residual)/1e6,1),
            "GBV %" = round(sum(gbv.residual) / total.gbv *100, 1),
            "Loans %" = round(n() / nrow(df) * 100, 1),
            "Avg. Loan size (€k)" = round(sum(gbv.residual)/n()/1e3,1))

r.p29.g.gbvByDefaultVintage <- ggplot(r.p29.gbvByDefaultVintage, aes(x = range.date, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  geom_text(aes(label = paste0(`GBV %`, "%"), vjust = -0.1), size = 5) +
  labs(x = "Year Ranges", y = "GBV %", title = "GBV By Default Vintage") +
  ylim(0, 32) +
  annotate("text", x = 1:6, y = rep(0, 6), label = paste0(r.p29.gbvByDefaultVintage$`GBV Total (m)`, "M"), size = 5, hjust = 0.5, vjust = -25, color = "#054A53")
r.p29.g.gbvByDefaultVintage

r.p29.g.loansByDefaultVintage <- ggplot(r.p29.gbvByDefaultVintage, aes(x = range.date, y = `Loans %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  geom_text(aes(label = paste0(`GBV %`, "%"), vjust = -0.1), size = 5) +
  labs(x = "Year Ranges", y = "Loans %", title = "Loans By Default Vintage", subtitle = "and avg. loan size") +
  ylim(0, 32) +
  annotate("text", x = 1:6, y = rep(0, 6), label = paste0(r.p29.gbvByDefaultVintage$`Avg. Loan size (€k)`, "€k"), size = 5, hjust = 0.5, vjust = -24.5, color = "#054A53")
r.p29.g.loansByDefaultVintage


###-----------------------------------------------------------------------###
#-----                     Page 31 report                                -----         
###-----------------------------------------------------------------------###
df <- merge(LOANS, original.GuaranteesData[,c("id.loan", "guarantee.type")], by = "id.loan", all.x= TRUE)
df <- df %>%
  distinct(id.loan, .keep_all = TRUE)
df$guarantee.presence <- ifelse(!is.na(df$guarantee.type), "guaranteed", "not guaranteed")

r.p31.gbvByGuaranteePresence <- df %>%
  select(gbv.residual, guarantee.presence) %>%
  group_by(guarantee.presence) %>%
  summarise("GBV %" = round(sum(gbv.residual) / total.gbv * 100, 2))

#gbv % per guaranteed and no guaranteed
custom_colors <- c("#054A53", "#ACDBE1")
r.p31.g.gbvByGuaranteePresence <- ggplot(r.p31.gbvByGuaranteePresence, aes(x = "", y = `GBV %`, fill = guarantee.presence)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "Guarantee Presence") +
  theme_void() +
  geom_text(aes(label = paste0(`GBV %`, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = custom_colors)
r.p31.g.gbvByGuaranteePresence


#gbv % per guarantee type chart
df1 <- df %>%
  filter(guarantee.presence == "guaranteed")
total.guaranteed.gbv <- sum(df1$gbv.residual)
r.p31.gbvByGuaranteedType <- df1 %>% select(gbv.residual, guarantee.type) %>% group_by(guarantee.type) %>% 
  summarise("GBV %" = round(sum(gbv.residual) / total.guaranteed.gbv *100, 2))

r.p31.g.gbvByGuaranteedType <- ggplot(r.p31.gbvByGuaranteedType, aes(x = guarantee.type, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "Guarantee Type", y = "GBV %", title= "GBV Residual % per Guarantee Type") + 
  geom_text(aes(label = `GBV %`), vjust = 1, size = 5)
r.p31.g.gbvByGuaranteedType
#custom_colors <- c("#054A53", "#ACDBE1", "#008197", "#28464B", "#003A44", "#595F60")