source("Functions_updated.R")
source("Library.R")


###------------------------------------------###
#---         Check primary keys       -----
###------------------------------------------###

possible_keys_LOANS <- detect_primary_keys_NAs_perc(LOANS,number_NAs=1,percentage=0.9)
possible_keys_COUNTERPARTIES <- detect_primary_keys_NAs_perc(COUNTERPARTIES,number_NAs=1,percentage=0.9)


dataframes_list <- list(LOANS, COUNTERPARTIES)
results_list <- lapply(dataframes_list, detect_primary_keys_NAs_perc, number_NAs = 1, percentage = 0.9)

# Print the results
for (i in seq_along(dataframes_list)) {
  cat("Primary keys for", names(dataframes_list)[i], ":\n")
  print(results_list[[i]])
  cat("\n")
}


#########################################
##---      Check dependencies Loans    ---##
#########################################
LOANS_na <- LOANS %>%  mutate(across(everything(), ~ifelse(is.na(.), "-", .)))
LOANS_matrix <- find_dependencies_matrix(LOANS_na)
LOANS_matrix_rounded <- round(LOANS_matrix,2)

# add a row with the sum of 1 (dependencies) for each column and order the matrix for better visualization:
LOANS_matrix_rounded  <- LOANS_matrix_rounded %>% as.data.frame()
column_sums_ones <- sapply(LOANS_matrix_rounded, function(col) sum(col == 1))

LOANS_matrix_rounded <- rbind(LOANS_matrix_rounded, column_sums_ones) 
rownames(LOANS_matrix_rounded)[19] <- 'Sum_ones'
LOANS_matrix_rounded <- LOANS_matrix_rounded[c(19, 1:18), ]

LOANS_matrix_ordered <- as.matrix(LOANS_matrix_rounded)
col_index <- order(LOANS_matrix_ordered[1,],  decreasing = TRUE)
LOANS_matrix_ordered <- LOANS_matrix_ordered[,col_index]


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
rownames(COUNTERPARTIES_matrix_rounded)[9] <- 'Sum_ones'
COUNTERPARTIES_matrix_rounded <- COUNTERPARTIES_matrix_rounded[c(9, 1:8), ]

COUNTERPARTIES_matrix_ordered <- as.matrix(COUNTERPARTIES_matrix_rounded)
col_index <- order(COUNTERPARTIES_matrix_ordered[1,],  decreasing = TRUE)
COUNTERPARTIES_matrix_ordered <- COUNTERPARTIES_matrix_ordered[,col_index]



#########################################
##---      Profiling     ---##
#########################################

#non numeric profile LOANS
Profile_LOANS <- ExpData(data=LOANS,type=2) %>% as.data.frame()
Profile_LOANS <- Profile_LOANS %>%
  filter(!grepl("numeric", Variable_Type, ignore.case = TRUE))

#numeric profile LOANS
Profile_Numeric <- ExpData(data=LOANS,type=2, fun = c("mean", "median", "var")) %>% as.data.frame()
Profile_Numeric<-Profile_Numeric[complete.cases(Profile_Numeric),]

# numeric variables plot (proportion of observations per money amount)
plot1 <- ExpNumViz(LOANS,target=NULL,nlim=10,Page=c(2,2),sample=4)
plot1[[1]]

#profile for COUNTERPARTIES
Profile_COUNTERPARTIES <- ExpData(data=COUNTERPARTIES,type=2) %>% as.data.frame()


#########################################
##---      charts and tables    ---##
#########################################

#summary table for totals
r.introductionP6 <- LOANS %>% 
  summarise(
    '# Borrowers' = n_distinct(id.bor),
    '# LOANS' = n_distinct(id.loan),
    'GBV (m)' = round(sum(gbv.residual) / 1e6, 1),  # Round GBV to 2 decimal places and convert to millions
    'Average Borrower size (k)' = round(sum(gbv.residual) / n_distinct(id.bor) / 1e3, 1),  # Round average borrower size to 2 decimal places and convert to thousands
    'Average loan size (k)' = round(sum(gbv.residual) / n_distinct(id.loan) / 1e3, 1)  # Round average loan size to 2 decimal places and convert to thousands
  )


#gbv ranges chart
gbv_ranges <- c(0, 5000, 10000, 25000, 50000, Inf)  # Define the age ranges
gbv_labels <- c("0-5k", "5k-10k", "10k-25k", "25k-50k", "+50k")  # Define labels for the ranges

#create range.gbv column, calculate the total gbv, create table with range.gbv and % of gbv per range
df <- LOANS
df$range.gbv <- cut(df$gbv.residual, breaks = gbv_ranges, labels = gbv_labels, include.lowest = TRUE)
total.gbv <- sum(df %>% group_by(range.gbv) %>% summarize(total = sum(gbv.residual)) %>% pull(total))
summar <- df %>% select(gbv.residual, range.gbv) %>% group_by(range.gbv) %>% 
  summarise("GBV %" = round(sum(gbv.residual) / total.gbv *100, 2))

chart_range_gbv <- ggplot(summar, aes(x = range.gbv, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "GBV Ranges", y = "GBV %", title= "GBV Residual % per GBV Range") + 
  geom_text(aes(label = `GBV %`), vjust = 2.5, size = 5)
chart_range_gbv





merged1 <- merge(link.counterparties.entities, 
                                   ENTITIES[,c("id.entity", "cf.piva", "area")], by = "id.entity", all.x= TRUE)

merged2 <- merge(merged1, 
                 COUNTERPARTIES[,c("id.counterparty", "role", "n.entities")], by = "id.counterparty", all.x= TRUE)

merged2 <- merged2 %>%
  filter(grepl("borrower", role, ignore.case = TRUE))

total.borrowers <- sum(merged2 %>% summarize(total = nrow(.)) %>% pull(total))
summar <- merged2 %>% select(id.entity, area) %>% group_by(area) %>% 
  summarise("Borrower %" = round( n_distinct(id.entity) / total.borrowers *100, 2))

chart_borr <- ggplot(summar, aes(x = range.gbv, y = `GBV %`)) +
  geom_bar(stat = 'identity', fill = "#71EAF7", alpha = 0.7) +
  labs(x = "GBV Ranges", y = "GBV %", title= "GBV Residual % per GBV Range") + 
  geom_text(aes(label = `GBV %`), vjust = 2.5, size = 5)
chart_borr








coordinates <- data.frame(
  region = c("lazio", "marche", "toscana", "umbria", "sardegna", "sicilia",
             "emilia romagna", "friuli venezia giulia", "trentino alto adige", "veneto",
             "liguria", "lombardia", "piemonte", "valle d'aosta", "abruzzo", 
             "basilicata", "calabria", "campania", "molise", "puglia"),
  latitude = c(41.9028, 43.6168, 43.7711, 42.9384, 39.2153, 37.5994,
               44.4944, 45.9636, 46.6062, 45.4349, 44.4072, 45.4642, 
               45.0735, 45.7386, 42.3512, 40.5735, 39.3088, 38.1157, 
               41.4635, 40.8518),
  longitude = c(12.4964, 13.5189, 11.2486, 12.5047, 9.1106, 14.0154,
                11.3426, 13.0439, 11.1228, 12.3385, 8.9334, 9.1900, 
                7.8479, 7.3206, 13.4075, 15.3714, 16.1910, 16.3297, 
                14.1900, 17.0465)
)



world <- map_data("world")
italy <- subset(world, region == "Italy")

# Create a ggplot with a map of Italy
italymap <- ggplot(italy, aes(x = long, y = lat, group = group)) +
  geom_path() +
  scale_x_continuous(limits = c(5, 20)) +  # Set longitude limits to focus on Italy
  scale_y_continuous(limits = c(35, 48))  +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black")

  italymap
  # Print the map of Italy
  
italy_map <- italymap + geom_polygon(data = coordinates, aes(x = long, y = lat, group = group))
  






