###-----------------------------------------------------------------------###
#-----                     Page 6 report                                 -----         
###-----------------------------------------------------------------------###

# #Borrowers, #Loans, GBV(m), Average Borrower size(k), Average loan size (k)
r.p6.introduction <- LOANS %>% 
  summarise(
    '# Borrowers' = n_distinct(id.bor),
    '# Loans' = n_distinct(id.loan),
    'GBV (m)' = round(sum(gbv.residual) / 1e6, 1),  # Round GBV to 2 decimal places and convert to millions
    'Average Borrower size (k)' = round(sum(gbv.residual) / n_distinct(id.bor) / 1e3, 1),  # Round average borrower size to 2 decimal places and convert to thousands
    'Average loan size (k)' = round(sum(gbv.residual) / n_distinct(id.loan) / 1e3, 1)  # Round average loan size to 2 decimal places and convert to thousands
  )
  

###-----------------------------------------------------------------------###
#-----                     Page 27 map                                   -----         
###-----------------------------------------------------------------------###
italy_sf <- ENTITIES$area 
GBV_percentage_byArea <- ENTITIES %>% select(id.entity, area) %>%
  left_join(link.counterparties.entities, by = "id.entity") %>%
  left_join(link.loans.counterparties, by = "id.counterparty") %>%
  left_join(LOANS %>% select(id.loan, gbv.residual), by = "id.loan") %>%
  group_by(area) %>%
  summarise(gbv.residual = sum(gbv.residual, na.rm = TRUE))
GBV_percentage_byArea <- transform(GBV_percentage_byArea, gbv_percentage = gbv.residual / sum(gbv.residual, na.rm = TRUE) * 100)

ggplot(GBV_percentage_byArea, aes(x = region, y = gbv_percentage, fill = area)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "GBV Percentage by Area", x = "Area", y = "GBV Percentage") +
  theme_minimal()



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

# Merge the coordinates with your original data
GBV_percentage_byArea <- merge(GBV_percentage_byArea, coordinates, by = "region", all.x = TRUE)
GBV_na <- GBV_percentage_byArea[!complete.cases(GBV_percentage_byArea$longitude, GBV_percentage_byArea$latitude), ]
GBV_percentage_byArea <- GBV_percentage_byArea[complete.cases(GBV_percentage_byArea$longitude, GBV_percentage_byArea$latitude), ]

sf_data <- st_as_sf(GBV_percentage_byArea, coords = c("longitude", "latitude"), crs = 4326)

# Plot the map
plot(sf_data["gbv.residual"])

ggplot() +
  geom_sf(data = sf_data, aes(fill = gbv.residual / sum(gbv.residual, na.rm = TRUE))) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "GBV %") +
  labs(title = "GBV Percentage by Region")








###-----------------------------------------------------------------------###
#-----                     Page 27 second graph                        -----         
###-----------------------------------------------------------------------###
Borrowers <- COUNTERPARTIES %>% filter(role=='borrower') %>% distinct()
Borrowers <- left_join(Borrowers,LOANS, by = "id.bor",relationship = "many-to-many")
Borrowers <- left_join(Borrowers,link.counterparties.entities,by = "id.counterparty",relationship = "many-to-many")
Borrowers <- left_join(Borrowers,ENTITIES, by = "id.entity")

# Borrowers_area_table <- Borrowers_area %>% mutate(area = ifelse(area == "ISLANDS", "SOUTH", area)) %>% 
#   group_by(area)  %>%
#   summarise(perc_borrowers = n_distinct(id.counterparty)/total_borrowers,
#             perc_gbv = sum(gbv.original)/total_gbv)

r.p27.borrowersByProvince <- Borrowers %>% select(id.counterparty,gbv.residual,or.province) %>% distinct() %>% 
  group_by(or.province) %>% 
    summarise(sum_gbv = round(sum(gbv.residual)/ 1e6, 1), 
              N_borr = n_distinct(id.counterparty), 
              avg_size = round( sum(gbv.residual)/N_borr / 1e3, 1)) %>% 
      arrange(desc(sum_gbv))
# the top 5 are Roma (rm), Teramo(te), Pescara(pe) ,Milano (mi), Genova (ge)
r.p27.borrowersByProvince.head <- head(r.p27.borrowersByProvince, 5)



