message(n_distinct(LOANS$id.bor))

# #Borrowers, #Loans, GBV(m), Average Borrower size(k), Average loan size (k)
r.introductionP6 <- LOANS %>% 
  summarise(
    '# Borrowers' = n_distinct(id.bor),
    '# Loans' = n_distinct(id.loan),
    'GBV (m)' = round(sum(gbv.residual) / 1e6, 1),  # Round GBV to 2 decimal places and convert to millions
    'Average Borrower size (k)' = round(sum(gbv.residual) / n_distinct(id.bor) / 1e3, 1),  # Round average borrower size to 2 decimal places and convert to thousands
    'Average loan size (k)' = round(sum(gbv.residual) / n_distinct(id.loan) / 1e3, 1)  # Round average loan size to 2 decimal places and convert to thousands
  )
  

italy_sf <- ENTITIES$area 
GBV_percentage_byArea <- ENTITIES %>% select(id.entity)

ggplot() +
  geom_sf(data = italy_sf, aes(fill = GBV_percentage), color = "white", size = 0.2) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "GBV%") +
  labs(title = "GBV Percentage by Region in Italy") +
  theme_minimal()