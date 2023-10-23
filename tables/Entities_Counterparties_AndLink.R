filtered_m_CoOwners <- m.CoOwners %>%
  select(id.bor, cf.piva) %>%
  distinct() %>%
  anti_join(m.Borrower, by = c("id.bor", "cf.piva"))

staging_entities_counterparties <- bind_rows(
  mutate(m.Borrower %>% select(id.bor, cf.piva, type.subject, city, province), role = factor('borrower', levels = c('borrower', 'guarantor', 'other'))),
  mutate(filtered_m_CoOwners, role = factor('borrower', levels = c('borrower', 'guarantor', 'other'))),
  mutate(m.Guarantor, role = factor('guarantor', levels = c('borrower', 'guarantor', 'other')))
)
staging_entities_counterparties$cf.piva <- clean_cf.piva(staging_entities_counterparties$cf.piva)
staging_entities_counterparties <- add_type_subject_column(staging_entities_counterparties)
staging_entities_counterparties <- staging_entities_counterparties %>% distinct()

count_entities <- function(x) as.integer(str_count(x, ',') + 1)
# Group counterparties and create n.entities 
staging_entities_counterparties <- staging_entities_counterparties %>%
  group_by(id.bor, role) %>%
  summarize(
    cf.piva = paste(cf.piva, collapse = ","), 
    n.entities = count_entities(cf.piva),
    type.subject = first(type.subject),
    city = first(city),
    province = first(province),
    .groups = 'drop') 

staging_entities_counterparties$id.counterparty <- paste0("c", seq_len(nrow(staging_entities_counterparties)))    
staging_entities_counterparties <- divide_column_by_character(staging_entities_counterparties, cf.piva, ",")

staging_entities_counterparties <- staging_entities_counterparties %>%
  mutate(id.entity = group_indices(., coalesce(cf.piva, NA_character_), type.subject, city, province))
# In this code, we use coalesce() to replace NA values in the cf.piva column with a special "NA" character string to ensure rows with NA values in cf.piva will be assigned unique identifiers.

#Creation of the link.entities.counterparties table
link.counterparties.entities <- staging_entities_counterparties %>% select(id.counterparty,id.entity)

#Creating the counterparties table
COUNTERPARTIES <- staging_entities_counterparties %>% select (id.counterparty, id.bor, role, n.entities)
COUNTERPARTIES <- COUNTERPARTIES %>% distinct()
COUNTERPARTIES <- COUNTERPARTIES %>%
  mutate(
    name= NA,
    id.group = NA,
    flag.imputed = NA
  )

#Creation of the entities table
ENTITIES <- staging_entities_counterparties %>% select (id.entity, cf.piva, type.subject, city, province)
ENTITIES <- ENTITIES %>% distinct()

#age: number taken from the codice fiscale
ENTITIES <- add_age_column(ENTITIES)

# range-age= type:factor. Taken also from the CF
ENTITIES <- add_age_range_column(ENTITIES)

# sex= type: factor. Taken also from the CF
ENTITIES <- add_sex_column(ENTITIES)
ENTITIES$sex <- factor(ENTITIES$sex, levels = c('m', 'f'))

ENTITIES <- ENTITIES %>% mutate(
  name=NA,
  dummy.info=NA,
  solvency.pf=NA,
  income.pf=NA,
  type.pg=NA,
  date.cessation=NA,
  status.pg=NA,
  flag.imputed=NA
)

# Read the contents of the .paths file to get the location of the metadata file, to then read the Geo sheet
geoMetadataPath_line <- grep("^geoMetadataPath=", paths_content)
geoMetadataPath_value <- sub("^geoMetadataPath=\\s*", "", paths_content[geoMetadataPath_line])

GEO.metadata <- read_excel(geoMetadataPath_value, sheet = "Geo")
GEO.metadata <- GEO.metadata %>% mutate_all(tolower)

ENTITIES <- ENTITIES %>%
  mutate(city = case_when(
    city == "reggio di  calabria" ~ "reggio di calabria",
    city == "s benedetto del tronto" ~ "san benedetto del tronto",
    city == "quartu sant`elena" ~ "quartu sant'elena",
    city == "citta' sant angelo" ~ "citta' sant'angelo",
    city == "forlì" ~ "forli'",
    city == "reggio nell`emilia" ~ "reggio nell'emilia",
    city == "montecatini terme" ~ "montecatini-terme",
    
    TRUE ~ city  # Keep other values unchanged
  ))

ENTITIES <- ENTITIES %>% rename("or.province" =province)

# Merge specific columns from 'city_info' into 'ENTITIES'
ENTITIES <- ENTITIES %>%
  left_join(GEO.metadata %>% select(city, province, region, area), by = "city")

ENTITIES <- ENTITIES %>% distinct()
#This were the rows of the cities with repeated name
indices_to_delete <- which(ENTITIES$city == 'san teodoro' & ENTITIES$province == 'sassari')
ENTITIES <- ENTITIES[-indices_to_delete,]
#I select the columns in the order I want (the order in Metadata)
ENTITIES <- ENTITIES %>% select(id.entity, name, cf.piva, type.subject, dummy.info, sex, range.age, age, solvency.pf, income.pf, type.pg, status.pg, date.cessation, city, or.province, province, region, area, flag.imputed)