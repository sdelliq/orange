#Counterparties must be ran first

#Creates the dataframe with the id.bor and cf.piva column, with unique values
ENTITIES <- bind_rows(w_id.bor_cf.piva_borrower, w_id.bor_cf.piva_coOwner, w_id.bor_cf.piva_guarantor) %>% distinct()

# id_entity: character created as e1/e2/etc
ENTITIES$id.entity <- paste0("e", seq_len(nrow(ENTITIES)))

#Get more info from Borrowers
ENTITIES <- ENTITIES %>% left_join(original.BorrowerData %>% select(id.bor=NDG, type.subject=`Registry type`, city=`Borrower town`, or.province=`Borrower Province`), by= "id.bor")
ENTITIES <- ENTITIES %>% mutate_all(tolower)

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

#I first check if there are cities with the same name in different provinces/regions - I saved the result in a list, since I don't think it's worth re.running.
# repeated_cities <- GEO.metadata %>%
#   group_by(city) %>%
#   summarise(unique_provinces = n_distinct(province)) %>%
#   filter(unique_provinces > 1)

repeated_cities = c("calliano", "castro", "livo", "peglio", "samone", "san teodoro")
if(any(ENTITIES$city %in% repeated_cities)){
  print("You might have a city assigned to a province incorrectly. Check for: calliano, castro, livo, peglio, samone, san teodoro")
}

# Merge specific columns from 'city_info' into 'ENTITIES'
ENTITIES <- ENTITIES %>%
  left_join(GEO.metadata %>% select(city, province, region, area), by = c("city" = "city"))

#This were the cities with repeated name
ENTITIES <- ENTITIES %>%
  filter(!(city == "san teodoro" & province == "sassari"))

#It was only selected to check when duplicated
ENTITIES <- ENTITIES %>% select(!or.province)

ENTITIES$province <- factor(ENTITIES$province, levels = c(
  "abroad", "agrigento", "alessandria", "ancona", "aosta", "arezzo", "ascoli piceno", "asti", "avellino",
  "bari", "barletta andria trani", "belluno", "benevento", "bergamo", "biella", "bologna", "bolzano", "brescia",
  "brindisi", "cagliari", "caltanissetta", "campobasso", "caserta", "catania", "catanzaro", "chieti", "como", 
  "cosenza", "cremona", "crotone", "cuneo", "enna", "fermo", "ferrara", "firenze", "foggia", "forli' cesena", 
  "frosinone", "genova", "gorizia", "grosseto", "imperia", "isernia", "l'aquila", "la spezia", "latina", "lecce", 
  "lecco", "livorno", "lodi", "lucca", "macerata", "mantova", "massa carrara", "matera", "messina", "milano", 
  "modena", "monza brianza", "napoli", "novara", "nuoro", "oristano", "padova", "palermo", "parma", "pavia", 
  "perugia", "pesaro urbino", "pescara", "piacenza", "pisa", "pistoia", "pordenone", "potenza", "prato", "ragusa", 
  "ravenna", "reggio calabria", "reggio emilia", "rieti", "rimini", "roma", "rovigo", "salerno", "sassari", "savona", 
  "siena", "siracusa", "sondrio", "sud sardegna", "taranto", "teramo", "terni", "torino", "trapani", "trento", 
  "treviso", "trieste", "udine", "varese", "venezia", "verbano cusio ossola", "vercelli", "verona", "vibo valentia", 
  "vicenza", "viterbo"
))

ENTITIES$region <- factor(ENTITIES$region, levels =c(
  "abroad", "abruzzo", "basilicata", "calabria", "campania", "emilia romagna", "friuli venezia giulia", 
  "lazio", "liguria", "lombardia", "marche", "molise", "piemonte", "puglia", "sardegna", "sicilia", "toscana", 
  "trentino alto adige", "umbria", "valle d'aosta", "veneto"
))

ENTITIES$area <- factor(ENTITIES$area, levels =c(
  "abroad", "islands", "south", "center", "north-east", "north-west"
))



#I select the columns in the order I want (the order in Metadata)
ENTITIES <- ENTITIES %>% select(id.entity, name, cf.piva, type.subject, dummy.info, sex, range.age, age, solvency.pf, income.pf, type.pg, status.pg, date.cessation, city, province, region, area, flag.imputed)
