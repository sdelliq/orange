#Create a function to check which cf.piva are not of 11 (piva) or 16 (cf)
check_errors_cf.piva <- ENTITIES %>% select (id.entity, cf.piva) %>% filter(!(str_length(cf.piva) == 11 | str_length(cf.piva) == 16))
check_errors_cf.piva <- check_errors_cf.piva %>% mutate (lunghezza = str_length(cf.piva))

#See how many borrowers are over 95
check_high_ages <- ENTITIES %>% select (id.entity, age) %>% filter(age>95)

#Cities without provinces (cities with names wrongly written)
check_citiesWronglyWritten <- ENTITIES %>% select (id.entity, city, province) %>% filter(is.na(province)) %>% select(-province)
check_citiesWronglyWritten <- check_citiesWronglyWritten %>% group_by(city) %>% summarise(id.entity = paste(id.entity, collapse = ","), cases_afected = n())
