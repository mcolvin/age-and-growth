

# LOAD FISH SPECIES DATA SCRAPED FROM FISHBASE
com2<- odbcConnectAccess2007("C:/Users/mcolvin/Documents/projects/Completed-Projects-And-Back-Burner/Age and Growth/data/age_growth_data.accdb")


fish_dat<- sqlFetch(com2, "Fish Data")

