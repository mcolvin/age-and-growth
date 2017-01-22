

# LOAD FISH SPECIES DATA SCRAPED FROM FISHBASE
#fish_dat<- read.csv("./dat/combos.csv")#

fn<- dir("./output")




com3<- odbcConnectAccess2007("./output/output.accdb")
sqlTables(com3)

combos<- sqlFetch(com3, "combos")

combos_in_db<- sqlFetch(com3, "qry_combo_inventory")
