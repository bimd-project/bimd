library(readxl)

# --------------------------------------- Higher geographical level summary measure --------------------------------------- #

# ------------------------------------------------- AVERAGE SCORE ------------------------------------------ #

# can be calculated for each domain, here an example for the overall BIMD2001 at the level of municipality 

# upload file with domains and overall indices 

# this code is using overall indices but can be modified for individual domains 

bimd2001 <- read.csv("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%201%20BIMD2001%20DOMAINS%20(SCORE%2C%20RANKS%2C%20DECILES)/BIMD2001_DOMAINS_STATISTICAL_SECTOR_ELLIS_WIDE.csv")

names(bimd2001)
bimd2001 <- dplyr::select(bimd2001, c(1,20))


# population for statistical sectors 
pop2001 <- read_delim("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%201%20BIMD2001%20DOMAINS%20(SCORE%2C%20RANKS%2C%20DECILES)/population_2001")

bimd_pop <- merge(bimd2001, pop2001, by = "CD_RES_SECTOR", all = F)
names(bimd_pop)

bimd_pop$SCORE_pop <- bimd_pop$BIMD2001_score*bimd_pop$POPULATION

bimd_pop$municipality <- substr(bimd_pop$CD_RES_SECTOR, 1,5)
bimd_pop <- aggregate(SCORE_pop ~ municipality, bimd_pop, sum)
head(bimd_pop)

# total population per municipality 

pop2001 <- read_delim("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%201%20BIMD2001%20DOMAINS%20(SCORE%2C%20RANKS%2C%20DECILES)/population_2001")
pop2001$municipality <- substr(pop2001$CD_RES_SECTOR, 1,5 )
pop2001 <- aggregate(POPULATION ~ municipality, pop2001, sum)
names(pop2001)[2] <- "total_pop"

# names of the municipalities 
municipality <- read_delim("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%201%20BIMD2001%20DOMAINS%20(SCORE%2C%20RANKS%2C%20DECILES)/names_sectors_municipalities_2001.csv")
municipality <- municipality[!duplicated(municipality$COMMUNE),]

fin_pop <- merge(bimd_pop, pop2001, by = "municipality", all = F)

final <- merge(fin_pop, municipality, by.x = "municipality", by.y = "REFNIS2001", all = F)
names(final)

final$avg_score <-(final$SCORE_pop)/final$total_pop
final$rank_avg_score <- rank(-final$avg_score , na.last = 'NA', ties.method = "random") # assigns to the most negative number the highest SCORE

final <- final[order(final$rank_avg_score, decreasing = F),]
final <- select(final, -c(2,3,4,5))

write.xlsx(final, " ", sheetName = "Sheet1", colNames = TRUE, rowNames = TRUE, append = FALSE)
