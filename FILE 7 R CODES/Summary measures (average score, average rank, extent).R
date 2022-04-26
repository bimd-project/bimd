library(readxl)
# --------------------------------------- Higher geographical level summary measures --------------------------------------- #

# ------------------------------------------------- AVERAGE SCORE ------------------------------------------ #

# can be calculated for each domain, here an example for the overall BIMD2001 at the level of municipality 

# upload file with domains and overall indices 

# this code is using overall indices but can be modified for individual domains 

bimd2001 <-read.csv("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%201%20BIMD2001%20DOMAINS%20(SCORE%2C%20RANKS%2C%20DECILES)/bimd2001_domains_score_ranks_deciles.csv")

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

# ------------------------ AVERAGE rank ------------------------------------------ #

# can be calculated for each domain, here an example for overall BIMD2001 at the level of municipality 

bimd2001 <-read.csv("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%201%20BIMD2001%20DOMAINS%20(SCORE%2C%20RANKS%2C%20DECILES)/bimd2001_domains_score_ranks_deciles.csv")
names(bimd2001)

bimd2001 <- dplyr::select(bimd2001, c(1,20,21))
bimd2001$BIMD2001_rank <- rank(bimd2001$BIMD2001_score , na.last = 'NA', ties.method = "random") # assigns to the most negative number the highest rank

# population for statistical sectors 
pop2001 <- read_delim("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%201%20BIMD2001%20DOMAINS%20(SCORE%2C%20RANKS%2C%20DECILES)/population_2001")

bimd_pop <- merge(bimd2001, pop2001, by = "CD_RES_SECTOR", all = F)
names(bimd_pop)

bimd_pop$rank_pop <- bimd_pop$BIMD2001_rank*bimd_pop$POPULATION

bimd_pop$municipality <- substr(bimd_pop$CD_RES_SECTOR, 1,5)
bimd_pop <- aggregate(rank_pop ~ municipality, bimd_pop, sum)

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

final$avg_rank <-(final$rank_pop)/final$total_pop
final$rank_avg_rank <- rank(-final$avg_rank , na.last = 'NA', ties.method = "random") # assigns to the most negative number the highest rank

final <- final[order(final$rank_avg_rank, decreasing = F),]
final <- select(final, -c(2,3,4,5))

write.xlsx(final, "", sheetName = "Sheet1", colNames = TRUE, rowNames = TRUE, append = FALSE)

# ------------------------  extent ------------------------------------------ #

# can be calculated for each domain, here an example for overall BIMD2001 at the level of municipality 

bimd2001 <-read.csv("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%201%20BIMD2001%20DOMAINS%20(SCORE%2C%20RANKS%2C%20DECILES)/bimd2001_domains_score_ranks_deciles.csv")
names(bimd2001)

bimd2001 <- dplyr::select(bimd2001, c(1,21))

# calculate percentile 

bimd2001$percentile <- ceiling((bimd2001$BIMD2001_rank/18295)*100) # change the number 18295 to 18764 if 2011

pop2001 <- read_delim("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%201%20BIMD2001%20DOMAINS%20(SCORE%2C%20RANKS%2C%20DECILES)/population_2001")

bimd_pop <- merge(bimd2001, pop2001, by = "CD_RES_SECTOR", all = F)
table(bimd_pop$percentile)
extent_weights <- read.csv("C:/Users/otavova/OneDrive - UCL/GitHub_BIMD_2001/BIMD2011/Data_BIMD2011/extent_weights.csv")

fin <- merge(bimd_pop, extent_weights, by = "percentile", all = F)
fin$pop_weight <- fin$WEIGHT*fin$POPULATION
fin$municipality <- substr(fin$CD_RES_SECTOR, 1,5)
fin1 <- aggregate(pop_weight ~ municipality, fin, sum)

pop2001 <- read_delim("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%201%20BIMD2001%20DOMAINS%20(SCORE%2C%20RANKS%2C%20DECILES)/population_2001")
pop2001$municipality <- substr(pop2001$CD_RES_SECTOR, 1,5)
pop <- aggregate(POPULATION ~ municipality, pop2001, sum)

fin2 <- merge(fin1, pop, by = "municipality", all = F)
fin2$extent <- fin2$pop_weight/fin2$POPULATION

# rank extent and order from the most deprived 
fin2$rank_extent <- rank(-fin2$extent , na.last = 'NA', ties.method = "random") # assigns to the most negative number the highest rank

fin2 <- fin2[order(fin2$rank_extent, decreasing = F),]

# add names of the municipalities 
municipality <- read_delim("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%201%20BIMD2001%20DOMAINS%20(SCORE%2C%20RANKS%2C%20DECILES)/names_sectors_municipalities_2001.csv")

municipality <- municipality[!duplicated(municipality$COMMUNE),]

fin3 <- merge(fin2, municipality, by.x = "municipality", by.y = "REFNIS2001", all = F)
head(fin3)
fin3 <- select(fin3, -c(2,3,6,7))

write.xlsx(fin3, "", sheetName = "Sheet1", 
           colNames = TRUE, rowNames = F, append = FALSE)

