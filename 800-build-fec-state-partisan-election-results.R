library(tidyverse)
library(tidylog)
library(readxl)

##### 2018 ####
# READ TABLE
election_2018 <- read_excel('data/federalelections2018.xlsx', sheet = 3, col_names = FALSE)
# REMOVE UNUSED TOP ROWS
election_2018 <- election_2018[-1:-3,]
# RENAME COLUMNS
colnames(election_2018) <- election_2018[1,]
# DROP COLUMN NAME ROW
election_2018 <- election_2018[-1,]
# DROP TOTALS
election_2018 <- election_2018[-56:-73, ]

election_2018 <- election_2018 %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  # CONVERT TO NUMERIC
  mutate(`Democratic Candidates` = as.numeric(`Democratic Candidates`),
         `Republican Candidates` = as.numeric(`Republican Candidates`),
         `Other Candidates` = as.numeric(`Other Candidates`)) %>%
  # RENAME COLUMNS
  rename(state = State,
         dem = `Democratic Candidates`,
         rep = `Republican Candidates`,
         other = `Other Candidates`) %>%
  # ADD SHARE OF VOTE
  mutate(vote_total = dem + rep + other,
         dem_share = dem / vote_total,
         rep_share = rep / vote_total,
         other_share = other / vote_total) %>%
  select(-vote_total)

# MAKE TIDY
# Gather Votes
election_2018_1 <- election_2018 %>%
  select(-dem_share, -rep_share, -other_share) %>%
  gather(party, raw_vote, dem:other)
# Gather Share
election_2018 <- election_2018 %>%
  select(-dem, -rep, -other) %>%
  rename(dem = dem_share,
         rep = rep_share,
         other = other_share) %>%
  gather(party, share_vote, dem:other) %>%
  # Join with Raw Vote 
  right_join(election_2018_1) %>%
  # Add Year and Type
  mutate(year = 2018,
         type = 'GE') %>%
  # Reorder
  select(state, party, year, type, raw_vote, share_vote)


##### 2016 - General ####
# READ TABLE
election_2016 <- read_excel('data/federalelections2016.xlsx', sheet = 5, col_names = FALSE)
# REMOVE UNUSED TOP ROWS
election_2016 <- election_2016[-1:-3,]
# RENAME COLUMNS
colnames(election_2016) <- election_2016[1,]
# DROP COLUMN NAME ROW
election_2016 <- election_2016[-1,]
# DROP TOTALS
election_2016 <- election_2016[-57:-75, ]

election_2016 <- election_2016 %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  # CONVERT TO NUMERIC
  mutate(`Democratic Candidates` = as.numeric(`Democratic Candidates`),
         `Republican Candidates` = as.numeric(`Republican Candidates`),
         `Other Candidates` = as.numeric(`Other Candidates`)) %>%
  # RENAME COLUMNS
  rename(state = State,
         dem = `Democratic Candidates`,
         rep = `Republican Candidates`,
         other = `Other Candidates`) %>%
  # ADD SHARE OF VOTE
  mutate(vote_total = dem + rep + other,
         dem_share = dem / vote_total,
         rep_share = rep / vote_total,
         other_share = other / vote_total) %>%
  select(-vote_total)

# MAKE TIDY
# Gather Votes
election_2016_1 <- election_2016 %>%
  select(-dem_share, -rep_share, -other_share) %>%
  gather(party, raw_vote, dem:other)
# Gather Share
election_2016 <- election_2016 %>%
  select(-dem, -rep, -other) %>%
  rename(dem = dem_share,
         rep = rep_share,
         other = other_share) %>%
  gather(party, share_vote, dem:other) %>%
  # Join with Raw Vote 
  right_join(election_2016_1) %>%
  # Add Year and Type
  mutate(year = 2016,
         type = 'GE') %>%
  # Reorder
  select(state, party, year, type, raw_vote, share_vote)

# BIND INTO BIG TABLE
elections <- election_2016 %>% rbind(election_2018)



##### 2016 - Presidential ####
# READ TABLE
election_2016 <- read_excel('data/federalelections2016.xlsx', sheet = 3, col_names = FALSE)
# REMOVE UNUSED TOP ROWS
election_2016 <- election_2016[-1:-3,]
# DROP EV AND TOTALS
election_2016 <- election_2016[, c(1,4:6)]
# DROP TOTALS
election_2016 <- election_2016[-53:-59, ]
# RENAME COLUMNS
colnames(election_2016) <- c('state', 'rep', 'dem', 'other')
# DROP COLUMN NAME ROW
election_2016 <- election_2016[-1,]

election_2016 <- election_2016 %>%
  # CONVERT NAs TO 0
  mutate_all(~replace(., is.na(.), 0)) %>%
  # CONVERT TO NUMERIC
  mutate(rep = as.numeric(rep),
         dem = as.numeric(dem),
         other = as.numeric(other)) %>%
  # ADD SHARE OF VOTE
  mutate(vote_total = dem + rep + other,
         dem_share = dem / vote_total,
         rep_share = rep / vote_total,
         other_share = other / vote_total) %>%
  select(-vote_total)

# MAKE TIDY
# Gather Votes
election_2016_1 <- election_2016 %>%
  select(-dem_share, -rep_share, -other_share) %>%
  gather(party, raw_vote, rep:other)
# Gather Share
election_2016 <- election_2016 %>%
  select(-dem, -rep, -other) %>%
  rename(dem = dem_share,
         rep = rep_share,
         other = other_share) %>%
  gather(party, share_vote, dem:other) %>%
  # Join with Raw Vote 
  right_join(election_2016_1) %>%
  # Add Year and Type
  mutate(year = 2016,
         type = 'PRES') %>%
  # Reorder
  select(state, party, year, type, raw_vote, share_vote)

# BIND INTO BIG TABLE
elections <- election_2016 %>% rbind(elections)

# SAVE
save(elections, file = 'rda/elections.rda')

# CLEAR ENVIRONS
rm(list=ls())

# LOAD ELECTIONS
load('rda/elections.rda')


##### 2014 - General ####
# READ TABLE
election_2014 <- read_excel('data/federalelections2014.xls', sheet = 3, col_names = FALSE)
# REMOVE UNUSED TOP ROWS
election_2014 <- election_2014[-1:-3,]
# RENAME COLUMNS
colnames(election_2014) <- election_2014[1,]
# DROP COLUMN NAME ROW
election_2014 <- election_2014[-1,]
# DROP TOTALS
election_2014 <- election_2014[-57:-73, ]

election_2014 <- election_2014 %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  # CONVERT TO NUMERIC
  mutate(`Democratic Candidates` = as.numeric(`Democratic Candidates`),
         `Republican Candidates` = as.numeric(`Republican Candidates`),
         `Other Candidates` = as.numeric(`Other Candidates`)) %>%
  # RENAME COLUMNS
  rename(state = State,
         dem = `Democratic Candidates`,
         rep = `Republican Candidates`,
         other = `Other Candidates`) %>%
  # ADD SHARE OF VOTE
  mutate(vote_total = dem + rep + other,
         dem_share = dem / vote_total,
         rep_share = rep / vote_total,
         other_share = other / vote_total) %>%
  select(-vote_total)

# MAKE TIDY
# Gather Votes
election_2014_1 <- election_2014 %>%
  select(-dem_share, -rep_share, -other_share) %>%
  gather(party, raw_vote, dem:other)
# Gather Share
election_2014 <- election_2014 %>%
  select(-dem, -rep, -other) %>%
  rename(dem = dem_share,
         rep = rep_share,
         other = other_share) %>%
  gather(party, share_vote, dem:other) %>%
  # Join with Raw Vote 
  right_join(election_2014_1) %>%
  # Add Year and Type
  mutate(year = 2014,
         type = 'GE') %>%
  # Reorder
  select(state, party, year, type, raw_vote, share_vote)

# BIND INTO BIG TABLE
elections <- election_2014 %>% rbind(elections)

# SAVE
save(elections, file = 'rda/elections.rda')

# CLEAR ENVIRONS
rm(list=ls())

# LOAD ELECTIONS
load('rda/elections.rda')



##### 2012 - General ####
# READ TABLE
election_2012 <- read_excel('data/federalelections2012.xls', sheet = 5, col_names = FALSE)
# REMOVE UNUSED TOP ROWS
election_2012 <- election_2012[-1:-4,]
# RENAME COLUMNS
colnames(election_2012) <- election_2012[1,]
# DROP COLUMN NAME ROW
election_2012 <- election_2012[-1,]
# DROP TOTALS
election_2012 <- election_2012[-57:-75, ]

election_2012 <- election_2012 %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  # CONVERT TO NUMERIC
  mutate(`Democratic Candidates` = as.numeric(`Democratic Candidates`),
         `Republican Candidates` = as.numeric(`Republican Candidates`),
         `Other Candidates` = as.numeric(`Other Candidates`)) %>%
  # RENAME COLUMNS
  rename(state = State,
         dem = `Democratic Candidates`,
         rep = `Republican Candidates`,
         other = `Other Candidates`) %>%
  # ADD SHARE OF VOTE
  mutate(vote_total = dem + rep + other,
         dem_share = dem / vote_total,
         rep_share = rep / vote_total,
         other_share = other / vote_total) %>%
  select(-vote_total)

# MAKE TIDY
# Gather Votes
election_2012_1 <- election_2012 %>%
  select(-dem_share, -rep_share, -other_share) %>%
  gather(party, raw_vote, dem:other)
# Gather Share
election_2012 <- election_2012 %>%
  select(-dem, -rep, -other) %>%
  rename(dem = dem_share,
         rep = rep_share,
         other = other_share) %>%
  gather(party, share_vote, dem:other) %>%
  # Join with Raw Vote 
  right_join(election_2012_1) %>%
  # Add Year and Type
  mutate(year = 2012,
         type = 'GE') %>%
  # Reorder
  select(state, party, year, type, raw_vote, share_vote)

# BIND INTO BIG TABLE
elections <- election_2012 %>% rbind(elections)

# SAVE
save(elections, file = 'rda/elections.rda')

# CLEAR ENVIRONS
rm(list=ls())

# LOAD ELECTIONS
load('rda/elections.rda')



##### 2012 - Presidential ####
# READ TABLE
election_2012 <- read_excel('data/federalelections2012.xls', sheet = 3, col_names = FALSE)
# REMOVE UNUSED TOP ROWS
election_2012 <- election_2012[-1:-4,]
# DROP EV AND TOTALS
election_2012 <- election_2012[, c(1,4:6)]
# DROP TOTALS
election_2012 <- election_2012[-53:-56, ]
# RENAME COLUMNS
colnames(election_2012) <- c('state', 'dem', 'rep', 'other')
# DROP COLUMN NAME ROW
election_2012 <- election_2012[-1,]

election_2012 <- election_2012 %>%
  # CONVERT NAs TO 0
  mutate_all(~replace(., is.na(.), 0)) %>%
  # CONVERT TO NUMERIC
  mutate(rep = as.numeric(rep),
         dem = as.numeric(dem),
         other = as.numeric(other)) %>%
  # ADD SHARE OF VOTE
  mutate(vote_total = dem + rep + other,
         dem_share = dem / vote_total,
         rep_share = rep / vote_total,
         other_share = other / vote_total) %>%
  select(-vote_total)

# MAKE TIDY
# Gather Votes
election_2012_1 <- election_2012 %>%
  select(-dem_share, -rep_share, -other_share) %>%
  gather(party, raw_vote, dem:other)
# Gather Share
election_2012 <- election_2012 %>%
  select(-dem, -rep, -other) %>%
  rename(dem = dem_share,
         rep = rep_share,
         other = other_share) %>%
  gather(party, share_vote, dem:other) %>%
  # Join with Raw Vote 
  right_join(election_2012_1) %>%
  # Add Year and Type
  mutate(year = 2012,
         type = 'PRES') %>%
  # Reorder
  select(state, party, year, type, raw_vote, share_vote)

# BIND INTO BIG TABLE
elections <- election_2012 %>% rbind(elections)

# SAVE
save(elections, file = 'rda/elections.rda')

# CLEAR ENVIRONS
rm(list=ls())

# LOAD ELECTIONS
load('rda/elections.rda')



##### 2010 - General ####
# READ TABLE
election_2010 <- read_excel('data/federalelections2010.xls', sheet = 4, col_names = FALSE)
# REMOVE UNUSED TOP ROWS and STATE NAME
election_2010 <- election_2010[-1:-3, -1]
# RENAME COLUMNS
colnames(election_2010) <- election_2010[1,]
# DROP COLUMN NAME ROW
election_2010 <- election_2010[-1,]
# DROP TOTALS
election_2010 <- election_2010[-56:-74, ]

election_2010 <- election_2010 %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  # CONVERT TO NUMERIC
  mutate(`Democratic Candidates` = as.numeric(`Democratic Candidates`),
         `Republican Candidates` = as.numeric(`Republican Candidates`),
         `Other Candidates` = as.numeric(`Other Candidates`)) %>%
  # RENAME COLUMNS
  rename(state = State,
         dem = `Democratic Candidates`,
         rep = `Republican Candidates`,
         other = `Other Candidates`) %>%
  # ADD SHARE OF VOTE
  mutate(vote_total = dem + rep + other,
         dem_share = dem / vote_total,
         rep_share = rep / vote_total,
         other_share = other / vote_total) %>%
  select(-vote_total)

# MAKE TIDY
# Gather Votes
election_2010_1 <- election_2010 %>%
  select(-dem_share, -rep_share, -other_share) %>%
  gather(party, raw_vote, dem:other)
# Gather Share
election_2010 <- election_2010 %>%
  select(-dem, -rep, -other) %>%
  rename(dem = dem_share,
         rep = rep_share,
         other = other_share) %>%
  gather(party, share_vote, dem:other) %>%
  # Join with Raw Vote 
  right_join(election_2010_1) %>%
  # Add Year and Type
  mutate(year = 2010,
         type = 'GE') %>%
  # Reorder
  select(state, party, year, type, raw_vote, share_vote)

# BIND INTO BIG TABLE
elections <- election_2010 %>% rbind(elections)

# SAVE
save(elections, file = 'rda/elections.rda')

# CLEAR ENVIRONS
rm(list=ls())

# LOAD ELECTIONS
load('rda/elections.rda')



##### 2008 - General ####
# READ TABLE
election_2008 <- read_excel('data/federalelections2008.xls', sheet = 5, col_names = FALSE)
# REMOVE UNUSED TOP ROWS
election_2008 <- election_2008[-1:-3, ]
# RENAME COLUMNS
colnames(election_2008) <- election_2008[1,]
# DROP COLUMN NAME ROW
election_2008 <- election_2008[-1,]
# DROP TOTALS
election_2008 <- election_2008[-57:-73, ]

election_2008 <- election_2008 %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  # CONVERT TO NUMERIC
  mutate(`Democratic Candidates` = as.numeric(`Democratic Candidates`),
         `Republican Candidates` = as.numeric(`Republican Candidates`),
         `Other Candidates` = as.numeric(`Other Candidates`)) %>%
  # RENAME COLUMNS
  rename(state = State,
         dem = `Democratic Candidates`,
         rep = `Republican Candidates`,
         other = `Other Candidates`) %>%
  # ADD SHARE OF VOTE
  mutate(vote_total = dem + rep + other,
         dem_share = dem / vote_total,
         rep_share = rep / vote_total,
         other_share = other / vote_total) %>%
  select(-vote_total)

# MAKE TIDY
# Gather Votes
election_2008_1 <- election_2008 %>%
  select(-dem_share, -rep_share, -other_share) %>%
  gather(party, raw_vote, dem:other)
# Gather Share
election_2008 <- election_2008 %>%
  select(-dem, -rep, -other) %>%
  rename(dem = dem_share,
         rep = rep_share,
         other = other_share) %>%
  gather(party, share_vote, dem:other) %>%
  # Join with Raw Vote 
  right_join(election_2008_1) %>%
  # Add Year and Type
  mutate(year = 2008,
         type = 'GE') %>%
  # Reorder
  select(state, party, year, type, raw_vote, share_vote)

# BIND INTO BIG TABLE
elections <- election_2008 %>% rbind(elections)

# SAVE
save(elections, file = 'rda/elections.rda')

# CLEAR ENVIRONS
rm(list=ls())

# LOAD ELECTIONS
load('rda/elections.rda')



##### 2008 - Presidential ####
# READ TABLE
election_2008 <- read_excel('data/federalelections2008.xls', sheet = 3, col_names = FALSE)
# REMOVE UNUSED TOP ROWS
election_2008 <- election_2008[-1:-3,]
# DROP EV AND TOTALS
election_2008 <- election_2008[, c(1,4:6)]
# DROP TOTALS
election_2008 <- election_2008[-53:-56, ]
# RENAME COLUMNS
colnames(election_2008) <- c('state', 'dem', 'rep', 'other')
# DROP COLUMN NAME ROW
election_2008 <- election_2008[-1,]

election_2008 <- election_2008 %>%
  # CONVERT NAs TO 0
  mutate_all(~replace(., is.na(.), 0)) %>%
  # CONVERT TO NUMERIC
  mutate(rep = as.numeric(rep),
         dem = as.numeric(dem),
         other = as.numeric(other)) %>%
  # ADD SHARE OF VOTE
  mutate(vote_total = dem + rep + other,
         dem_share = dem / vote_total,
         rep_share = rep / vote_total,
         other_share = other / vote_total) %>%
  select(-vote_total)

# MAKE TIDY
# Gather Votes
election_2008_1 <- election_2008 %>%
  select(-dem_share, -rep_share, -other_share) %>%
  gather(party, raw_vote, dem:other)
# Gather Share
election_2008 <- election_2008 %>%
  select(-dem, -rep, -other) %>%
  rename(dem = dem_share,
         rep = rep_share,
         other = other_share) %>%
  gather(party, share_vote, dem:other) %>%
  # Join with Raw Vote 
  right_join(election_2008_1) %>%
  # Add Year and Type
  mutate(year = 2008,
         type = 'PRES') %>%
  # Reorder
  select(state, party, year, type, raw_vote, share_vote)

# BIND INTO BIG TABLE
elections <- election_2008 %>% rbind(elections)

# SAVE
save(elections, file = 'rda/elections.rda')

# CLEAR ENVIRONS
rm(list=ls())

# LOAD ELECTIONS
load('rda/elections.rda')



##### 2006 - General ####
# READ TABLE
election_2006 <- read_excel('data/federalelections2006.xls', sheet = 3, col_names = FALSE)
# REMOVE UNUSED TOP ROWS
election_2006 <- election_2006[-1:-3, ]
# RENAME COLUMNS
colnames(election_2006) <- election_2006[1,]
# DROP COLUMN NAME ROW
election_2006 <- election_2006[-1,]
# DROP TOTALS
election_2006 <- election_2006[-55:-57, ]

election_2006 <- election_2006 %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  # CONVERT TO NUMERIC
  mutate(`Democratic Candidates` = as.numeric(`Democratic Candidates`),
         `Republican Candidates` = as.numeric(`Republican Candidates`),
         `Other Candidates` = as.numeric(`Other Candidates`)) %>%
  # RENAME COLUMNS
  rename(state = State,
         dem = `Democratic Candidates`,
         rep = `Republican Candidates`,
         other = `Other Candidates`) %>%
  # ADD SHARE OF VOTE
  mutate(vote_total = dem + rep + other,
         dem_share = dem / vote_total,
         rep_share = rep / vote_total,
         other_share = other / vote_total) %>%
  select(-vote_total)

# MAKE TIDY
# Gather Votes
election_2006_1 <- election_2006 %>%
  select(-dem_share, -rep_share, -other_share) %>%
  gather(party, raw_vote, dem:other)
# Gather Share
election_2006 <- election_2006 %>%
  select(-dem, -rep, -other) %>%
  rename(dem = dem_share,
         rep = rep_share,
         other = other_share) %>%
  gather(party, share_vote, dem:other) %>%
  # Join with Raw Vote 
  right_join(election_2006_1) %>%
  # Add Year and Type
  mutate(year = 2006,
         type = 'GE') %>%
  # Reorder
  select(state, party, year, type, raw_vote, share_vote)

# BIND INTO BIG TABLE
elections <- election_2006 %>% rbind(elections)

# SAVE
save(elections, file = 'rda/elections.rda')

# CLEAR ENVIRONS
rm(list=ls())

# LOAD ELECTIONS
load('rda/elections.rda')



##### 2004 - General ####
# READ TABLE
election_2004 <- read_excel('data/federalelections2004.xls', sheet = 8, col_names = FALSE)
# REMOVE UNUSED TOP ROWS
election_2004 <- election_2004[-1:-3, ]
# RENAME COLUMNS
colnames(election_2004) <- election_2004[1,]
# DROP COLUMN NAME ROW
election_2004 <- election_2004[-1,]
# DROP TOTALS
election_2004 <- election_2004[-56:-57, ]

election_2004 <- election_2004 %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  # CONVERT TO NUMERIC
  mutate(`Democratic Candidates` = as.numeric(`Democratic Candidates`),
         `Republican Candidates` = as.numeric(`Republican Candidates`),
         `Other Candidates` = as.numeric(`Other Candidates`)) %>%
  # RENAME COLUMNS
  rename(state = State,
         dem = `Democratic Candidates`,
         rep = `Republican Candidates`,
         other = `Other Candidates`) %>%
  # ADD SHARE OF VOTE
  mutate(vote_total = dem + rep + other,
         dem_share = dem / vote_total,
         rep_share = rep / vote_total,
         other_share = other / vote_total) %>%
  select(-vote_total)

# MAKE TIDY
# Gather Votes
election_2004_1 <- election_2004 %>%
  select(-dem_share, -rep_share, -other_share) %>%
  gather(party, raw_vote, dem:other)
# Gather Share
election_2004 <- election_2004 %>%
  select(-dem, -rep, -other) %>%
  rename(dem = dem_share,
         rep = rep_share,
         other = other_share) %>%
  gather(party, share_vote, dem:other) %>%
  # Join with Raw Vote 
  right_join(election_2004_1) %>%
  # Add Year and Type
  mutate(year = 2004,
         type = 'GE') %>%
  # Reorder
  select(state, party, year, type, raw_vote, share_vote)

# BIND INTO BIG TABLE
elections <- election_2004 %>% rbind(elections)

# SAVE
save(elections, file = 'rda/elections.rda')

# CLEAR ENVIRONS
rm(list=ls())

# LOAD ELECTIONS
load('rda/elections.rda')



##### 2004 - Presidential ####
# READ TABLE
election_2004 <- read_excel('data/federalelections2004.xls', sheet = 6, col_names = FALSE)
# REMOVE UNUSED TOP ROWS
election_2004 <- election_2004[-1:-2,]
# DROP EV AND TOTALS
election_2004 <- election_2004[, c(1,4:6)]
# DROP TOTALS
election_2004 <- election_2004[-53:-59, ]
# RENAME COLUMNS
colnames(election_2004) <- c('state', 'rep', 'dem', 'other')
# DROP COLUMN NAME ROW
election_2004 <- election_2004[-1,]

election_2004 <- election_2004 %>%
  # CONVERT NAs TO 0
  mutate_all(~replace(., is.na(.), 0)) %>%
  # CONVERT TO NUMERIC
  mutate(rep = as.numeric(rep),
         dem = as.numeric(dem),
         other = as.numeric(other)) %>%
  # ADD SHARE OF VOTE
  mutate(vote_total = dem + rep + other,
         dem_share = dem / vote_total,
         rep_share = rep / vote_total,
         other_share = other / vote_total) %>%
  select(-vote_total)

# MAKE TIDY
# Gather Votes
election_2004_1 <- election_2004 %>%
  select(-dem_share, -rep_share, -other_share) %>%
  gather(party, raw_vote, rep:other)
# Gather Share
election_2004 <- election_2004 %>%
  select(-dem, -rep, -other) %>%
  rename(dem = dem_share,
         rep = rep_share,
         other = other_share) %>%
  gather(party, share_vote, dem:other) %>%
  # Join with Raw Vote 
  right_join(election_2004_1) %>%
  # Add Year and Type
  mutate(year = 2004,
         type = 'PRES') %>%
  # Reorder
  select(state, party, year, type, raw_vote, share_vote)

# BIND INTO BIG TABLE
elections <- election_2004 %>% rbind(elections)

# SAVE
save(elections, file = 'rda/elections.rda')

# CLEAR ENVIRONS
rm(list=ls())

# LOAD ELECTIONS
load('rda/elections.rda')



##### 2002 - General ####
# READ TABLE - SENATE
senate_election_2002 <- read_excel('data/2002fedresults.xls', sheet = 3, col_names = FALSE)
# REMOVE UNUSED TOP ROWS, OMIT PRIMARY
senate_election_2002 <- senate_election_2002[-1:-3, -2:-4]
# RENAME COLUMNS
colnames(senate_election_2002) <- senate_election_2002[1,]
# DROP COLUMN NAME ROW
senate_election_2002 <- senate_election_2002[-1,]
# DROP TOTALS
senate_election_2002 <- senate_election_2002[-55, ]

# READ TABLE - HOUSE
house_election_2002 <- read_excel('data/2002fedresults.xls', sheet = 4, col_names = FALSE)
# REMOVE UNUSED TOP ROWS, OMIT PRIMARY
house_election_2002 <- house_election_2002[-1:-3, -2:-4]
# RENAME COLUMNS
colnames(house_election_2002) <- house_election_2002[1,]
# DROP COLUMN NAME ROW
house_election_2002 <- house_election_2002[-1,]
# DROP TOTALS
house_election_2002 <- house_election_2002[-55, ]

# rename columns for merge
senate_election_2002 <- senate_election_2002 %>%
  rename(state = State,
         s_dem = Democratic,
         s_rep = Republican,
         s_other = Other)

# rename columns for merge
house_election_2002 <- house_election_2002 %>%
  rename(state = State,
         h_dem = Democratic,
         h_rep = Republican,
         h_other = Other)

election_2002 <- senate_election_2002 %>%
  left_join(house_election_2002) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  # CONVERT TO NUMERIC
  mutate(s_dem = as.numeric(s_dem),
         s_rep = as.numeric(s_rep),
         s_other = as.numeric(s_other),
         h_dem = as.numeric(h_dem),
         h_rep = as.numeric(h_rep),
         h_other = as.numeric(h_other),
         # ADD HOUSE AND SENATE TOGEHTER
         dem = s_dem + h_dem,
         rep = s_rep + h_rep,
         other = s_other + h_other) %>%
  # DESELECT HOUSE AND SENATE TOTALS
  select(-s_dem:-h_other) %>%
  # ADD SHARE OF VOTE
  mutate(vote_total = dem + rep + other,
         dem_share = dem / vote_total,
         rep_share = rep / vote_total,
         other_share = other / vote_total) %>%
  select(-vote_total)

# MAKE TIDY
# Gather Votes
election_2002_1 <- election_2002 %>%
  select(-dem_share, -rep_share, -other_share) %>%
  gather(party, raw_vote, dem:other)
# Gather Share
election_2002 <- election_2002 %>%
  select(-dem, -rep, -other) %>%
  rename(dem = dem_share,
         rep = rep_share,
         other = other_share) %>%
  gather(party, share_vote, dem:other) %>%
  # Join with Raw Vote 
  right_join(election_2002_1) %>%
  # Add Year and Type
  mutate(year = 2002,
         type = 'GE') %>%
  # Reorder
  select(state, party, year, type, raw_vote, share_vote)

# BIND INTO BIG TABLE
elections <- election_2002 %>% rbind(elections)

# SAVE
save(elections, file = 'rda/elections.rda')

# CLEAR ENVIRONS
rm(list=ls())

# LOAD ELECTIONS
load('rda/elections.rda')