library(tidyverse)
library(ggplot2)
library(rvest)
setwd("~/Documents/Hyannis")

linear_weights <- read_csv("linear_weights.csv")


library(RSelenium)

# start the Selenium server to scrape up-to-date stats
rdriver <- rsDriver(browser = "firefox",
                    chromever  = NULL
)
remDr <- rdriver[["client"]]

url <- "http://pointstreak.com/baseball/stats.html?leagueid=166&seasonid=33591&view=batting"

# Find batting stats from league website
remDr$navigate(url)
button_selector <- 'a[title="Games"]'
button_element <- remDr$findElement(using = "css", value = button_selector)
button_element$clickElement()
Sys.sleep(5)
table_selector <- "#batting1"
source <- remDr$getPageSource()
html_string <- unlist(source)
html <- read_html(html_string)
table_selector <- "#batting1"
website_data <- html %>%
  html_nodes(css = table_selector) %>%
  html_table(fill = TRUE) %>% pluck(1)


# Get total team stats from league website
url2 <- "http://pointstreak.com/baseball/stats.html?leagueid=166&seasonid=33591&view=teampitching"
remDr$navigate(url2)
source <- remDr$getPageSource()
html_string <- unlist(source)
html <- read_html(html_string)
table_selector <- "#ptotal"
team_data <- html %>%
  html_nodes(css = table_selector) %>%
  html_table(fill = TRUE) %>% pluck(1)
team_totals <- team_data[11,]

remDr$close()
rdriver$server$stop()


# wOBA Calculation
# Find 1B and PA
mutations <- website_data %>% mutate(Singles = H - `2B` - `3B` - HR, PA = AB + BB + HBP + SF + SH) %>% filter(AB > 0)

# Get weights of each event
w1B <- linear_weights %>% filter(wobaResult == "Single") %>% pull(Weight)
w2B <- linear_weights %>% filter(wobaResult == "Double") %>% pull(Weight)
w3B <- linear_weights %>% filter(wobaResult == "Triple") %>% pull(Weight)
wHR <- linear_weights %>% filter(wobaResult == "Home run") %>% pull(Weight)
wBB <- linear_weights %>% filter(wobaResult == "Walk") %>% pull(Weight)
wHBP <- linear_weights %>% filter(wobaResult == "Hit by pitch") %>% pull(Weight)
wSAC <- linear_weights %>% filter(wobaResult == "SacBunt") %>% pull(Weight)
wSF <- linear_weights %>% filter(wobaResult == "SacFly") %>% pull(Weight)  

# Calculate wOBA
woba_summary <- mutations %>%
  group_by(Player, Team) %>%
  mutate(wOBA = round((w1B * Singles + w2B * `2B` + w3B * `3B` + wHR * HR + wBB * BB + wHBP * HBP + wSAC * SH + wSF * SF) / PA,3))


# Find league wOBA
lgWOBA <- (sum(woba_summary$Singles) * w1B + sum(woba_summary$`2B`) * w2B + sum(woba_summary$`3B`) * w3B + sum(woba_summary$HR) * wHR + 
  sum(woba_summary$BB) * wBB + sum(woba_summary$HBP) * wHBP + sum(woba_summary$SH) * wSAC + sum(woba_summary$SF) * wSF ) / sum(woba_summary$PA)

# Get wOBA scale from league run values
woba_scale <- 1.254443

# Convert wOBA to wRAA
wRAA_summary <- woba_summary %>% mutate(wRAA = round((wOBA - lgWOBA) / woba_scale * PA, 3)) %>% arrange(-wRAA) 

# Park Factors
teams <- c("BOU", "BRE", "CHA", "COT", "FAL", "HAR", "HYA", "ORL", "WAR", "YD")
park_factor <- c(90.1, 115.2, 98.9, 100.4, 104.8, 85.2, 102.9, 94, 100.3, 112.2)
pf_table <- as.data.frame(cbind(teams, as.numeric(park_factor) / 100))
colnames(pf_table) <- c("Team", "ParkFactor")
park_factor <- wRAA_summary %>% left_join(pf_table, by = "Team")


# Convert wRAA to batting runs
lgR_PA <- sum(mutations$R) / sum(mutations$PA)
batRuns_summary <- park_factor %>% mutate(batRuns = round(wRAA + (lgR_PA - (as.numeric(ParkFactor) * lgR_PA)) * PA, 3)) 

# Baserunning Runs - can only use SB and CS
Outs <- team_totals$IP %% 1 * 10 + 3 * floor(team_totals$IP)
wSB <- 0.2
wCS <- -1 * (2 * (sum(mutations$R) / (Outs)) + 0.075)

lgwSB <- (sum(mutations$SB) * wSB + sum(mutations$CS) * wCS) / (sum(mutations$Singles) + sum(mutations$BB) + sum(mutations$HBP))
bsRuns_summary <- batRuns_summary %>% mutate(bsRuns = round(SB * wSB + CS * wCS - lgwSB * (Singles + BB + HBP), 3))

# Positional Adjustment - currently not included
#positions <- c("C", "1B", "2B", "3B", "SS", "IF", "LF", "CF", "RF", "OF", "P")
#pos_adj <- c(12.5, -12.5, 2.5, 2.5, 7.5, 2.5, -7.5, 2.5, -7.5, -2.5, 0.0)
#pos_adj_table <- as.data.frame(cbind(positions, pos_adj))
#colnames(pos_adj_table) <- c("P", "PosAdj")
#pos_summary <- bsRuns_summary %>% left_join(pos_adj_table, by = "P") %>% mutate(wPosAdj = as.numeric(PosAdj) * G /  162)

# League Adjustment
lgAdj <- (-1 * (sum(bsRuns_summary$batRuns) + sum(bsRuns_summary$bsRuns)) / sum(bsRuns_summary$PA))
lg_summary <- bsRuns_summary %>% mutate(lgAdjustment = lgAdj * PA)

# Runs Per Win
rpw <- 9*(sum(lg_summary$R) / (Outs / 3)) * 1.5 + 3

# Replacement Level
Total_WAR <- 1000 * 220 / 2430 * 0.57
GP <- sum(team_totals$W)
rep_level <- Total_WAR * (GP/220) * rpw / sum(mutations$PA)
rep_summary <- lg_summary %>% mutate(repLevel = rep_level * PA)

# Calculate WAR
war_summary <- rep_summary %>% mutate(WAR = round((batRuns + bsRuns + lgAdjustment + repLevel) / rpw, 3))
hya_war <- war_summary %>% filter(Team == "HYA")

# Offensive WAR Summary
select_war <- war_summary %>% select(Player, Team, P, PA, wOBA, wRAA, ParkFactor, batRuns, bsRuns, WAR) %>% arrange(-WAR)
hya_select <- select_war %>% filter(Team == "HYA")

# With PitchingWAR.R File, calculate WAR by team
teamOWAR <- select_war %>% group_by(Team) %>% summarize(TmOWAR = sum(WAR)) %>% arrange(-TmOWAR)
teamPWAR <- read_csv("teamPWAR.csv")
teamPWAR <- teamPWAR %>% rename(Team = Team.x)
teamWAR <- teamOWAR %>% inner_join(teamPWAR, by = "Team") %>% mutate(TmWAR = TmOWAR + TmPWAR) %>% arrange(-TmWAR)

