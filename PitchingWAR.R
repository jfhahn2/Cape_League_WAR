library(tidyverse)
library(ggplot2)
library(rvest)
setwd("~/Documents/Hyannis")

linear_weights <- read_csv("linear_weights.csv")

library(RSelenium)

# start the Selenium server to scrape up-to-date stats
rdriver <- rsDriver(browser = "firefox",
                    port = 55L,
                    chromever  = NULL
)
remDr <- rdriver[["client"]]

url <- "http://pointstreak.com/baseball/stats.html?leagueid=166&seasonid=33591&view=pitching"

# Find pitching stats
remDr$navigate(url)
button_selector <- 'a[title="Games"]'
button_element <- remDr$findElement(using = "css", value = button_selector)
button_element$clickElement()
Sys.sleep(5)
table_selector <- "#pitching1"
source <- remDr$getPageSource()
html_string <- unlist(source)
html <- read_html(html_string)
table_selector <- "#pitching1"
website_data <- html %>%
  html_nodes(css = table_selector) %>%
  html_table(fill = TRUE) %>% pluck(1)

# Pitching stats are split among 2 screens
url2 <- "http://pointstreak.com/baseball/stats.html?leagueid=166&seasonid=33591&view=pitching&pset=1&orderby=era&range="

remDr$navigate(url2)
button_selector <- 'a[title="Shutouts"]'
button_element <- remDr$findElement(using = "css", value = button_selector)
button_element$clickElement()
Sys.sleep(5)
table_selector <- "#pitching1"

source <- remDr$getPageSource()
html_string <- unlist(source)
html <- read_html(html_string)
data2 <- html %>%
  html_nodes(css = table_selector) %>%
  html_table(fill = TRUE) %>% pluck(1)

merged <- website_data %>% inner_join(data2, by = "Player")

remDr$close()
rdriver$server$stop()


# Fix partial IP
fix_ip <- merged %>% mutate(TrueIP = floor(IP) + 10 * (IP - floor(IP)) / 3)

## FIP-BASED WAR

# Calculate league constants for FIP and ERA
lgERA <- sum(merged$ER) * 9 / sum(fix_ip$TrueIP)
lgFIP <- (sum(merged$HR) * 13 + 3 * sum(merged$HBP) + 3 * sum(merged$BB) - 2 * sum(merged$SO)) / sum(fix_ip$TrueIP)
FIP_constant <- lgERA - lgFIP

# Calculate FIP for pitchers
fip_summary <- fix_ip %>% mutate(FIP = round((13 * HR + 3 * BB + 3 * HBP - 2 * SO) / TrueIP + FIP_constant, 2))

# Calculate league RA9 constants
lgRA9 <- sum(merged$R) * 9 / sum(fix_ip$TrueIP)
RA9_Adj <- lgRA9 - lgERA
fipr9_summary <- fip_summary %>% mutate(FIPR9 = FIP + RA9_Adj)  

# Park Factors
teams <- c("BOU", "BRE", "CHA", "COT", "FAL", "HAR", "HYA", "ORL", "WAR", "YD")
park_factor <- c(90.1, 115.2, 98.9, 100.4, 104.8, 85.2, 102.9, 94, 100.3, 112.2)
pf_table <- as.data.frame(cbind(teams, as.numeric(park_factor) / 100))
colnames(pf_table) <- c("Team.x", "ParkFactor")
park_factor <- fipr9_summary %>% left_join(pf_table, by = "Team.x")
pf_summary <- park_factor %>% mutate(pFIPR9 = FIPR9 / as.numeric(ParkFactor))

lgFIPAdj <- lgFIP + FIP_constant + RA9_Adj

# Get RAAP9
RAAP9_summary <- pf_summary %>% mutate(RAAP9 = round(lgFIPAdj - pFIPR9, 3)) 

# Dynamic RPW and WPG
rpw_summary <- RAAP9_summary %>% mutate(IP_G = TrueIP / G) %>% mutate(inner = ((18 - IP_G) * lgFIPAdj + IP_G * pFIPR9) / 18) %>% mutate(dRPW = 1.5*(2 + inner))
wpg_summary <- rpw_summary %>% mutate(WPGAA = round(RAAP9 / dRPW, 4))

# Find Replacement Level and WPG Above Replacement
replvl_summary <- wpg_summary %>% mutate(rep_lvl = (1 - GS / G) * 0.03 + (GS / G) * 0.12)
wpgar_summary <- replvl_summary %>% mutate(WPGAR = round((WPGAA + rep_lvl) * IP / 9, 5))

# Make sure Total WAR is correct amount
Total_WAR <- 1000 * 220 / 2430 * 0.43 * sum(wpgar_summary$W) / 220
WAR_to_assign <- Total_WAR - sum(wpgar_summary$WPGAR, na.rm = TRUE)
WAR_Per_IP <- WAR_to_assign / sum(wpgar_summary$TrueIP)
fipwar_summary <- wpgar_summary %>% mutate(fWAR = round(WPGAR + WAR_Per_IP * TrueIP, 3))

## RA9-BASED WAR

# Calculate RA9
ra9_summary <- fix_ip %>% mutate(RA9 = round(R * 9 / TrueIP, 2))

park_factor <- ra9_summary %>% left_join(pf_table, by = "Team.x")
pf_summary <- park_factor %>% mutate(pRA9 = RA9 / as.numeric(ParkFactor))

# Get RAAP9
RAAP9_summary <- pf_summary %>% mutate(RAAP9 = round(lgRA9 - pRA9, 3)) 

# Dynamic RPW and WPG
rpw_summary <- RAAP9_summary %>% mutate(IP_G = TrueIP / G) %>% mutate(inner = ((18 - IP_G) * lgRA9 + IP_G * pRA9) / 18) %>% mutate(dRPW = 1.5*(2 + inner))
wpg_summary <- rpw_summary %>% mutate(WPGAA = round(RAAP9 / dRPW, 4))

# Find Replacement Level and WPG Above Replacement
replvl_summary <- wpg_summary %>% mutate(rep_lvl = (1 - GS / G) * 0.03 + (GS / G) * 0.12)
wpgar_summary <- replvl_summary %>% mutate(WPGAR = round((WPGAA + rep_lvl) * IP / 9, 5))

# Make sure Total WAR is correct amount
Total_WAR <- 1000 * 220 / 2430 * 0.43 * sum(wpgar_summary$W) / 220
WAR_to_assign <- Total_WAR - sum(wpgar_summary$WPGAR, na.rm = TRUE)
WAR_Per_IP <- WAR_to_assign / sum(wpgar_summary$TrueIP)
ra9war_summary <- wpgar_summary %>% mutate(rWAR = round(WPGAR + WAR_Per_IP * TrueIP, 3))

# Merge RA9-WAR and FIP-WAR
ra9_merge <- ra9war_summary %>% select(Player, RA9, rWAR)
pwar_summary <- fipwar_summary %>% inner_join(ra9_merge, by = "Player")

# Summary table
select_pwar <- pwar_summary %>% select(Player, Team.x, IP, ParkFactor, ERA.x, FIP, fWAR, RA9, rWAR) %>% arrange(-rWAR)
hya_pwar <- select_pwar %>% filter(Team.x == "ORL")

# Write CSV for team WAR
teamPWAR <- select_pwar %>% group_by(Team.x) %>% summarize(TmPWAR = sum(rWAR, na.rm = TRUE)) %>% arrange(-TmPWAR)

write_csv(teamPWAR, "teamPWAR.csv")
