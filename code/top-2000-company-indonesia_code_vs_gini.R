# Analysis Plan
# Comparing Number of Company listed in top 2000 percountry and compare to:
# Count Company vs Country GDP
# Count Company vs Gini

setwd("~/0. Personal/Blog/Top 2000 Company")

# load library
library(tidyverse)
library(httr)
library(rvest)
library(janitor)
library(ggplot2)
library(patchwork)
library(ggpubr)

# DATA COLLECTION
# Top 2000 Company
# forbes <- read_delim('forbes_2000.txt',delim='\t')
url = "https://www.forbes.com/lists/global2000"
response = httr::GET(url)
cont = read_html(response$content)
html <- cont %>% html_elements("a.table-row")

parse_column <- function(html, css) {
  html |> html_elements(css) |> html_text()
}

items <- c(".rank", ".name", ".country", ".sales", ".profits", ".assets", ".marketValue")

tbl_rank <- parse_column(html, ".rank")
tbl_name <- parse_column(html, ".name")
tbl_country <- parse_column(html, ".country")
tbl_sales <- parse_column(html, ".sales")
tbl_profits <- parse_column(html, ".profits")
tbl_assets <- parse_column(html, ".assets")
tbl_markets <- parse_column(html, ".marketValue")

tbl_rank <- as.integer(gsub("^(\\d+)\\.$", "\\1", tbl_rank))

forbes2000 <- data.frame(
  rank = tbl_rank,
  name = tbl_name,
  country = tbl_country,
  sales = tbl_sales,
  profits = tbl_profits,
  assets = tbl_assets,
  market = tbl_markets
)

tibble::as_tibble(forbes2000)

# Country Population 2023 - DONE
table_gini <- read_csv("gini-coefficient-by-country-2023.csv")
table_gini <- table_gini %>% mutate(gini = case_when(is.na(giniWB) ~ giniCIA, TRUE ~ giniWB ))
table_gini <- table_gini %>% select("country","gini")

# Country GDP
wiki_gdp <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)"
html_gdp <- read_html(wiki_gdp) %>% rvest::html_nodes("table.wikitable") %>% 
  html_table(fill=TRUE)
table_gdp <- html_gdp[[1]]
table_gdp <- table_gdp %>% row_to_names(row_number = 1) %>% clean_names()
colnames(table_gdp)[3:8] <- c("imf_estimate","imf_year","wb_estimate","wb_year","un_estimate","un_year")

# clean value for country gdp
# convert imf estimate to number 
table_gdp$imf_estimate = as.numeric(gsub(",","",table_gdp$imf_estimate))
table_gdp$imf_year = as.numeric(table_gdp$imf_year)
table_gdp$imf_year[is.na(table_gdp$imf_year)]= 2023
table_gdp = table_gdp[complete.cases(table_gdp),]

# CHART
# Forbes aggregate by country
forbes_country = forbes2000 %>% group_by(country) %>% summarise(number_company=n()) %>% 
  arrange(desc(number_company)) %>% mutate(rank = dense_rank(desc(number_company)))

# combine all chart into one canvas
# combine_pop = (p_pop + p_ncom) #/p_ncom_pop_bubble

# CHART 1
table_gdp_2023 = table_gdp %>% select(country_territory,un_region,imf_estimate) 
colnames(table_gdp_2023)=c("country","un_region","imf_gdp")
  
company_gdp = left_join(forbes_country,table_gdp_2023,by="country")
company_gdp = company_gdp[complete.cases(company_gdp),]
company_gdp = company_gdp %>% mutate(rank_gdp = dense_rank(desc(imf_gdp)))
company_gdp = company_gdp %>% mutate(color_tag = case_when(country=="Indonesia" ~ 1, TRUE ~ 0)) %>% 
  mutate(label_tag = case_when(rank<6 ~ country,rank_gdp<6~country,country=="Indonesia"~country))
company_gdp$color_tag = as.factor(company_gdp$color_tag)
  
# Bubble Chart: Company vs Country GDP - size
p_gdp_bubble <- ggplot(data=company_gdp, aes(x=imf_gdp/1000, y=number_company, size=imf_gdp)) +
  geom_point(shape=21,aes(fill=number_company,colour=number_company)) +
  geom_smooth(method = "lm", se = TRUE)+
  ggpubr::stat_cor(method = "pearson", label.x = 100, label.y = 300)+
  stat_regline_equation(label.x =90, label.y = 250)+
  scale_colour_gradient(low="light blue",high="blue")+
  scale_fill_gradient(low="light blue",high="blue")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Company in Forbes 2000") +
  xlab("GDP in Billion USD") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +1)+
  labs(
    title = "GDP vs Number of Company in Forbes 2000",
    caption = "Source: Amir Harjo Analysis"
  )

p_gdp_bubble

# take out 3 outlier
company_gdp_notop3 = company_gdp %>% filter(!(country %in% c("United States","China","Japan")))

p_gdp_bubble_no3 <- ggplot(data=company_gdp_notop3, aes(x=imf_gdp/1000, y=number_company, size=imf_gdp)) +
  geom_point(shape=21,aes(fill=number_company,colour=number_company)) +
  geom_smooth(method = "lm", se = TRUE)+
  ggpubr::stat_cor(method = "pearson", label.x = -5, label.y = 30)+
  stat_regline_equation(label.x = 3, label.y = 25)+
  scale_colour_gradient(low="light blue",high="blue")+
  scale_fill_gradient(low="light blue",high="blue")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Company in Forbes 2000") +
  xlab("GDP in Billion USD") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +1)+
  labs(
    title = "GDP vs Number of Company in Forbes 2000 (exclude US, China and Japan)",
    caption = "Source: Amir Harjo Analysis"
  )

p_gdp_bubble_no3

p_gdp = p_gdp_bubble + p_gdp_bubble_no3
p_gdp

# CHART 2
company_gini = left_join(company_gdp,table_gini,by="country")

# Bubble Chart: Company vs Gini - size GDP
p_gini_bubble <- ggplot(data=company_gini, aes(x=gini, y=number_company, size=imf_gdp)) +
  geom_point(shape=21,aes(fill=number_company,colour=number_company)) +
  scale_colour_gradient(low="green",high="blue")+
  scale_fill_gradient(low="green",high="blue")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Company in Forbes 2000") +
  xlab("Gini") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +1)+
  labs(
    title = "Gini vs Number of Company in Forbes 2000",
    caption = "Source: Amir Harjo Analysis"
  )

p_gini_bubble

# take out 3 outlier
company_gini_notop3 = company_gini %>% filter(!(country %in% c("United States","China","Japan")))

p_gini_no3 <- ggplot(data=company_gini_notop3, aes(x=gini, y=number_company, size=imf_gdp)) +
  geom_point(shape=21,aes(fill=number_company,colour=number_company)) +
  # geom_smooth(method = "lm", se = TRUE)+
  # ggpubr::stat_cor(method = "pearson", label.x = -5, label.y = 30)+
  scale_colour_gradient(low="green",high="blue")+
  scale_fill_gradient(low="green",high="blue")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Company in Forbes 2000") +
  xlab("Gini") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +1)+
  labs(
    title = "Gini vs Number of Company in Forbes 2000 (exclude US, China and Japan)",
    caption = "Source: Amir Harjo Analysis"
  )

p_gini_no3

# Regression based on Gini and GDP to number of company
company_regression = company_gini_notop3 %>% select("country","number_company","imf_gdp","gini")
company_regression = company_regression[complete.cases(company_regression),]

# Regression
comp_reg = lm(number_company ~ imf_gdp + gini, data = company_regression)
summary(comp_reg)
plot(comp_reg)

# Source:
# Detik: https://finance.detik.com/berita-ekonomi-bisnis/d-6765192/top-8-perusahaan-ri-masuk-forbes-global-2000-ada-bri-hingga-garuda
# Forbes: https://www.forbes.com/lists/global2000/?sh=5c3e54525ac0'

# World Population Review
# Number of people: https://worldpopulationreview.com/

# Wikipedia
# Country GDP: https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)
# Country GDP/Capita: https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)_per_capita

# Global Innovation Index
# https://www.theglobaleconomy.com/rankings/gii_index/

# Corruption Perception Index
# https://www.transparency.org/en/cpi/2021

# R Selenium
# https://appsilon.com/webscraping-dynamic-websites-with-r/

# Gini
# CIA: https://www.cia.gov/the-world-factbook/field/gini-index-coefficient-distribution-of-family-income/country-comparison
# Ourworld in Data: https://ourworldindata.org/income-inequality
# World Population Review: https://worldpopulationreview.com/country-rankings/gini-coefficient-by-country
