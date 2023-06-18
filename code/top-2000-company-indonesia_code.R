# Analysis Plan
# Comparing Number of Company listed in top 2000 percountry and compare to:
# Count Company vs Country Population
# Count Company vs Country GDP
# Count Company vs Country GDP/Capita
# Count Company vs Country % Population in Capital Market
# Count GDP vs Country Innovation Index
# Count Innovation Index vs Country Corruption Index

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
table_population <- read_csv("countries-table.csv")
table_population <- table_population %>% select("country","pop2023","rank")

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

# Country GDP per Capita
wiki_gdp_percapita <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)_per_capita"
html_gdp_percapita <- read_html(wiki_gdp_percapita) %>% rvest::html_nodes("table.wikitable") %>% 
  html_table(fill=TRUE)
table_gdp_percapita <- html_gdp_percapita[[1]]
table_gdp_percapita <- table_gdp_percapita %>% row_to_names(row_number = 1) %>% clean_names()
colnames(table_gdp_percapita)[3:8] <- c("imf_estimate","imf_year","wb_estimate","wb_year","un_estimate","un_year")

# clean value for country gdp percapita
# convert imf estimate to number 
table_gdp_percapita$imf_estimate = as.numeric(gsub(",","",table_gdp_percapita$imf_estimate))
table_gdp_percapita$imf_year = as.numeric(table_gdp_percapita$imf_year)
table_gdp_percapita$imf_year[is.na(table_gdp_percapita$imf_year)]= 2023
table_gdp_percapita = table_gdp_percapita[complete.cases(table_gdp_percapita),]

# Country Innovation Index
innovation <- read_delim('Innovation_index.txt',delim='\t')

# Country Corruption Index
corruption <- "https://en.wikipedia.org/wiki/Corruption_Perceptions_Index"
corruption_index <- read_html(corruption) %>% rvest::html_nodes("table.wikitable") %>% 
  html_table(fill=TRUE)
corruption_index <- corruption_index[[2]]
colnames(corruption_index)[1:3] <- c("rank","nation/territory","2022") 
corruption_index<- corruption_index%>% select("rank","nation/territory","2022")
corruption_index$rank<-as.numeric(corruption_index$rank)
corruption_index<- corruption_index[complete.cases(corruption_index),]

# clean value for country gdp percapita
# convert imf estimate to number 
table_gdp_percapita$imf_estimate = as.numeric(gsub(",","",table_gdp_percapita$imf_estimate))
table_gdp_percapita$imf_year = as.numeric(table_gdp_percapita$imf_year)
table_gdp_percapita$imf_year[is.na(table_gdp_percapita$imf_year)]= 2023
table_gdp_percapita = table_gdp_percapita[complete.cases(table_gdp_percapita),]

# CHART
# CHART 1
# Barchart Rank Number of Big Company and Number of Population
forbes_country = forbes2000 %>% group_by(country) %>% summarise(number_company=n()) %>% 
  arrange(desc(number_company)) %>% mutate(rank = dense_rank(desc(number_company)))

# Join forbes country and table population
forbes_country_population = left_join(forbes_country,table_population,by="country")
forbes_country_population = forbes_country_population %>% mutate(color_tag = case_when(country=="Indonesia" ~ 1, TRUE ~ 0)) %>% 
  mutate(label_tag = case_when(rank.y<6 ~ country,rank.x<6~country))
forbes_country_population$color_tag = as.factor(forbes_country_population$color_tag)

# bar chart number company
p_ncom <-ggplot(data=forbes_country_population, aes(x=reorder(country,desc(rank.x)), y=number_company,fill=color_tag)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=label_tag),size=3,nudge_x = 0,nudge_y = +25)+
  scale_fill_manual(values=c('light blue', 'orange'))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # axis.line = element_line(colour = "black"),
        legend.position = "none")+
  labs(
    title = "Indonesia's Rank 26",
    subtitle = "Number of Company in Forbes 2000",
    caption = "Source: Amir Harjo Analysis and Forbes"
  )+
  xlab("")+
  ylab("Number of Company by Country")+
  coord_trans(y ='log10')+
  coord_flip()

p_ncom

# bar chart population
p_pop <-ggplot(data=forbes_country_population, aes(x=reorder(country,desc(rank.y)), y=pop2023/1000000,fill=color_tag)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=label_tag),size=3,nudge_x = 0,nudge_y = +25)+
  scale_fill_manual(values=c('light blue', 'orange'))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        #axis.line = element_line(colour = "black"),
        legend.position = "none")+
  labs(
    title = "Indonesia's Rank 4",
    subtitle = "2023 Population",
    caption = "Source: Amir Harjo Analysis and World Population Review"
  )+
  xlab("")+
  ylab("Population (in Million)")+
  scale_y_continuous(limits=c(0,1600))+
  # coord_trans(x ="log10")+
  coord_flip()

p_pop

# bubble chart correlation between rank number of population vs number company where bubble is the number of company
p_ncom_pop_bubble <- ggplot(data=forbes_country_population, aes(x=rank.y, y=rank.x, size=rank.y/rank.x)) +
  geom_point(shape=21,aes(fill=number_company,colour=number_company)) +
  scale_colour_gradient(low="light blue",high="blue")+
  scale_fill_gradient(low="light blue",high="blue")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Rank Based on Number of Company") +
  xlab("Rank Based on Population") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +1)+
  labs(
    title = "Correlation of Population Rank and Number of Company Rank",
    subtitle = "Size Show the Relative Population Rank to Number of Company Rank",
    caption = "Source: Amir Harjo Analysis"
  )

p_ncom_pop_bubble

# combine all chart into one canvas
combine_pop = (p_pop + p_ncom) #/p_ncom_pop_bubble
combine_pop

# CHART 2
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

# CHART 3
table_gdp_pc_2023 = table_gdp_percapita %>% select(country_territory,un_region,imf_estimate) 
colnames(table_gdp_pc_2023)=c("country","un_region","imf_gdp_pc")

company_gdp_pc = left_join(forbes_country,table_gdp_pc_2023,by="country")
company_gdp_pc = company_gdp_pc[complete.cases(company_gdp_pc),]
company_gdp_pc = company_gdp_pc %>% mutate(rank_gdp_pc = dense_rank(desc(imf_gdp_pc)))
company_gdp_pc = company_gdp_pc %>% mutate(color_tag = case_when(country=="Indonesia" ~ 1, TRUE ~ 0)) %>% 
  mutate(label_tag = case_when(rank<6 ~ country,rank_gdp_pc<6~country,country=="Indonesia"~country))
company_gdp_pc$color_tag = as.factor(company_gdp_pc$color_tag)

# Bubble Chart: Company vs Country GDP - size
p_gdp_pc_bubble <- ggplot(data=company_gdp_pc, aes(x=imf_gdp_pc, y=number_company, size=imf_gdp_pc)) +
  geom_point(shape=21,aes(fill=number_company,colour=number_company)) +
  scale_colour_gradient(low="light blue",high="blue")+
  scale_fill_gradient(low="light blue",high="blue")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Company in Forbes 2000") +
  xlab("GDP per Capita (USD)") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +1)+
  labs(
    title = "GDP Per Capita vs Number of Company in Forbes 2000",
    caption = "Source: Amir Harjo Analysis"
  )

p_gdp_pc_bubble

# take out 3 outlier
company_gdp_pc_notop3 = company_gdp_pc %>% filter(!(country %in% c("United States","China","Japan")))

p_gdp_bubble_pc_no3 <- ggplot(data=company_gdp_pc_notop3, aes(x=imf_gdp_pc/1000, y=number_company, size=imf_gdp_pc)) +
  geom_point(shape=21,aes(fill=number_company,colour=number_company)) +
  geom_smooth(method = "lm", se = TRUE)+
  ggpubr::stat_cor(method = "pearson", label.x = -5, label.y = 30)+
  scale_colour_gradient(low="light blue",high="blue")+
  scale_fill_gradient(low="light blue",high="blue")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Company in Forbes 2000") +
  xlab("GDP per Capita (thousand USD)") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +1)+
  labs(
    title = "GDP per Capita vs Number of Company in Forbes 2000 (exclude US, China and Japan)",
    caption = "Source: Amir Harjo Analysis"
  )

p_gdp_bubble_pc_no3

# CHART 4
# GDP vs Innovation Index
colnames(innovation)=c("country_ori","Innovation Index","rank_index","year")
innovation = innovation %>% select("country_ori","Innovation Index","rank_index")
innovation = innovation %>% mutate(country=case_when(country_ori=="UK"~"United Kingdom",country_ori=="USA"~"United States",TRUE~country_ori))
inno_gdp = left_join(innovation,table_gdp_2023,by="country")
inno_gdp = inno_gdp %>% mutate(rank_gdp = dense_rank(desc(imf_gdp)))
inno_gdp = left_join(forbes_country,inno_gdp,by="country")
inno_gdp = inno_gdp[complete.cases(inno_gdp),]
inno_gdp = inno_gdp %>% mutate(color_tag = case_when(country=="Indonesia" ~ 1, TRUE ~ 0)) %>% 
  mutate(label_tag = case_when(rank_index<6 ~ country,rank<6~country,country=="Indonesia"~country))


# Bubble Chart: Innovation vs Country GDP
p_gdp_inno_bubble <- ggplot(data=inno_gdp, aes(x=`Innovation Index`, y=imf_gdp/1000, size=number_company)) +
  geom_point(shape=21,aes(fill=number_company,colour=number_company)) +
  scale_colour_gradient(low="light blue",high="blue")+
  scale_fill_gradient(low="light blue",high="blue")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("GDP (Billion USD)") +
  xlab("Innovation Index") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +1)+
  labs(
    title = "Innovation Index vs GDP",
    caption = "Source: Amir Harjo Analysis"
  )

p_gdp_inno_bubble

# no outlier
inno_notop3 = inno_gdp %>% filter(!(country %in% c("United States","China","Japan")))

p_gdp_inno_bubble_no_3 <- ggplot(data=inno_notop3, aes(x=`Innovation Index`, y=imf_gdp/1000, size=number_company)) +
  geom_point(shape=21,aes(fill=number_company,colour=number_company)) +
  scale_colour_gradient(low="light blue",high="blue")+
  scale_fill_gradient(low="light blue",high="blue")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Innovation Index") +
  xlab("GDP (Billion USD)") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +1)+
  labs(
    title = "Innovation Index vs GDP (exclude US, China and Japan",
    caption = "Source: Amir Harjo Analysis"
  )

p_gdp_inno_bubble_no_3

# based on ranking
inno_rank_bubble <- ggplot(data=inno_gdp, aes(x=rank_index, y=rank, size=number_company)) +
  geom_point(shape=21,aes(fill=number_company,colour=number_company)) +
  scale_colour_gradient(low="light blue",high="blue")+
  scale_fill_gradient(low="light blue",high="blue")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Rank Number of Company") +
  xlab("Rank Innovation Index") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +1)+
  labs(
    title = "Innovation Index vs Number Company",
    caption = "Source: Amir Harjo Analysis"
  )

inno_rank_bubble

# based on number company and index
inno_com_bubble <- ggplot(data=inno_notop3, aes(x=`Innovation Index`, y=number_company, size=number_company)) +
  geom_point(shape=21,aes(fill=number_company,colour=number_company)) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
  scale_colour_gradient(low="light blue",high="blue")+
  scale_fill_gradient(low="light blue",high="blue")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Company") +
  xlab("Innovation Index") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +1)+
  labs(
    title = "Innovation Index vs Number Company",
    caption = "Source: Amir Harjo Analysis"
  )+
  scale_y_continuous(limits=c(0,70))

inno_com_bubble

# based on innovation index and percapita
inno_pc = left_join(innovation,table_gdp_pc_2023,by="country")
inno_pc = inno_pc[complete.cases(inno_pc),]
inno_pc = inno_pc %>% mutate(color_tag = case_when(country=="Indonesia" ~ 1, TRUE ~ 0)) %>% 
  mutate(label_tag = case_when(rank_index<11 ~ country,country=="Indonesia"~country))
inno_pc$color_tag = as.factor(inno_pc$color_tag)

# bubble chart
inno_pc_bubble <- ggplot(data=inno_pc, aes(x=`Innovation Index`, y=imf_gdp_pc/1000),fill=color_tag) +
  geom_point(shape=21,aes(fill=color_tag),size=3) +
  scale_fill_manual(values=c('light blue', 'blue'))+
  stat_smooth(method = "lm", formula = y ~ x ++ I(x^2) , size = 1)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("GDP per Capita (thousand USD)") +
  xlab("Innovation Index") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +3)+
  labs(
    title = "Innovation Index vs GDP Per Capita",
    caption = "Source: Amir Harjo Analysis"
  )

inno_pc_bubble

# CHART 5 Corruption Index
# Number Company vs Corruption Index
colnames(corruption_index)=c("rank_cpi","country","clean score")
corruption_index$`clean score` = as.numeric(corruption_index$`clean score`)
corruption_ind = left_join(forbes_country, corruption_index,by="country")
corruption_ind = corruption_ind[complete.cases(corruption_ind),]
corruption_ind= corruption_ind %>% mutate(color_tag = case_when(country=="Indonesia" ~ 1, TRUE ~ 0)) %>% 
  mutate(label_tag = case_when(rank_cpi<6 ~ country,rank<6~country,country=="Indonesia"~country))

# chart index and number company
corruption_com_bubble <- ggplot(data=corruption_ind, aes(x=`clean score`, y=number_company, size=number_company)) +
  geom_point(shape=21,aes(fill=number_company,colour=number_company)) +
  # stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
  scale_fill_manual(values=c('light blue', 'blue'))+
  scale_colour_gradient(low="blue",high="blue")+
  scale_fill_gradient(low="light blue",high="blue")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Number of Company") +
  xlab("CPI Score") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +1)+
  labs(
    title = "CPI Score vs Number Company",
    caption = "Source: Amir Harjo Analysis"
  )+
  scale_y_continuous(limits=c(0,70))

corruption_com_bubble

# innovation index vs corruption index
corr_inno = left_join(innovation,corruption_index,by="country")
corr_inno = corr_inno %>% mutate(color_tag = case_when(country=="Indonesia" ~ 1, TRUE ~ 0)) %>% 
  mutate(label_tag = case_when(rank_cpi<6 ~ country,rank_index<6~country,country=="Indonesia"~country))
corr_inno$color_tag = as.factor(corr_inno$color_tag)

# corruption and innvovation bubble
corr_inno_bubble <- ggplot(data=corr_inno, aes(x=`clean score`, y=`Innovation Index`),fill=color_tag) +
  geom_point(shape=21,aes(fill=color_tag),size=2) +
  stat_smooth(method = "lm", formula = y ~ x , size = 1)+
  scale_fill_manual(values=c('light blue', 'blue'))+
  ggpubr::stat_cor(method = "pearson", label.x = -5, label.y = 30)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  ylab("Innovation Index") +
  xlab("CPI Score") +
  theme(legend.position = "none")+
  geom_text(aes(label=label_tag),size=4,nudge_x = 0,nudge_y = +1)+
  labs(
    title = "CPI Score vs Innovation Index",
    caption = "Source: Amir Harjo Analysis"
  )+
  scale_y_continuous(limits=c(0,70))

corr_inno_bubble

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