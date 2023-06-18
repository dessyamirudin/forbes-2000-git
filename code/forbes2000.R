library(httr)
library(rvest)

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

