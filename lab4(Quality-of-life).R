library(rvest)

target_countries <- c("Portugal", "Czech Republic", "Croatia", "Russia", "United States")
years <- 2014:2021

for (year in years) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  
  filename <- paste0("quality_of_life_", year, ".html")
  download.file(url, destfile = filename, quiet = TRUE)
  
  content <- read_html(filename)
  nodes <- html_nodes(content, "table")
  df <- html_table(nodes[[2]], fill = TRUE) %>% as.data.frame()
  df <- df[, -1]
  
  colnames(df) <- c(
    "Country", "Quality_of_Life_Index", 
    "Purchasing_Power_Index", "Safety_Index", 
    "Health_Care_Index", "Cost_of_Living_Index",
    "Property_Price_to_Income_Ratio", 
    "Traffic_Commute_Time_Index", 
    "Pollution_Index", "Climate_Index", "Extra"
  )[1:ncol(df)]
  
  df_filtered <- df %>% filter(Country %in% target_countries)
  
  assign(paste0("df_", year), df_filtered)
  assign(paste0("df_filtered_", year), df_filtered)
}


combined_df <- do.call(rbind, lapply(years, function(year) {
  df <- get(paste0("df_filtered_", year))
  df$Year <- year
  return(df)
}))

combined_df$Purchasing_Power_Index <- as.numeric(as.character(combined_df$Purchasing_Power_Index))
colors <- rainbow(length(target_countries))


index_list <- list(
  list(
    var = "Purchasing_Power_Index",
    title = "Индекс покупательной способности"
  ),
  list(
    var = "Pollution_Index",
    title = "Индекс загрязнения"
  ),
  list(
    var = "Property_Price_to_Income_Ratio",
    title = "Отношение цены на жилье к доходу"
  ),
  list(
    var = "Cost_of_Living_Index",
    title = "Индекс прожиточного минимума"
  ),
  list(
    var = "Safety_Index",
    title = "Индекс безопасности"
  ),
  list(
    var = "Health_Care_Index",
    title = "Индекс медицинского обслуживания"
  ),
  list(
    var = "Traffic_Commute_Time_Index",
    title = "Индекс времени движения на дороге"
  ),
  list(
    var = "Climate_Index",
    title = "Климатический индекс",
    filter = function(df) {
      subset_df <- subset(df, Year >= 2016)
      subset_df$Climate_Index <- as.numeric(as.character(subset_df$Climate_Index))
      subset_df
    }
  )
)


colors <- rainbow(length(target_countries))
par(mar = c(11, 4, 4, 11))


for (index in index_list) {
  if (!is.null(index$filter)) {
    current_df <- index$filter(combined_df)
  } else {
    current_df <- combined_df
  }
  
  plot(NA,
       xlim = range(current_df$Year), 
       ylim = range(current_df[[index$var]], na.rm = TRUE),
       xlab = "Год", 
       ylab = "Индекс",
       main = paste0(index$title, " (", min(current_df$Year), "-", max(current_df$Year), ")")
  )
  
  for (i in seq_along(target_countries)) {
    country_data <- subset(current_df, Country == target_countries[i])
    lines(country_data$Year, country_data[[index$var]], 
          type = "o", col = colors[i], lwd = 2)
  }
  
  legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, inset = c(-0.3, 0), xpd = TRUE, bty = "n")}





# Музеи Ростовской области (8 вариант)

library(rvest)
library(dplyr)
library(purrr)

url <- "https://ru.wikipedia.org/wiki/Список_музеев_Ростовской_области"

page <- read_html(url)
museum_table <- page %>% html_element("table.wikitable") %>% html_table()
museum_nodes <- page %>% html_element("table.wikitable") %>% html_elements("tr")
museum_nodes <- museum_nodes[-1]

extract_name_link <- function(node) {
  link_node <- node %>% html_element("td a")
  name <- link_node %>% html_text(trim = TRUE)
  link <- link_node %>% html_attr("href")
  if (!is.na(link)) {
    link <- paste0("https://ru.wikipedia.org", link)
  }
  list(name = name, link = link)
}

name_link_list <- map(museum_nodes, extract_name_link)

names <- map_chr(name_link_list, "name")
links <- map_chr(name_link_list, "link")

notes <- museum_table$Примечания

extract_address <- function(link) {
  Sys.sleep(0.5)
  if (is.na(link)) return(NA)
  
  museum_page <- tryCatch(read_html(link), error = function(e) return(NA))
  if (inherits(museum_page, "xml_document") == FALSE) return(NA)
  
  rows <- museum_page %>% html_elements("tr")
  address_node <- NA
  
  for (row in rows) {
    header <- row %>% html_element("th")
    if (!is.na(header) && !is.null(header)) {
      header_text <- header %>% html_text2()
      if (str_trim(header_text) == "Адрес") {
        address_node <- row %>% html_element("td") %>% html_text2()
        break
      }
    }
  }
  
  if (is.na(address_node)) return(NA)
  clean_address <- str_remove_all(address_node, "\\[\\d+\\]")
  clean_address <- str_remove_all(clean_address, "47°.*|HGЯO.*|\\d{2}°\\d{2}′\\d{2}″.*|mw-parser-output.*")
  clean_address <- str_squish(clean_address)
  
  return(clean_address)
}

addresses <- map_chr(links, extract_address)

museum_df <- tibble(
  Название = names,
  Адрес = addresses,
  Примечание = notes,
  Ссылка = links
)

print(museum_df)

