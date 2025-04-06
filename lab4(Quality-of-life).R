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


#индекс покупательной способности
plot(
  NA, xlim = range(years), ylim = range(combined_df$Purchasing_Power_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Индекс покупательной способности",
  main = "Индекс покупательной способности (2014–2021)"
)
for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Purchasing_Power_Index, type = "o", col = colors[i], lwd = 2)
}
legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n")


#индекс загрязнения
plot(
  NA, xlim = range(years), ylim = range(combined_df$Pollution_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Индекс загрязнения",
  main = "Индекс загрязнения (2014–2021)"
)
for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Pollution_Index, type = "o", col = colors[i], lwd = 2)
}
legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n")


#отношение цены на жилье к доходу
plot(
  NA, xlim = range(years), ylim = range(combined_df$Property_Price_to_Income_Ratio, na.rm = TRUE),
  xlab = "Год", ylab = "Отношение цены на жилье к доходу",
  main = "Отношение цены на жилье к доходу (2014–2021)"
)
for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Property_Price_to_Income_Ratio, type = "o", col = colors[i], lwd = 2)
}
legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n", inset = c(0, 0.4))


#индекс прожиточного минимума
plot(
  NA, xlim = range(years), ylim = range(combined_df$Cost_of_Living_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Индекс прожиточного минимума",
  main = "Индекс прожиточного минимума (2014–2021)"
)
for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Cost_of_Living_Index, type = "o", col = colors[i], lwd = 2)
}
legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n", inset = c(0, 0.2))


#индекс безопасности
plot(
  NA, xlim = range(years), ylim = range(combined_df$Safety_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Индекс безопасности",
  main = "Индекс безопасности (2014–2021)"
)
for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Safety_Index, type = "o", col = colors[i], lwd = 2)
}
legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n", inset = c(0, 0.25))


#индекс медицинского обслуживания
plot(
  NA, xlim = range(years), ylim = range(combined_df$Health_Care_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Индекс медицинского обслуживания",
  main = "Индекс медицинского обслуживания (2014–2021)"
)
for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Health_Care_Index, type = "o", col = colors[i], lwd = 2)
}
legend("bottomright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n")



#индекс времени движения на дороге
plot(
  NA, xlim = range(years), ylim = range(combined_df$Traffic_Commute_Time_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Индекс времени движения на дороге",
  main = "Индекс времени движения на дороге (2014–2021)"
)
for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df, Country == target_countries[i])
  lines(country_data$Year, country_data$Traffic_Commute_Time_Index, type = "o", col = colors[i], lwd = 2)
}
legend("topright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n", inset = c(0, 0.25))


# Климатический индекс (только с 2016 года)
combined_df_filtered <- subset(combined_df, Year >= 2016)
combined_df_filtered$Climate_Index <- as.numeric(as.character(combined_df_filtered$Climate_Index))

plot(
  NA, xlim = range(2016:2021), ylim = range(combined_df_filtered$Climate_Index, na.rm = TRUE),
  xlab = "Год", ylab = "Климатический индекс",
  main = "Климатический индекс (2016–2021)"
)

for (i in seq_along(target_countries)) {
  country_data <- subset(combined_df_filtered, Country == target_countries[i])
  lines(country_data$Year, country_data$Climate_Index, type = "o", col = colors[i], lwd = 2)
}

legend("bottomright", legend = target_countries, col = colors, lty = 1, lwd = 2, bty = "n")







# Музеи Ростовской области (8 вариант)

library(rvest)
library(dplyr)
library(stringr)
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


