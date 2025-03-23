install.packages("readxl")


#Задание 2
library(readxl)

data <- read_excel("N:/Prihodko/3 курс весна/R_labs/R_lab3(data_set).xlsx", sheet = "Лист1"); data

# Столбчая диаграмма
man_matrix <- as.matrix(data[, c(3:10)])
woman_matrix <- as.matrix(data[, c(12:19)])
rez_matrix <- man_matrix + woman_matrix ; rez_matrix
rownames(rez_matrix) <- data$Олимпиада


par(mar = c(11, 4, 4, 6))
barplot(t(rez_matrix), beside = TRUE, col = rainbow(8), 
        main = "Количество мест 1-8 по каждой Олимпиаде по прыжкам в воду
        (мужчины и женщины)", 
        ylab = "Количество мест",
        las = 2)
legend("topright",  legend = paste("Место", 1:8), col = rainbow(8), lty = 1, xpd = TRUE, inset = c(-0.1, 0))



# Круговая диаграмма по количеству первых мест в каждой из олимпиад
filtered_data <- data[data$`1-м` > 0 | data$`1-ж` > 0, ]
first_places <- rowSums(filtered_data[, c(3, 12)])
names(first_places) <- filtered_data$Олимпиада

par(mar = c(5, 5, 6, 2))
pie(first_places, main = "Количество первых мест в каждой Олимпиаде по прыжкам в воду(мужчины и женщины)", 
    col = rainbow(length(first_places)),
    labels = paste(names(first_places), " (", first_places, ")", sep = ""))  #legend("topright", legend = , fill = rainbow(length(first_places)), cex = 0.8)




#Тенденции изменения количества призовых мест отдельно по мужчинам и женщинам
years <- as.numeric(gsub("[^0-9]", "", data$Олимпиада))
men_prizes <- rowSums(data[, 3:5])
women_prizes <- rowSums(data[, 12:14])

plot(years, men_prizes, type = "o", col = "blue", 
     main = "Тенденции изменения количества призовых мест", 
     xlab = "Олимпиада", ylab = "Количество призовых мест", 
     ylim = range(c(men_prizes, women_prizes)),
     las = 2, xaxt = "n")

axis(1, at = years, labels = years, las = 1)
lines(years, women_prizes, type = "o", col = "red")
legend("topright", legend = c("Мужчины", "Женщины"), col = c("blue", "red"), lty = 1)



#Задание 3(первые места)

library(readxl)

data_first_places <- read_excel("N:/Prihodko/3 курс весна/R_labs/Первые места по 6-ти олимпиадам.xlsx", sheet = "Лист1"); data_first_places
years <- as.numeric(gsub("[^0-9]", "", data_first_places$Олимпиада))
data_first_places <- data_first_places[, -1]

matplot(years,  data_first_places, type = "b",
  pch = 1, lty = 1:7, col = 1:7, xlab = "Год Олимпиады", 
  ylab = "Количество первых мест", main = "Изменение количества первых мест по годам", xaxt = "n")
#Почему-то отступы срабатывают со втрого раза
par(mar = c(3, 4, 4, 9))
axis(1, at = years, labels = years, las = 1)
legend( x = "topright", legend = names(data_first_places), col = 1:7, 
        pch = 1, lty = 1:7, xpd = TRUE, inset = c(-0.25, 0))

#Задание 3(призовые места)
library(readxl)
data_winners <- read_excel("N:/Prihodko/3 курс весна/R_labs/Призовые места по 6-ти олимпиадам.xlsx", sheet = "Лист1"); data_winners
years<- as.numeric(gsub("[^0-9]", "", data_winners$Олимпиада))
data_winners <- data_winners[, -1]

matplot(years,  data_winners, type = "b",
         pch = 1, lty = 1, col = 1:7, 
         ylab = "Количество призовых мест", main = "Изменение количества призовых мест по годам", xaxt = "n")
#Почему-то отступы срабатывают со втрого раза
par(mar = c(3, 4, 4, 9))
axis(1, at = years, labels = years, las = 1)
legend( x = "topright", legend = names(data_winners), col = 1:7, 
        pch = 1, lty = 1, xpd = TRUE, inset = c(-0.25, 0))



#Задание 4
# Столбчая диаграмма
par(mfrow=c(1,2)) 
man_matrix <- as.matrix(data[, c(3:5)])
rownames(man_matrix) <- data$Олимпиада

par(mar = c(11, 4, 4, 2))
barplot(t(man_matrix), beside = TRUE, col = rainbow(3), 
        main = "Количество призовых мест мужчин 
        по каждой Олимпиаде", 
        ylab = "Количество мест",
        las = 2)
legend("topright",  legend = paste("Место", 1:3), col = rainbow(3), lty = 1, xpd = TRUE)


woman_matrix <- as.matrix(data[, c(12:14)])
rownames(woman_matrix) <- data$Олимпиада

par(mar = c(11, 4, 4, 2))
barplot(t(woman_matrix), beside = TRUE, col = rainbow(3), 
        main = "Количество призовых мест женщин 
        по каждой Олимпиаде", 
        ylab = "Количество мест",
        las = 2)
legend("topright",  legend = paste("Место", 1:3), col = rainbow(3), lty = 1, xpd = TRUE)


#Круговая
par(mfrow=c(1,2))
filtered_data_m <- data[data$`1-м` > 0, ]
first_places_m <- rowSums(filtered_data_m[, 3])
names(first_places_m) <- filtered_data_m$Олимпиада

par(mar = c(5, 5, 6, 2))
pie(first_places_m, main = "Количество первых мест среди мужчин
    в каждой Олимпиаде", 
    col = rainbow(length(first_places_m)),
    labels = NA)
legend("topright", legend =paste(names(first_places_m), " (", first_places_m, ")", sep = "") , fill = rainbow(length(first_places_m)), cex = 0.8)

filtered_data_w <- data[data$`1-ж` > 0, ]
first_places_w <- rowSums(filtered_data_w[,12])
names(first_places_w) <- filtered_data_w$Олимпиада

par(mar = c(5, 5, 6, 2))
pie(first_places_w, main = "Количество первых мест среди женщин
    в каждой Олимпиаде",  
    col = rainbow(length(first_places_w)),
    labels = NA)
legend("topright", legend =paste(names(first_places_w), " (", first_places_w, ")", sep = "") , fill = rainbow(length(first_places_w)), cex = 0.8)



