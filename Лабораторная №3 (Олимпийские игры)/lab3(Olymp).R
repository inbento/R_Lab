install.packages("readxl")

library(readxl)

data <- read_excel("N:/Prihodko/3 курс весна/R_labs/R_lab3(data_set).xlsx", sheet = "Лист1"); data

#Задание 2
# Столбчая диаграмма
man_matrix <- as.matrix(data[, c(3:10)])
woman_matrix <- as.matrix(data[, c(12:19)])
rez_matrix <- man_matrix + woman_matrix ; rez_matrix
rownames(rez_matrix) <- data$Олимпиада


par(mar = c(11, 4, 4, 2))
barplot(t(rez_matrix), beside = TRUE, col = rainbow(8), 
        main = "Количество мест 1-8 по каждой Олимпиаде", 
        ylab = "Количество мест",
        las = 2)
legend("topright",  legend = paste("Место", 1:8), col = rainbow(8), lty = 1, xpd = TRUE)



# Круговая диаграмма по количеству первых мест в каждой из олимпиад
filtered_data <- data[data$`1-м` > 0 | data$`1-ж` > 0, ]
first_places <- rowSums(filtered_data[, c(3, 12)])
names(first_places) <- filtered_data$Олимпиада

par(mar = c(5, 5, 6, 2))
pie(first_places, main = "Количество первых мест в каждой Олимпиаде", 
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
pie(first_places_m, main = "Количество мужчин, которые заняли первые места 
    в каждой Олимпиаде", 
    col = rainbow(length(first_places_m)),
    labels = NA)
legend("topright", legend =paste(names(first_places_m), " (", first_places_m, ")", sep = "") , fill = rainbow(length(first_places_m)), cex = 0.8)

filtered_data_w <- data[data$`1-ж` > 0, ]
first_places_w <- rowSums(filtered_data_w[,12])
names(first_places_w) <- filtered_data_w$Олимпиада

par(mar = c(5, 5, 6, 2))
pie(first_places_w, main = "Количество женщин, которые заняли первые места 
    в каждой Олимпиаде",  
    col = rainbow(length(first_places_w)),
    labels = NA)
legend("topright", legend =paste(names(first_places_w), " (", first_places_w, ")", sep = "") , fill = rainbow(length(first_places_w)), cex = 0.8)



