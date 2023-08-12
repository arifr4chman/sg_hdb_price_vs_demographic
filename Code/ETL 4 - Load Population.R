library(tidyverse)
library(arrow)

setwd ('C:\\Users\\rachmaa1\\Documents\\Arif\\GT\\MGT 6203\\Project\\data')

# Read data only on row 11 (header) and 12
population_csv <- read_csv("M810011-table.csv",skip = 10) #, header = T, nrows = 1)

population <- rbind (population_csv [2:19,],population_csv [25,] )

population <- population [,1:9]
view (population)

str (population)


# transpose the data
population2 <- as.data.frame (t(population) )
colnames (population2) <- population2[1,]
population2 <- population2 [2:nrow (population2),]

population2 <- cbind(year = rownames(population2), population2)
rownames(population2) <- 1:nrow(population2)


population2[, "0 - 4 Years"] = as.numeric(population2[, "0 - 4 Years"] )
# population2$year <- as.Date (population2$year, format = '%Y')
view (population2)

# cast all field numeric
columns <- colnames (population2)
# columns <- columns [2:length (columns)]  #Skip year?
x <- 0
for (x in 1:length (columns)) { 
  population2[, columns [x]] = as.numeric(population2[, columns [x]] )
}

population2$children = population2$`0 - 4 Years` + population2$`5 - 9 Years`+
  population2$`10 - 14 Years` + population2$`15 - 19 Years` 
population2$adult = population2$`20 - 24 Years` + population2$`25 - 29 Years`+
  population2$`30 - 34 Years` + population2$`35 - 39 Years` + 
  population2$`40 - 44 Years` + population2$`45 - 49 Years` + 
  population2$`50 - 54 Years` + population2$`55 - 59 Years`

population2$senior_citizen = population2$`60 - 64 Years` + 
  population2$`65 - 69 Years` + population2$`70 - 74 Years` +
  population2$`75 - 79 Years` + population2$`80 - 84 Years` + 
  population2$`85 - 89 Years` + population2$`90 Years & Over`

str (population2)
summary (population2)

write_csv(population2, file='Population.csv')

