
source("checkerboard_setup.R")

plate <- data.table(PlateID = 1, Date = "2022-05-20", Description = "Imipenem/Mupirocin Checkerboard #1") %>% data.table
doseMatrix <- CreateDoseMatrix(27, 1, "leftward", 34, 1000, "upward") %>% data.table
wellContents <- DefineWellContents(doseMatrix, 1, 10081) %>% data.table
measurements <- PrepareGrowthData("raw_data/20230520.tsv", 1)
UpdateDatabase(plate, wellContents, measurements)

plate <- data.table(PlateID = 2, Date = "2022-05-21", Description = "Imipenem/Mupirocin Checkerboard #2")
doseMatrix <- CreateDoseMatrix(27, 1, "leftward", 34, 1000, "upward")
wellContents <- DefineWellContents(doseMatrix, 2, 10081)
measurements <- PrepareGrowthData("raw_data/20230521.tsv", 2)
UpdateDatabase(plate, wellContents, measurements)


