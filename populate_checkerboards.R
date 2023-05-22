source("checkerboard_setup.R")

# Setup experiment 0
experiment0 <- SetUpExperiment(
	PlateID = 0,
	Date = "2023-05-19",
	Description = "Imipenem/Mupirocin Checkerboard #0",
	DoseMatrix = CreateDoseMatrix(34, 1000, "leftward", 27, 2, "upward"),
	StrainID = 10081,
	FilePath = "raw_data/20230519.tsv"
)
UpdateDatabase(experiment0$Plate, experiment0$WellContents, experiment0$Measurements)

# Setup experiment 1
experiment1 <- SetUpExperiment(
	PlateID = 1,
	Date = "2023-05-20",
	Description = "Imipenem/Mupirocin Checkerboard #1",
	DoseMatrix = CreateDoseMatrix(27, 1, "leftward", 34, 1000, "upward"),
	StrainID = 10081,
	FilePath = "raw_data/20230520.tsv"
)
UpdateDatabase(experiment1$Plate, experiment1$WellContents, experiment1$Measurements)

# Setup experiment 2
experiment2 <- SetUpExperiment(
	PlateID = 2,
	Date = "2023-05-21",
	Description = "Imipenem/Mupirocin Checkerboard #2",
	DoseMatrix = CreateDoseMatrix(27, 1, "leftward", 34, 1000, "upward"),
	StrainID = 10081,
	FilePath = "raw_data/20230521.tsv"
)
UpdateDatabase(experiment2$Plate, experiment2$WellContents, experiment2$Measurements)
