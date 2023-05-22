library(data.table)
library(tidyverse)
library(DBI)
library(dplyr)
library(dbplyr)
library(hrbrthemes)

# Function to create a dose matrix for two compounds
CreateDoseMatrix <- function(CompoundA, MaxDoseA, DirectionA, CompoundB, MaxDoseB, DirectionB) {

	# Ensure CompoundA and CompoundB are integers
	CompoundA <- as.integer(CompoundA)
	CompoundB <- as.integer(CompoundB)

	# If either CompoundA or CompoundB is not an integer, stop the function and return an error message
	if (is.na(CompoundA) || is.na(CompoundB)) {
		stop("CompoundA and CompoundB must be integers.")
	}

	# Define the number of rows and columns for the plate
	Rows <- 8
	Columns <- 12

	# Create a data.table with all combinations of rows and columns
	WellDetails <- CJ(Row = LETTERS[1:Rows], Column = 1:Columns, unique = TRUE)

	# Create a new column 'WellName' that combines the row and column names
	WellDetails[, WellName := paste0(Row, sprintf("%02d", Column))]

	# Calculate the dose for CompoundA based on the direction
	WellDetails[, DoseA := if (DirectionA == "leftward") {
		MaxDoseA / 2^(Columns - Column)
	} else if (DirectionA == "rightward") {
		MaxDoseA / 2^(Column - 1)
	} else {
		stop("Invalid DirectionA specified.")
	}]

	# Calculate the dose for CompoundB based on the direction
	WellDetails[, DoseB := if (DirectionB == "downward") {
		MaxDoseB / 2^(as.numeric(factor(Row)) - 1)
	} else if (DirectionB == "upward") {
		MaxDoseB / 2^(Rows - as.numeric(factor(Row)))
	} else {
		stop("Invalid DirectionB specified.")
	}]

	# Set the dose for the first row and column to 0
	WellDetails[Row == min(Row), DoseB := 0]
	WellDetails[Column == min(Column), DoseA := 0]

	# Add the compound IDs to the data.table
	WellDetails[, CompoundA := CompoundA]
	WellDetails[, CompoundB := CompoundB]

	# Remove leading zeros from the 'WellName' column
	WellDetails[, WellName := gsub("(?<=[A-Z])0+", "", WellName, perl = TRUE)]

	return(WellDetails)
}

# Function to define the contents of each well
DefineWellContents <- function(DoseMatrix, PlateID, StrainID) {

	# Initialize an empty data.table for the plate layout
	PlateLayout <- data.table(
		PlateID = integer(),
		StrainID = integer(),
		Row = character(),
		Column = character(),
		WellName = character(),
		Dose = numeric(),
		CompoundID = integer()
	)

	# Loop over each row in the dose matrix
	for (i in 1:nrow(DoseMatrix)) {

		# Extract the row details
		Row <- DoseMatrix$Row[i]
		Column <- DoseMatrix$Column[i]
		WellName <- DoseMatrix$WellName[i]
		DoseA <- format(DoseMatrix$DoseA[i], scientific = FALSE)
		DoseB <- format(DoseMatrix$DoseB[i], scientific = FALSE)
		CompoundA <- DoseMatrix$CompoundA[i]
		CompoundB <- DoseMatrix$CompoundB[i]

		# Add the details for CompoundA to the plate layout
		PlateLayout <- rbindlist(list(
			PlateLayout,
			data.table(
				PlateID = PlateID,
				StrainID = StrainID,
				Row = Row,
				Column = Column,
				WellName = WellName,
				Dose = DoseA,
				CompoundID = CompoundA
			)
		))

		# Add the details for CompoundB to the plate layout
		PlateLayout <- rbindlist(list(
			PlateLayout,
			data.table(
				PlateID = PlateID,
				StrainID = StrainID,
				Row = Row,
				Column = Column,
				WellName = WellName,
				Dose = DoseB,
				CompoundID = CompoundB
			)
		))
	}

	return(PlateLayout)
}

# Function to prepare the growth data
PrepareGrowthData <- function(filePath, PlateID) {

	# Read the data from the file
	data <- fread(filePath, header = TRUE)

	# Transpose the data and convert it to a data.table
	data <- as.data.table(t(data))

	# Set the column names
	setnames(data, as.character(unlist(data[1, ])))

	# Remove the first row
	data <- data[-1, ]

	# Convert the data to long format
	data <- melt(data, id.vars = "Time [s]", variable.name = "WellName", value.name = "OD600")

	# Exclude the 'Temp. [°C]' row
	data <- data[WellName != "Temp. [°C]", ]

	# Rename the 'Time [s]' column to 'Time'
	setnames(data, "Time [s]", "Time")

	# Convert 'Time' column from character to numeric
	data[, Time := as.numeric(Time)]

	# Convert 'OD600' column from character to numeric with fixed decimal places
	data[, OD600 := round(as.numeric(OD600), 4)]

	# Add the PlateID column
	data[, PlateID := PlateID]

	return(data)
}

# Function to update the database
UpdateDatabase <- function(Plate, WellContents, Measurements, dbPath = "experiments.db") {

	# Connect to the SQLite database
	con <- dbConnect(RSQLite::SQLite(), dbPath)

	# Retrieve column names for Plate, WellContents, and Measurements tables
	PlateColumns <- dbGetQuery(con, "PRAGMA table_info(Plate)")$name
	WellContentsColumns <- dbGetQuery(con, "PRAGMA table_info(WellContents)")$name
	MeasurementsColumns <- dbGetQuery(con, "PRAGMA table_info(Measurements)")$name

	# Insert or update data into the "Plate" table
	dbWriteTable(
		con, "Plate", Plate[, ..PlateColumns],
		row.names = FALSE, overwrite = FALSE, append = TRUE)

	# Insert or update data into the "WellContents" table
	dbWriteTable(
		con, "WellContents", WellContents[, ..WellContentsColumns],
		row.names = FALSE, overwrite = FALSE, append = TRUE)

	# Insert or update data into the "Measurements" table
	dbWriteTable(
		con, "Measurements", Measurements[, ..MeasurementsColumns],
		row.names = FALSE, overwrite = FALSE, append = TRUE)

	# Disconnect from the database
	dbDisconnect(con)
}

