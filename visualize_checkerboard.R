source("checkerboard_setup.R")

# Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), "experiments.db")

# Read the entire Plate table
Plate <- dbReadTable(con, "Plate")

# Read the entire WellContents table
WellContents <- dbReadTable(con, "WellContents")

# Read the entire Measurements table
Measurements <- dbReadTable(con, "Measurements")


# Disconnect from the database
dbDisconnect(con)


Measurements <- Measurements %>%
	group_by(WellName, PlateID) %>%
	mutate(OD600 = OD600 - min(OD600))

tFinal <- Measurements %>% select(PlateID, Time) %>% unique() %>% mutate(Diff = abs(Time - 60*60*12)) %>%
	group_by(PlateID) %>% slice_min(Diff)


doc_theme <- theme_ipsum(
	base_family = "Arial",
	caption_margin = 12,
	axis_title_size = 12,
	axis_col = "black")

WellContents %>%
	filter(CompoundID == 34 & Dose == 1000) %>%
	select(WellName, PlateID) %>%
	inner_join(WellContents %>% filter(CompoundID == 27)) %>%
	inner_join(Measurements) %>%
	inner_join(tFinal) %>%
	mutate(Dose = round(Dose, 5)) %>%
	ggplot(aes(x = as.factor(Dose), y = OD600)) +
	geom_boxplot() +
	xlab("Imipenem Dose (ng/µL)") +
	ylab("OD600") +
	ggtitle(expression(italic("Acinetobacter baumannii")~"Growth")) +
	labs(subtitle = "in 1000 ng/µL Mupirocin at 12 Hours") +
	doc_theme +
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

