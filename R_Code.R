#Rishi Naidoo a/l Santa Kumar, TP066943

if (!require(tidyr)) {
  install.packages("tidyr")
}

library(tidyr)

#DATA IMPORTING

#importing data frame into RStudio from a CSV file
KL_Data = read.csv("C:\\Users\\WINx10\\OneDrive - Asia Pacific University\\Degree Year 2 Sem 1\\PFDA\\Assignment\\kl_property_data.csv", 
                   header = TRUE)

#renaming columns of the data frame
names(KL_Data) = c("Area", "Cost", "Rooms", "Baths", "Parking_Spaces", "Housing_Type", "Square_Footage", "Furnished_Status")

# Subset the dataset KL_Data based on the condition that "Area" is equal to "Mont Kiara, Kuala Lumpur"
KL_Data <- KL_Data[KL_Data$Area == "Mont Kiara, Kuala Lumpur", ]

#to view the output in tabular format
View(KL_Data)



#DATA CLEANING & PRE - PROCESSING

#keep only unique rows
KL_Data = unique(KL_Data)

#Rename the "Cost" column to "Cost_RM"
colnames(KL_Data)[colnames(KL_Data) == "Cost"] <- "Cost_RM"

#Remove commas from the "Cost_RM" column, and "RM" from the values
KL_Data$Cost_RM <- gsub("[, ]", "", gsub("RM", "", KL_Data$Cost_RM))

#Convert "Cost_RM" column to numerical values
KL_Data$Cost_RM <- as.numeric(KL_Data$Cost_RM)



#Replace character "Studio" with numerical value 1 in the "Rooms" column
KL_Data$Rooms <- gsub("Studio", 1, KL_Data$Rooms)

#Clean the "Rooms" column by removing non-numeric characters and patterns like "+1", "+2", etc.
KL_Data$Rooms <- gsub("\\D", "", gsub("\\+\\d", "", KL_Data$Rooms))

#Convert "Rooms" column to numerical values
KL_Data$Rooms <- as.numeric(KL_Data$Rooms)



#Remove anything within brackets in the "Housing_Type" column
KL_Data$Housing_Type <- gsub("\\s*\\([^\\)]+\\)", "", KL_Data$Housing_Type)

# Replace "Bungalow Land" with "Bungalow" in the "Housing_Type" column
KL_Data$Housing_Type <- gsub("Bungalow Land", "Bungalow", KL_Data$Housing_Type)

#Remove "number-sty" from the beginning of the "Housing_Type" column
KL_Data$Housing_Type <- gsub("^\\d+(\\.\\d+)?-sty\\s", "", KL_Data$Housing_Type)



#Separate "Square_Footage" into two columns, "Property_Type" and "Size," using " : " as the separator
KL_Data <- separate(KL_Data, col = Square_Footage, into = c("Property_Type", "Size"), sep = " : ")

#Define a pattern to filter rows in the "Size" column with the format "numeric sq.ft." allowing for commas and spaces
pattern <- "^\\d+(,\\d+)*(\\s*sq\\.\\s*ft\\.)?$"

#Filter rows based on the specified format in the "Size" column
KL_Data <- KL_Data[grepl(pattern, KL_Data$Size), ]

#Rename the "Size" column to "Square_Footage"
colnames(KL_Data)[colnames(KL_Data) == "Size"] <- "Square_Footage"

#Remove commas and spaces from the "Square_Footage" column, and "sq. ft." from the values
KL_Data$Square_Footage <- gsub("[, ]", "", gsub(" sq\\. ft\\.", "", KL_Data$Square_Footage))

#Convert "Square_Footage" column to numerical values
KL_Data$Square_Footage <- as.numeric(KL_Data$Square_Footage)



#DATA VALIDATION

#Summary for "Cost_RM" column (275,000 - 3,500,000)
summary(KL_Data$Cost_RM)

#Summary for "Rooms" column (1 - 5)
summary(KL_Data$Rooms)

#Summary for "Baths" column (1 - 6)
summary(KL_Data$Baths)

#Summary for "Parking_Spaces" column (1 - 3)
summary(KL_Data$Parking_Spaces)

#Summary for "Square_Footage" column (900 - 8100)
summary(KL_Data$Square_Footage)

# Get unique values of the "Housing_Type" column
unique(KL_Data$Housing_Type)

# Get unique values of the "Furnished_Status" column
unique(KL_Data$Furnished_Status)



# Replace NA and empty values in the 'Cost_RM' column with 0
KL_Data$Cost_RM[is.na(KL_Data$Cost_RM) | KL_Data$Cost_RM == ""] <- 0

# Replace NA and empty values in the 'Rooms' column with 0
KL_Data$Rooms[is.na(KL_Data$Rooms) | KL_Data$Rooms == ""] <- 0

# Replace NA and empty values in the 'Baths' column with 0
KL_Data$Baths[is.na(KL_Data$Baths) | KL_Data$Baths == ""] <- 0

# Replace NA and empty values in the 'Parking_Spaces' column with 0
KL_Data$Parking_Spaces[is.na(KL_Data$Parking_Spaces) | KL_Data$Parking_Spaces == ""] <- 0

# Replace NA and empty values in the 'Square_Footage' column with 0
KL_Data$Square_Footage[is.na(KL_Data$Square_Footage) | KL_Data$Square_Footage == ""] <- 0

# Replace NA and empty values in the 'Housing_Type' column with "Unknown"
KL_Data$Housing_Type[is.na(KL_Data$Housing_Type) | KL_Data$Housing_Type == ""] <- "Unknown"

# Replace NA and empty values in the 'Furnished_Status' column with "Unknown"
KL_Data$Furnished_Status[is.na(KL_Data$Furnished_Status) | KL_Data$Furnished_Status == ""] <- "Unknown"



#Set seed for reproducibility
set.seed(1)

#Replace values in the "Cost_RM" column that are less than 275000 or greater than 3500000 with random integers within the range [275000, 3500000]
KL_Data$Cost_RM <- ifelse(KL_Data$Cost_RM < 275000, round(runif(sum(KL_Data$Cost_RM < 275000), 275000, 3500000)),
                          ifelse(KL_Data$Cost_RM > 3500000, round(runif(sum(KL_Data$Cost_RM > 3500000), 275000, 3500000)),
                                 KL_Data$Cost_RM))

# Set seed for reproducibility
set.seed(2)

# Replace values in the "Rooms" column that are greater than 5 or less than 1 with random integers within the range [1, 5]
KL_Data$Rooms <- ifelse(KL_Data$Rooms > 5 | KL_Data$Rooms < 1, round(runif(sum(KL_Data$Rooms > 5 | KL_Data$Rooms < 1), 1, 5)),
                        KL_Data$Rooms)

# Set seed for reproducibility
set.seed(3)

# Replace values in the "Baths" column that are greater than 6 or less than 1 with random integers within the range [1, 6]
KL_Data$Baths <- ifelse(KL_Data$Baths > 6 | KL_Data$Baths < 1, round(runif(sum(KL_Data$Baths > 6 | KL_Data$Baths < 1), 1, 6)),
                        KL_Data$Baths)

# Set seed for reproducibility
set.seed(4)

# Replace values in the "Parking_Spaces" column that are greater than 3 or less than 1 with random integers within the range [1, 3]
KL_Data$Parking_Spaces <- ifelse(KL_Data$Parking_Spaces > 3 | KL_Data$Parking_Spaces < 1, round(runif(sum(KL_Data$Parking_Spaces > 3 | KL_Data$Parking_Spaces < 1), 1, 3)),
                                 KL_Data$Parking_Spaces)

# Set seed for reproducibility
set.seed(5)

# Replace values in the "Square_Footage" column that are less than 900, greater than 8100, or less than 1 with random integers within the range [900, 8100]
KL_Data$Square_Footage <- ifelse(KL_Data$Square_Footage < 900 | KL_Data$Square_Footage > 8100 | KL_Data$Square_Footage < 1, 
                                 round(runif(sum(KL_Data$Square_Footage < 900 | KL_Data$Square_Footage > 8100 | KL_Data$Square_Footage < 1), 900, 8100)),
                                 KL_Data$Square_Footage)

#Set seed for reproducibility
set.seed(6)

#Replace "Unknown" in the "Furnished_Status" column with either "Partly Furnished," "Fully Furnished," or "Unfurnished" randomly assigned
KL_Data$Furnished_Status <- ifelse(KL_Data$Furnished_Status == "Unknown",
                                   sample(c("Partly Furnished", "Fully Furnished", "Unfurnished"), sum(KL_Data$Furnished_Status == "Unknown"), replace = TRUE),
                                   KL_Data$Furnished_Status)

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# INDIVIDUAL PART

# Rishi Naidoo a/l Santa Kumar, TP066943

# Objective 2: To Investigate the Relationship Between Number of Rooms and Selected Key Aspects of Properties

# Libraries
library(ggplot2)
#install.packages("ggplot2")
library(dplyr)
#install.packages("dplyr")
library(plotrix)
#install.packages("plotrix")
library(ggpattern)
#install.packages("ggpattern")
library(hrbrthemes)
#install.packages("hrbrthemes")
library(rgl)
#install.packages("rgl")
library(plot3D)
#install.packages("plot3D")
library(viridis)
#install.packages("viridis")
library(plotly)
#install.packages("plotly")

# Filtering the dataset for properties
Properties_Data <- KL_Data

# Analysis 1: Count properties based on the number of rooms
# Analysis 1.1 How many properties have less than 3 rooms?
properties_less_than_3_rooms <- sum(Properties_Data$Rooms < 3)
# Analysis 1.2 How many properties have more or equal than 3 rooms?
properties_3_or_more_rooms <- sum(Properties_Data$Rooms >= 3)

# Analysis 2: Count properties based on square footage ranges
# Analysis 2.1 How many properties are in the range 900 - 2700?
properties_900_2700_sqft <- sum(Properties_Data$Rooms >= 3 & Properties_Data$Square_Footage > 900 & Properties_Data$Square_Footage < 2700)
# Analysis 2.2 How many properties are in the range 2700 - 4500?
properties_2700_4500_sqft <- sum(Properties_Data$Rooms >= 3 & Properties_Data$Square_Footage > 2700 & Properties_Data$Square_Footage < 4500)
# Analysis 2.3 How many properties are in the range 4500 - 6300?
properties_4500_6300_sqft <- sum(Properties_Data$Rooms >= 3 & Properties_Data$Square_Footage > 4500 & Properties_Data$Square_Footage < 6300)
# Analysis 2.4 How many properties are in the range  6300 - 8100?
properties_6300_8100_sqft <- sum(Properties_Data$Rooms >= 3 & Properties_Data$Square_Footage > 6300 & Properties_Data$Square_Footage < 8100)

# Analysis 3: Count properties based on housing type and price criteria
# Analysis 3.1 How many properties are Condominiums?
condos_count <- sum(Properties_Data$Rooms >= 3 & Properties_Data$Square_Footage > 900 & Properties_Data$Square_Footage < 2700 & Properties_Data$Housing_Type == "Condominium")
# Analysis 3.2 How many properties are Bungalows?
bungalows_count <- sum(Properties_Data$Rooms >= 3 & Properties_Data$Square_Footage > 900 & Properties_Data$Square_Footage < 2700 & Properties_Data$Housing_Type == "Bungalow")
# Analysis 3.3 How many properties are Serviced-Residences ?
serviced_residences_count <- sum(Properties_Data$Rooms >= 3 & Properties_Data$Square_Footage > 900 & Properties_Data$Square_Footage < 2700 & Properties_Data$Housing_Type == "Serviced Residence")
# Analysis 3.4 How many properties are Semi-detached Houses?
semi_detached_count <- sum(Properties_Data$Rooms >= 3 & Properties_Data$Square_Footage > 900 & Properties_Data$Square_Footage < 2700 & Properties_Data$Housing_Type == "Semi-detached House")
# Analysis 3.5 How many properties are Terrace/Link Houses?
terrace_link_houses_count <- sum(Properties_Data$Rooms >= 3 & Properties_Data$Square_Footage > 900 & Properties_Data$Square_Footage < 2700 & Properties_Data$Housing_Type == "Terrace/Link House")
# Analysis 3.6 How many properties are Townhouses?
townhouses_count <- sum(Properties_Data$Rooms >= 3 & Properties_Data$Square_Footage > 900 & Properties_Data$Square_Footage < 2700 & Properties_Data$Housing_Type == "Townhouse")

# Analysis 4: Count properties based on price criteria for condominiums
# Analysis 4.1 How many properties are priced more than 1.2 million?
condos_high_priced <- sum(Properties_Data$Rooms >= 3 & Properties_Data$Square_Footage > 900 & Properties_Data$Square_Footage < 2700 & Properties_Data$Housing_Type == "Condominium" & Properties_Data$Cost_RM >= 1200000)
# Analysis 4.2 How many properties are priced less than 1.2 million?
condos_low_priced <- sum(Properties_Data$Rooms >= 3 & Properties_Data$Square_Footage > 900 & Properties_Data$Square_Footage < 2700 & Properties_Data$Housing_Type == "Condominium" & Properties_Data$Cost_RM < 1200000)

# Displaying results
# Printing Analysis 1: Pie Chart
cat("Analysis 1.1: Properties with fewer than 3 rooms -", properties_less_than_3_rooms, "\n")
cat("Analysis 1.2: Properties with 3 or more rooms -", properties_3_or_more_rooms, "\n\n")

# Create Data
room <- data.frame(
  room_range = c("Properties less than 3", "Properties 3 or more"),
  data_room = c(properties_less_than_3_rooms, properties_3_or_more_rooms)
)

# Compute the position of labels
data <- room %>%
  arrange(desc(room_range)) %>%
  mutate(prop = data_room / sum(data_room) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop)

# Basic pie chart with hover feature using plot_ly
plot_ly(data, labels = ~room_range, 
        values = ~prop, 
        type = "pie", 
        text = ~paste("Value: ", data_room),
        hoverinfo = "text+label", 
        marker = list(colors = RColorBrewer::brewer.pal(3, "Set3")))

# Printing Analysis 2: Horizontal Lollipop Chart
cat("Analysis 2.1: Properties with 3 or more rooms and 900-2700 sqft -", properties_900_2700_sqft, "\n")
cat("Analysis 2.2: Properties with 3 or more rooms and 2700-4500 sqft -", properties_2700_4500_sqft, "\n")
cat("Analysis 2.3: Properties with 3 or more rooms and 4500-6300 sqft -", properties_4500_6300_sqft, "\n")
cat("Analysis 2.4: Properties with 3 or more rooms and 6300-8100 sqft -", properties_6300_8100_sqft, "\n\n")

# Create Data
sqft <- data.frame(
  sqft_range=c("900-2700", "2700-4500", "4500-6300", "6300-8100"),
  values=c(properties_900_2700_sqft, properties_2700_4500_sqft, properties_4500_6300_sqft, properties_6300_8100_sqft)
)

# Lollipop Chart Horizontal version
ggplot(sqft, aes(x=sqft_range, y=values)) +
  geom_segment( aes(xend=sqft_range, yend=0), color="#00cc85", size=1.5) +
  geom_point( color="#ccffcc", size=5, alpha=0.4) +
  geom_text(aes(label=paste0(values, " (", scales::percent(values / sum(values)), ")")),
            vjust=-0.75, size=4,col="#CCFF66") +
  theme_ft_rc() +
  coord_flip() +
  labs(title="Analysis 2", x="Square Feet", y="Values")

# Printing Analysis 3: Textured Histogram
cat("Analysis 3.1: Condominiums with >3 rooms, 900-2700 sqft -", condos_count, "\n")
cat("Analysis 3.2: Bungalows with >3 rooms, 900-2700 sqft -", bungalows_count, "\n")
cat("Analysis 3.3: Serviced Residences with >3 rooms, 900-2700 sqft -", serviced_residences_count, "\n")
cat("Analysis 3.4: Semi-detached Houses with >3 rooms, 900-2700 sqft -", semi_detached_count, "\n")
cat("Analysis 3.5: Terrace/Link Houses with >3 rooms, 900-2700 sqft -", terrace_link_houses_count, "\n")
cat("Analysis 3.6: Townhouses with >3 rooms, 900-2700 sqft -", townhouses_count, "\n\n")

# Create Data
houses = data.frame(
  housing_type=c("Condominiums","Bungalows","Serviced-Residence","Semi-detached Houses","Terrace/Link Houses","Townhouses"),
  data_housing=c(condos_count, bungalows_count, serviced_residences_count, semi_detached_count, terrace_link_houses_count, townhouses_count)
)
ggplot(houses, aes(x=housing_type, y=data_housing, fill=housing_type)) +
  geom_bar(stat="identity", alpha=.6, width=.4) +
  scale_fill_grey(start=0, end=0.75) +
  theme_bw()

ggplot(houses, aes(x=housing_type, y=data_housing)) +
  geom_col_pattern(
    aes(pattern=housing_type,
       pattern_angle=housing_type,
        pattern_spacing=housing_type
    ), 
    fill            = "#8EACF3",
    colour          = "#101138", 
    pattern_density = 0.25, 
    pattern_fill    = "#FFD57A",
    pattern_colour  = "#FF7276"
  ) +
  theme_minimal() +
  geom_text(aes(label=paste0(data_housing, " (", scales::percent(data_housing / sum(data_housing)), ")")),
            vjust=-0.75, size=3,col="red") +
  labs(title="Analysis 3")

# Printing Analysis 4: Doughnut Graph
cat("Analysis 4.1: Condominiums with >3 rooms, 900-2700 sqft, priced at 1.2 million or more -", condos_high_priced, "\n")
cat("Analysis 4.2: Condominiums with >3 rooms, 900-2700 sqft, priced less than 1.2 million -", condos_low_priced, "\n")

# Create Data
price <- data.frame(
  price_range=c("More than 1.2 million", "Less than 1.2 million"),
  count=c(condos_high_priced, condos_low_priced)
)

# Compute percentages
price$fraction = price$count / sum(price$count)

# Compute the cumulative percentages (top of each rectangle)
price$ymax = cumsum(price$fraction)

# Compute the bottom of each rectangle
price$ymin = c(0, head(price$ymax, n=-1))

# Make the plot
ggplot(price, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=price_range)) +
  geom_rect(col="white") +
  coord_polar(theta="y") +
  geom_text(aes(x = 3.5, y = (ymin + ymax) / 2,
                label = paste0("Count: ", count, "\n", round((ymax - ymin) * 100, 1), "%")),
            color = "#29437F", size = 3.5, inherit.aes = FALSE, hjust = 0.5, vjust = 0.5) +
  xlim(c(2, 4)) +
  theme_void() +
  scale_fill_brewer(palette=14)
  labs(title="Analysis 4")
