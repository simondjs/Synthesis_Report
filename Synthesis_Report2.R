library(tidyr) 
library(dplyr)
library(ggplot2)

# Check of tidyr is installed
if (!require("tidyr", quietly = TRUE)) {
  print("Package 'tidyr' is not installed.")
} else {
  print("Package 'tidyr' is installed.")
}

# Set the working directory to the location where the file is stored
setwd("/Users/simonschulze/Desktop/Academics/Critical Thinking")

# Load the CSV file
data <- read.csv("LPD2022_public.csv")

# View the first few rows of the data
head(data)

# Gather names of units
unique(data$Units)

# Filter out data which do not contain 'Population' within their Unit string
filtered_data <- data[grepl("population", data$Units, ignore.case = TRUE), ]

# Gather names of units within filtered_data
unique(filtered_data$Units)

# Define the patterns to filter out
patterns <- c(
  "Population in % of mean",
  "Population in % of the mean",
  "Population density estimates (no. of wallabies/km2)",
  "Population estimate (fish>50cm)",
  "Population abundance age 3 (millions)",
  "Population abundance age 3+ (millions)",
  "Population abundance age 4 (millions)",
  "Population indicies expressing changes in no of b reeding populations calculated as a percentage of the base year pop size.",
  "Population estimate per 100m2",
  "Population density",
  "Population density estimate (no. per 100 km2)",
  "Population index (Individuals per km2)",
  "Population Size (tonnes x 10^7)",
  "Population density (deer/km??)",
  "Population estimate (fish per rkm)"
)

# Filter the dataset
filtered_pop <- filtered_data[!grepl(paste(patterns, collapse = "|"), filtered_data$Units, ignore.case = TRUE), ]

unique(filtered_pop$Units)

# Reshape the dataset from wide to long format
long_data <- pivot_longer(filtered_pop, 
                          cols = starts_with("X"),  # select columns starting with "X"
                          names_to = "Year",       # create a new column "Year" for column names
                          values_to = "Population") # create a new column "Population" for cell values



# Get the class of data in a specific row of a column
class(long_data$Population[1])

# Filter out rows where Population is "NULL"
cleaned_long_data <- long_data %>%
  filter(Population != "NULL")

# Get the class of data in a specific row of a column
class(cleaned_long_data$Year[1])

# Remove 'X' from the Year column and convert to numerical
cleaned_nonx_long_data <- cleaned_long_data %>%
  mutate(Year = as.numeric(sub("^X", "", Year)))

# Check that year is now numeric
class(cleaned_nonx_long_data$Year[1])

# Group by ID, find the maximum Year value for each group, and filter rows accordingly
max_year_data <- cleaned_nonx_long_data %>%
  group_by(ID) %>%
  filter(Year == max(Year)) %>%
  ungroup()

##ERROR because of non-numeric value in population column
# Create a histogram with natural log of Population on the y-axis
ggplot(max_year_data, aes(x = log(Population))) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(x = "Natural Log of Population", y = "Frequency") +
  ggtitle("Histogram of Population (log scale)")


# Check the data types of the Population column
population_types <- sapply(max_year_data$Population, class)
unique_population_types <- unique(population_types)
print(unique_population_types)

# Turn Population numeric
max_year_data <- max_year_data %>%
  mutate(Population = as.numeric(Population))


# Filter the dataset for Population values greater than 0 but under 1
proportion_data <- max_year_data[max_year_data$Population > 0 & max_year_data$Population < 1, ]

# Calculate the proportion of filtered rows compared to the total number of rows
proportion <- nrow(proportion_data) / nrow(max_year_data)

# Print the proportion
print(proportion)

# Remove rows with Population values greater than 0 but under 1
max_year_data <- max_year_data[!(max_year_data$Population > 0 & max_year_data$Population < 1), ]

# Check the updated dataset
head(max_year_data)

# Calculate the percentile rank of each Population value and multiply by 100
max_year_data <- max_year_data %>%
  mutate(Percentile = percent_rank(Population) * 100)

# Check the updated dataset
head(max_year_data)


# Define population values of interest
population_values <- c(1000, 500, 100, 50)

# Calculate the percentile rank of each Population value
max_year_data <- max_year_data %>%
  mutate(Percentile = percent_rank(Population)*100)

# Find the rows closest to each population value
closest_rows <- lapply(population_values, function(value) {
  max_year_data %>%
    mutate(Difference = abs(Population - value)) %>%
    arrange(Difference) %>%
    slice(1)
})

# Combine the results into a data frame
closest_rows_df <- do.call(rbind, closest_rows)

# Print the result
print(closest_rows_df[, c("Population", "Percentile")])



## Visualization attempts

# Create a bar plot with natural log of Population
ggplot(max_year_data, aes(x = log(Population))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Natural Log of Population", y = "Frequency") +
  ggtitle("Bar Plot of Population (log scale)")

# Create a new column with row numbers
max_year_data <- max_year_data %>%
  mutate(Row_Number = row_number())

# Create a bar chart
ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile")




# Create a bar chart with y-axis on a logarithmic scale
ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "skyblue") + 
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10()



# Create a bar chart without weird black axis text
ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "skyblue") +  # Remove bin outline color
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10() +
  theme(axis.text.x = element_blank())  # Remove text labels under x-axis


# Create a bar chart with y-axis on a logarithmic scale
ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = NA) +  # Remove bin outline color
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10() +
  theme(axis.text.x = element_blank(),  # Remove text labels under x-axis
        panel.border = element_blank())  # Remove border around plot area


# Create a bar chart with y-axis on a logarithmic scale
ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = NA) +  # Remove bin outline color
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10() +
  theme(axis.text.x = element_blank(),  # Remove text labels under x-axis
        axis.line.x = element_blank())  # Remove x-axis line

ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "#1f77b4") +  # Change bin outline color to blue
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10() +
  theme(axis.text.x = element_blank(),  # Remove text labels under x-axis
        axis.line.x = element_blank())  # Remove x-axis line


p <- ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "#1f77b4") +  # Change bin outline color to blue
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10() +
  theme(axis.text.x = element_blank(),  # Remove text labels under x-axis
        axis.line.x = element_blank())  # Remove x-axis line

# Add horizontal lines at population values of 50, 100, 500, and 1000
p +
  geom_hline(yintercept = log10(c(50, 100, 500, 1000)), linetype = "dashed", color = "red")


# Create a bar chart with y-axis on a logarithmic scale
p <- ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "#1f77b4") +  # Change bin outline color to blue
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10() +
  theme(axis.text.x = element_blank(),  # Remove text labels under x-axis
        axis.line.x = element_blank())  # Remove x-axis line

# Add horizontal lines at logged population values of 50, 100, 500, and 1000
p +
  geom_hline(yintercept = log10(c(50, 100, 500, 1000)), linetype = "dashed", color = "red")


# Create a bar chart with y-axis on a logarithmic scale
p <- ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "#1f77b4") +  # Change bin outline color to blue
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10() +
  theme(axis.text.x = element_blank(),  # Remove text labels under x-axis
        axis.line.x = element_blank())  # Remove x-axis line

# Log-transform the values for horizontal lines
horizontal_lines <- c(50, 100, 500, 1000)

# Define colors for horizontal lines
line_colors <- c("red", "blue", "green", "purple")

# Add horizontal lines at logged population values of 50, 100, 500, and 1000
p +
  geom_hline(yintercept = horizontal_lines, linetype = "dashed", color = line_colors)



p <- ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "#1f77b4") +  # Change bin outline color to blue
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10() +
  theme(axis.text.x = element_blank(),  # Remove text labels under x-axis
        axis.line.x = element_blank())  # Remove x-axis line

# Create a data frame for horizontal lines
horizontal_data <- data.frame(yintercept = horizontal_lines, color = line_colors)

# Add horizontal lines at population values of 50, 100, 500, and 1000
p +
  geom_hline(data = horizontal_data, aes(yintercept = yintercept, color = color), linetype = "dashed") +
  geom_line(data = horizontal_data, aes(x = NULL, y = NULL, color = color), linetype = "dashed") +
  scale_color_identity(name = "Population", guide = "legend")

____
p <- ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "#1f77b4") +  # Change bin outline color to blue
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10() +
  theme(axis.text.x = element_blank(),  # Remove text labels under x-axis
        axis.line.x = element_blank())  # Remove x-axis line

# Create a data frame for horizontal lines
horizontal_data <- data.frame(yintercept = horizontal_lines, color = line_colors)

# Add horizontal lines at population values of 50, 100, 500, and 1000
p +
  geom_hline(data = horizontal_data, aes(yintercept = yintercept, color = color), linetype = "dashed") +
  geom_segment(data = horizontal_data, aes(x = -Inf, xend = Inf, y = yintercept, yend = yintercept, color = color), linetype = "dashed") +
  scale_color_identity(name = "Ne recommendations", guide = "legend")+
  labs(color = "Population Values")  # Change legend title here
____

# Create a bar chart with y-axis on a logarithmic scale
p <- ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "#1f77b4") +  # Change bin outline color to blue
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10() +
  theme(axis.text.x = element_blank(),  # Remove text labels under x-axis
        axis.line.x = element_blank())  # Remove x-axis line

# Create a data frame for horizontal lines
horizontal_data <- data.frame(yintercept = horizontal_lines, color = line_colors)

# Add horizontal lines at population values of 50, 100, 500, and 1000
p <- p +
  geom_hline(data = horizontal_data, aes(yintercept = yintercept, color = color), linetype = "dashed") +
  geom_segment(data = horizontal_data, aes(x = -Inf, xend = Inf, y = yintercept, yend = yintercept, color = color), linetype = "dashed") +
  scale_color_identity(name = "Ne recommendations", guide = "legend") +
  labs(color = "Population Values")  # Change legend title here

# Add vertical lines at column values representing percentiles of 25, 50, and 75
p + geom_vline(xintercept = c(250, 500, 750), linetype = "dashed", color = "red")


# Create a bar chart with y-axis on a logarithmic scale
p <- ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "#1f77b4") +  # Change bin outline color to blue
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10() +
  theme(axis.text.x = element_blank(),  # Remove text labels under x-axis
        axis.line.x = element_blank())  # Remove x-axis line

# Customize x-axis breaks
p + scale_x_discrete(breaks = seq(0, 100, by = 25))

# Create a data frame for horizontal lines
horizontal_data <- data.frame(yintercept = horizontal_lines, color = line_colors)

p

# Add horizontal lines at population values of 50, 100, 500, and 1000
p +
  geom_hline(data = horizontal_data, aes(yintercept = yintercept, color = color), linetype = "dashed") +
  geom_segment(data = horizontal_data, aes(x = -Inf, xend = Inf, y = yintercept, yend = yintercept, color = color), linetype = "dashed") +
  scale_color_identity(name = "Ne recommendations", guide = "legend") +
  labs(color = "Population Values")  # Change legend title here

# Create a bar chart with y-axis on a logarithmic scale
p <- ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "#1f77b4") +  # Change bin outline color to blue
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10() +
  theme(axis.text.x = element_blank(),  # Remove text labels under x-axis
        axis.line.x = element_blank())  # Remove x-axis line

# Customize x-axis breaks
p <- p + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))

# Create a data frame for horizontal lines
horizontal_data <- data.frame(yintercept = horizontal_lines, color = line_colors)

# Add horizontal lines at population values of 50, 100, 500, and 1000
p <- p +
  geom_hline(data = horizontal_data, aes(yintercept = yintercept, color = color), linetype = "dashed") +
  geom_segment(data = horizontal_data, aes(x = -Inf, xend = Inf, y = yintercept, yend = yintercept, color = color), linetype = "dashed") +
  scale_color_identity(name = "Ne recommendations", guide = "legend") +
  labs(color = "Population Values")  # Change legend title here

p





---------
## Final Plot


# Create a bar chart with y-axis on a logarithmic scale
p <- ggplot(max_year_data, aes(x = reorder(as.character(Row_Number), -Percentile), y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "#1f77b4") +  # Change bin outline color to blue
  labs(x = "Population Percentile", y = "Population") +
  ggtitle("Bar Chart of Population by Percentile") +
  scale_y_log10() +
  theme(axis.text.x = element_blank(),  # Remove text labels under x-axis
        axis.line.x = element_blank())  # Remove x-axis line

# Customize x-axis breaks
p <- p + scale_x_continuous(breaks = c(0, 25, 50, 75, 100))

# Create a data frame for horizontal lines
horizontal_data <- data.frame(yintercept = horizontal_lines, color = line_colors)

# Add horizontal lines at population values of 50, 100, 500, and 1000
p <- p +
  geom_hline(data = horizontal_data, aes(yintercept = yintercept, color = color), linetype = "dashed") +
  geom_segment(data = horizontal_data, aes(x = -Inf, xend = Inf, y = yintercept, yend = yintercept, color = color), linetype = "dashed") +
  scale_color_identity(name = "Ne recommendations", guide = "legend") +
  labs(color = "Population Values")  # Change legend title here

p



