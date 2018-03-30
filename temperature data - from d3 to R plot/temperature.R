#Read csv file
#Set Value and Anomaly fields to numbers, the Date as a string.
data<- read_csv("noaa-central-park.csv", col_types = cols(
                  Date = col_character(),
                  Value = col_double(),
                  Anomaly = col_double()
                ))

# Mutate the Date field into a Year field by extracting the first four digits.
data <- data %>% 
  mutate(Year = str_extract(Date, "^\\d\\d\\d\\d"))

# Group all the temperature readings by Year.
temperatures <- data %>% group_by(Year)

# Summarize the year-groups to get their mean, min, and max
yearSummary <- temperatures %>% summarise(
  Mean = mean(Anomaly),
  Max = max(Anomaly),
  Min = min(Anomaly)
)

# Add two geometries, a line and then points above the line
ggplot(yearSummary, aes(Year, Mean, group = 1)) + 
  # Add a ribbon geometry showing the span between min and max yearly anomalies.
  geom_ribbon(aes(ymin=Min, ymax=Max, x=Year), fill = "grey80") +
  # Now add a line geometry showing the mean.
  geom_line() +
  # change axis labels
  scale_y_continuous(breaks=seq(-12, 14, 2)) + 
  scale_x_discrete(breaks=seq(1900, 2010, 10)) + 
  # make background transparent, remove the grid
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  # Add title
  ggtitle("Yearly Temperature Anomalies")

# Write the output to PDF
ggsave("temperature.pdf")

