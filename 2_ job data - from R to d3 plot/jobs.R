library(tidyverse)

# Load data from a comma-separated file. You will need to set the working
# directory to the location of this file on your system, but DO NOT
# include this in your R code. We will be running your scripts on another
# filesystem, and your local paths will not work.
monthly_job_change <- read_csv("bls.txt")

# Add new variables that reformat and reinterpret the existing variables.
# Filter to highlight the Great Recession and its aftermath.
# Note that the "pipe" operator %>% must come at the end of a line.
monthly_job_change <- monthly_job_change %>% 
  mutate(JobChange = as.numeric(str_replace(Value, "\\(P\\)", ""))) %>%
  mutate(Preliminary = str_detect(Value, "\\(P\\)")) %>% 
  mutate(Month = str_replace(Period, "M", "")) %>%
  mutate(Date = as.Date(str_c(Year, "-", Month, "-01"))) %>%
  filter(Date > "2007-01-01")

# Add two geometries, a line and then points above the line
ggplot(monthly_job_change, aes(Date, JobChange)) + 
  geom_line() +
  geom_point(aes(color=Preliminary)) +
  ggtitle("Monthly Job Change") +
  theme(legend.position = "none") # remove the default legend for colors

# Write the output to PDF
ggsave("jobs.pdf")