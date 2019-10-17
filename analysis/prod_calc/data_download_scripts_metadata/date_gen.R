# Generate vector of dates for python ecmwf download
# John Godlee (johngodlee@gmail.com)
# 2018_10_25

# Create vector of start and end of month
date_vec <- as.Date(
	c(
		rbind(
			seq(from = as.Date("2010-01-01"), to = as.Date("2017-12-01"), by = "month"),
			seq(from = as.Date("2010-02-01"), to = as.Date("2018-01-01"), by = "month") -1
			)
		), 
	origin = "1970-01-01")

# Format as characters
date <- format(date_vec, format = "%Y-%m-%d")

# Write to text file, one date per line
write(date, file = "date_range.txt")
