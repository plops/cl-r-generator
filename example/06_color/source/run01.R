library(colorscience)
library(ggplot2)
# load illuminant data and observer data
d65_data <- colorscience::illuminantD65
cie1931_data <- colorscience::ciexyz31
wavelength <- cie1931_data[,1]
x <- cie1931_data[,2]
y <- cie1931_data[,3]
z <- cie1931_data[,4]
# combine illuminant and chromaticity into single data frame
combined_data <- data.frame(Wavelength = wavelength, X = x, Y = y, Z = z)
d65_data_merged <- merge(combined_data, d65_data, by.x = "Wavelength", by.y = "wlnm", all.x = TRUE)
d65_data_merged$intensity[is.na(d65_data_merged$intensity)] <- 0