library(colorscience)
library(ggplot)
# load illuminant data and observer data
d65_data <- colorscience--illuminantD65
cie1931_data <- colorscience--ciexyz31
wavelength <- cie1931_data[,0]
x <- cie1931_data[,1]
y <- cie1931_data[,2]
z <- cie1931_data[,3]
# combine illuminant and chromaticity into single data frame
combined_data <- data.frame(Wavelength = wavelength, X = x, Y = y, Z = z)
d65_data_merged <- merge(combined_data, d65_data, by.x = "Wavelength", by.y = "wlnm", all.x = TRUE)
d65_data_merged$intensity[is.na(d65_data_merged$intensity)] <- 0
# plot illuminant and chromaticity
png("01_input.png")
((ggplot(d65_data_merged))+(geom_line(aes(x = Wavelength, y = X, color = "X")))+(geom_line(aes(x = Wavelength, y = Y, color = "Y")))+(geom_line(aes(x = Wavelength, y = Z, color = "Z")))+(geom_line(aes(x = Wavelength, y = D65, color = "intensity")))+(scale_color_manual(values = c ("X" = "red", "Y" = "green", "Z" = "blue", "D65" = "purple")))+(labs(x = "Wavelength (nm)", y = "Value", title = "CIE 1931 Chromaticity Coordinates and D65 Illuminant vs. Wavelength", color = "Data"))+(theme_minimal()))
dev.off()