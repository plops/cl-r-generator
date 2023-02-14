# PTC from multiple images

Q: Given a set of 100 dark and bright images display the photon transfer
curve (PTC) of the camera. Print conversion gain, read noise in
electrons and full well capacity. The full well capacity is the number
of electrons where the PTC starts deviating from linear behaviour. Use
R.

A: To plot the photon transfer curve (PTC) in R, you can follow these steps:

    Load the required packages:

```R

library(dplyr)
library(ggplot2)
```

    Import the images and extract the pixel values for each image. You can use the imager package to do this:

```R

library(imager)

# Read in the images
images <- load.image("path/to/images/folder/*")

# Convert the images to grayscale
gray_images <- grayscale(images)

# Extract the pixel values for each image
pixel_values <- array(0, dim = c(nrow(gray_images), ncol(gray_images), nimages))
for (i in 1:nimages) {
  pixel_values[,,i] <- as.array(gray_images[,,i])
}
```

    Compute the mean and variance of the pixel values for each image:

```R

mean_values <- apply(pixel_values, c(1,2), mean)
var_values <- apply(pixel_values, c(1,2), var)
```
    Compute the conversion gain as the slope of the PTC:

```R

ptc_data <- data.frame(mean_values, var_values)
ptc_fit <- lm(var_values ~ mean_values, data = ptc_data)
conversion_gain <- coef(ptc_fit)[2]
```
    Compute the read noise as the standard deviation of the residuals of the PTC fit:

```R

read_noise <- sd(residuals(ptc_fit))
```

    Plot the PTC and identify the full well capacity as the point where the PTC deviates from linear behavior:

```R

ggplot(ptc_data, aes(mean_values, var_values)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Mean signal (electrons)", y = "Variance (electrons^2)") +
  geom_vline(xintercept = sqrt(2)*conversion_gain, linetype = "dashed") +
  annotate("text", x = sqrt(2)*conversion_gain, y = max(var_values), label = "Full well capacity") 
```

This will produce a plot of the PTC with a dashed vertical line
indicating the full well capacity. The conversion gain, read noise,
and full well capacity can be obtained from the output of the previous
steps.

# PTC from non-uniform image

Q: Now assume that we have a set of 30 gray images. The images show a
defocused edge (white on the right, black on the left). The edge is
the same in every image. The images just contain different shot
noise. Fit a sigmoid function to the edge. Assume that the noise is
quasi poisson-distributed. Estimate the conversion gain. Plot the PTC
curve. Use R.

A: To fit a sigmoid function to the edge of the gray images, you can
follow these steps:

    Load the required packages:

```R

library(dplyr)
library(ggplot2)
```
    Import the images and extract the pixel values for each image. You can use the imager package to do this:

```R

library(imager)

# Read in the images
images <- load.image("path/to/images/folder/*")

# Extract the pixel values for each image
nimages <- dim(images)[3]
edge_pixel_values <- array(0, dim = c(nrow(images), nimages))
for (i in 1:nimages) {
  edge_pixel_values[,i] <- as.array(images[,ncol(images)/2,i])
}
```
    Compute the mean and variance of the pixel values for each image:

```R

mean_values <- apply(edge_pixel_values, 2, mean)
var_values <- apply(edge_pixel_values, 2, var)
```
    Compute the quasi-Poisson variance for each mean value:

```R

quasi_poisson_var <- mean_values + var_values^2 / mean_values
```
    Fit a sigmoid function to the edge using the quasi-Poisson variance as weights:

```R

edge_data <- data.frame(x = 1:nrow(images), y = mean_values)
edge_fit <- nls(y ~ SSlogis(x, Asym, xmid, scal), 
                data = edge_data, 
                weights = 1/quasi_poisson_var,
                start = c(Asym = max(mean_values), xmid = nrow(images)/2, scal = nrow(images)/10))
```
    Extract the edge parameters from the fit:

```R

edge_params <- coef(edge_fit)
```
    Compute the conversion gain from the edge parameters:

```R

conversion_gain <- edge_params[1] / edge_params[3]
```
    Plot the edge and the fitted sigmoid function:

```R

ggplot(edge_data, aes(x, y)) +
  geom_point() +
  stat_function(fun = function(x) edge_params[1] * plogis((x - edge_params[2])/edge_params[3]), color = "red") +
  labs(x = "Position (pixels)", y = "Mean signal (counts)") 
```

This will produce a plot of the edge with the fitted sigmoid function.

To plot the PTC curve, you can follow the previous steps, but use the
variance of the mean values instead of the variance of the edge pixel
values. The conversion gain can be estimated from the slope of the
PTC.