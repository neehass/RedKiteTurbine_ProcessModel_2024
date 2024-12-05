# ---
# Turbinen-Parameter

# region + n hindernisse
# n turbinen

# ---
set.seed(42)

library(ggplot2)

# Dimension ----
timesteps <- 10
x_dim <- 100
y_dim <- 100

# Region + hindernisse ----
region <- array(0, dim = c(x_dim, y_dim, timesteps))
dim(region)

# Hindernisse in Region
n_hinder_1 <- 20
random_x <- sample(1:x_dim, n_hinder_1, replace = 1)  # Random x-coordinates
random_y <- sample(1:y_dim, n_hinder_1, replace = 1)  # Random y-coordinates

# For each random coordinate, set TRUE across all timesteps
for (i in 1:n_hinder_1) {
  region[random_x[i], random_y[i], ] <- 1
}

## Turbinen-Parameter ----
turb_neu_max <- 10   # Max Turbinen, die pro Zeitschritt gebaut werden können
turb_neu_perc <- 0.5  # Neubau-Prozentsatz abhängig von existierenden Turbinen

# Initiale State Turbinen ----
turbine <- array(FALSE, dim = c(x_dim, y_dim, timesteps))  # Kopiere die Region

n_turb_1 <- 10
random_x <- sample(1:x_dim, n_turb_1, replace = TRUE)  # Random x-coordinates
random_y <- sample(1:y_dim, n_turb_1, replace = TRUE)  # Random y-coordinates

# For each random coordinate, set TRUE across all timesteps
for (i in 1:n_turb_1) {
  turbine[random_x[i], random_y[i], ] <- TRUE 
  }



# plot region + tubr t=1 ----
region_layer <- region[,,1]
turbine_layer <- turbine[,,1]
# Combine into a data frame
x_dim <- dim(region_layer)[1]
y_dim <- dim(region_layer)[2]

# Create a data frame with coordinates and values
df <- data.frame(
  x = rep(1:x_dim, each = y_dim),
  y = rep(1:y_dim, times = x_dim),
  region = as.vector(region_layer),
  turbine = as.vector(turbine_layer)
)

# plot
p <- ggplot(df, aes(x = x, y = y)) +
  geom_tile(aes(fill = region), show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "blue", name = "Region") +
  geom_point(data = subset(df, turbine == TRUE), aes(x = x, y = y), color = "red", size = 2) +
  #geom_point(data = subset(df, region == TRUE), aes(x = x, y = y), color = "blue", size = 2) +
  labs(title = paste("Timestep = ", 1), x = "X", y = "Y") +
  theme_minimal()
print(p)

