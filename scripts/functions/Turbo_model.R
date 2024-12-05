# ---
# Turbinen-Model
# erster Versuch
  
# ---

# Dimension ----
timesteps <- 10
x_dim <- 5
y_dim <- 5

# Region + hindernisse ----
col_reg <- colorRampPalette(c("white", "saddlebrown"))(256)
region <- array(0, dim = c(x_dim, y_dim, timesteps))
dim(region)

# Hindernisse in Region
region[1, 2, ] <- 1  
region[2, 2, ] <- 1
image(region[, , 1], col = col_reg)

## Turbinen-Parameter ----
turb_neu_max <- 3     # Max Turbinen, die pro Zeitschritt gebaut werden können
turb_neu_perc <- 0.5  # Neubau-Prozentsatz abhängig von existierenden Turbinen

# Initiale State Turbinen ----
turbine <- array(FALSE, dim = c(x_dim, y_dim, timesteps))  # Kopiere die Region
turbine[2, 3, 1] <- TRUE  # Initiale Turbine 1
turbine[3, 2, 1] <- TRUE  # Initiale Turbine 2

# Zeige die initialen Turbinen
col_turb <- c("white","black")

par(new = TRUE)
image(turbine[, , 1], col = col_turb, axes = FALSE)


# Simulation über die Zeit ----
for (t in 1:(timesteps - 1)) {
  # Anzahl existierender Turbinen
  existing_turbs <- which(turbine[, , t], arr.ind = TRUE)
  n_existing <- nrow(existing_turbs)
  
  # Anzahl neuer Turbinen berechnen
  n_new <- min(ceiling(n_existing * turb_neu_perc), turb_neu_max)
  
  # Neue Turbinen bauen
  for (i in 1:n_new) {
    # Wähle zufällig eine bestehende Turbine als Nachbar aus
    chosen_turb <- existing_turbs[sample(1:n_existing, 1), ]
    x <- chosen_turb[1]
    y <- chosen_turb[2]
    
    # Potenzielle Nachbarn finden
    potential_neighbors <- expand.grid(
      x + c(-1, 0, 1),
      y + c(-1, 0, 1)
    )
    colnames(potential_neighbors) <- c("x", "y")
    
    # Filtere gültige Nachbarn (im Grid und nicht bereits eine Turbine)
    # Filtere gültige Nachbarn (im Grid, kein Hindernis, nicht schon belegt)
    valid_neighbors <- potential_neighbors[
      potential_neighbors$x >= 1 & potential_neighbors$x <= x_dim &
        potential_neighbors$y >= 1 & potential_neighbors$y <= y_dim, ]
    
    valid_neighbors <- valid_neighbors[!apply(valid_neighbors, 1, function(coord) {
      turbine[coord[1], coord[2], t] | region[coord[1], coord[2], t]
    }), ]
    
    # Falls keine gültigen Nachbarn vorhanden sind, überspringen
    if (nrow(valid_neighbors) == 0) next
    
    # Wähle zufällig einen Nachbarn aus
    new_turb <- valid_neighbors[sample(1:nrow(valid_neighbors), 1), ]
    
    # Neue Turbine hinzufügen
    turbine[new_turb$x, new_turb$y, t + 1] <- TRUE
  }
  
  # Kopiere bestehende Turbinen in den nächsten Zeitschritt
  turbine[, , t + 1] <- turbine[, , t] | turbine[, , t + 1]
}

# Visualisierung der Ergebnisse ----
#par(mfrow = c(1, timesteps))
par(mfrow = c(1, 1))
for (t in 1:timesteps) {
  #image(region[, , t], col = col_reg)
  image(turbine[, , t], col = col_turb,
        main = paste("Timestep", t), xlab = "X", ylab = "Y")
  Sys.sleep(0.1)
}


