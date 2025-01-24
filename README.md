## Wind Turbine and Red Kite Simulation Overview

This project simulates the placement of wind turbines and the distribution of 
red kites within a 100x100 grid over a series of timesteps. The simulation 
accounts for:

- Wind Turbine Placement: Turbines are constructed in clusters for efficiency, 
with a few randomly placed turbines to simulate real-world scenarios.
- Red Kite Dynamics: The population and movement of red kites are modeled using
a stochastic approach (Poisson distribution) combined with the Ricker function 
for population growth.

The goal is to explore the interaction between wind turbine development and the
habitat of red kites.

**Authors: Neele H. and Lukas L.**

**Date: December 2024**

### Project Structure:

The project consists of two main scripts:

- Parameter Initialization (Turbo_region_parameter_setting.R):
        - Defines the grid size, timesteps, initial turbines, and obstacles.
        - Sets up the simulation environment.

- Simulation Model (Turbo_kite_model.R):
        - Implements the simulation logic for turbine placement and red kite 
          dynamics.
        - Includes visualization for each timestep.
     
        
### Requirements

This project requires R and the following libraries:

- `ggplot2`
- `dplyr`


### How to Run:

- Set Up Parameters:
    - Run the `Turbo_region_parameter_setting.R` script to initialize the grid,
      obstacles (placeholder for cities), turbines, and red kites.

- Run the Simulation:
    - Execute the `Turbo_kite_model.R` script to simulate turbine placement and 
      red kite dynamics over specified timesteps.

- Visualize the Results:
    - The simulation will display a series of plots showing the grid, turbine 
      placements (red), and red kites (green) at each timestep.
      

### Future Improvements

- Implement the Baden-Württemberg.shp file as geographical boundaries.
- Implement a buffer zone around wind turbines to better simulate red kite 
  avoidance behavior.
- Add additional ecological constraints for red kite population dynamics.
- Explore the impact of increased turbine density on red kite mortality rates.



