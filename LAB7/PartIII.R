# - - - - - - - - - - - - - - - - #
# Author: Nathaniel Peyer         #
# Date: 10 November 2026          #
# Title Lab7 Part III             #
#                                 #
# This file was used for the      #
# creation of a histogram and the #
# calculations for part III of my #
# MET 335 Instrumentation class   #
# lab number 7.                   #
# - - - - - - - - - - - - - - - - #

# Top row fall speed
# V col is the size

library(readxl)
library(ggplot2)

# Importing the raw data.
rawDistDF <- read_excel("C:/Users/nate_/source/repos/MET335/LAB7/Data/20251019_0800_0859lt_dist.xls", 
                        col_names = FALSE)

# Extracting the date.
velocity <- as.numeric(unlist(raw[1, 2:21]))
diameter <- as.numeric(unlist(raw[3:24, 22]))
counts   <- as.matrix(raw[3:24, 2:21])

# Cast particle counts to numeric type.
storage.modee(counts) <- "numeric"

# Taking care of blank cells as 0s.
counts[is.na(counts)] <- 0

# Calculations
A  <- 0.00565 # Area of the disdrometer
dt <- 60      # Measurement Interval (delta t)

# Eq5: 6 * pi * 10^(-4) * sum(sum((D_i^3 * n_ij / A * dt))
# D_i = Mean diameter of particle bin (mm)
# n_ij = Particle count in each bin
# A = Measuring area (m^2)
# dt = Measurement interval (seconds)
diameterCounts <- rowSums(counts)
rainSum <- sum(diameter^3 * diameterCounts)

rainRate <- (6 * pi * 10^(-4) * rainSum) / (A * dt)

print(paste("Rain Rate:", round(rainRate, 3), "mm/hr"))

# Eq7: Z = sum(sum((D_i^6 * n_ij) / (A * dt * V__j)))
# V_j = Fall velocity of particle bin (m/s)
velocityAdjusted <- sweep(counts, MARGIN = 2, STATS = velocity, FUN = "/")
diameterAdjusted <- sweep(velocityAdjusted, MARGIN = 1, STATS = diameter^6, FUN = "*")
reflectivitySum <- sum(diameterAdjusted)

z <- reflectivitySum / (A * dt)
dBZ <- 10 * log10(z)

print(paste("Reflectivity Factor:", round(z, 4)))
print(paste("Radar Reflectivity:", round(dBZ, 2), "dBZ"))

# Creating table of results for the python script.
results <- data.frame(Measurment = c("Rain Rate", "Reflectivity Factor", "Radar Reflectivity"),
                      Value = c(rainRate, z, dBz),
                      Units = c("mm/hr", "mm^6/m^(-3)", "dBZ"))

print(results)

# Particle Size Distribution Plot.
distribution <- data.frame(Diameter = diameter, Count = diameterCounts)

# Generating the plot.
particlePlot <- ggplot(distribution, aes(x = Diameter, y = Count)) +
                geom_co(fill = "steelblue", color = black) +
                labs(title = "Disdrometer Particle Size Distribution",
                     subtitle = "19 October 2025",
                     x = "Particle Diameter (mm)",
                     y = "Number of Particles")+  
                theme_minimal()

print(particlePlot)
ggsave("Lab7_Particle_Distribution.png", plot = particlePlot, width = 10, height  = 6, dpi = 300)

