# - - - - - - - - - - - - - - - - #
# Author: Nathaniel Peyer         #
# Date: 10 November 2026          #
# Title Lab7 Part II              #
#                                 #
# This file was used for the      #
# creation of a histogram and the #
# calculations for part II of my  #
# MET 335 Instrumentation class   #
# lab number 7.                   #
# - - - - - - - - - - - - - - - - #


library(ggplot2)

# Importing the washer size data.
washer <- read.csv(
  "C:/Users/nate_/source/repos/MET335/LAB7/Data/met335_lab7_washers Peyer, N - Sheet1.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Checking the imported data.
head(washer)
str(washer)
colnames(washer)

# Creating the histogram of number vs diameter.
ggplot(washer, aes(x = Washer.Size..mm., weight = Number.of.Washers)) +
  geom_histogram(
    binwidth = 1,
    fill = "steelblue",
    color = "black"
  ) +
  labs(
    title = "Distribution of Washers as an Analog to hydrometeor Sizes",
    x = "Washer Size (mm)",
    y = "Frequency"
  ) +
  theme_minimal()


# Calculating the volume of the washers.
washer$Radius <- washer$Washer.Size..mm. / 2
washer$Volume.Per.Washer <- (4/3) * pi * washer$Radius^3
washer$Volume.Per.Size <- washer$Volume.Per.Washer * washer$Number.of.Washers

# Creating the histogram of Volume vs diameter.
ggplot(washer, aes(x = Volume.Per.Size, weight = Washer.Size..mm.)) +
  geom_histogram(
    binwidth = 0.5,
    fill = "steelblue",
    color = "black"
  ) +
  labs(
    title = "Volume of Washers vs Washer Diameter",
    x = "Volume of Washers (mm^3)",
    y = "Diameter of Washers (mm)"
  )
theme_minimal()

# Calculating the total number of washers.
washTotal <- sum(washer$Number.of.Washers)
print(paste("Total number of washers:", washTotal))

# Calculating the total volume of washers.
volTotal <- sum(washer$Volume.Per.Size)
print(paste("Total volume of washers:", volTotal))

# Computing the weighted average of washer diameter (Eq1).
# Eq1: d_n = sum(N_i d_i) / sum(N_i)
# N_i = total number of hydrometeors
# d_i = diameter of the hydrometeors
WeightedAvgWashDia <- sum(washer$Number.of.Washers * washer$Washer.Size..mm.) / sum(washer$Number.of.Washers)
print(paste("The weighted average of washer diameter:", WeightedAvgWashDia))

# Computing the weighted average of washer volume (Eq2).
# Eq2: d_v = sum(v_i * d_i) / sum(v_i)
# v_i  = total volume of hydrometeors of a given size
# d_i = diameter of the hydrometeors
WeightedAvgWashVol <- sum(washer$Volume.Per.Size * washer$Washer.Size..mm.) / sum(washer$Washer.Size..mm.)
print(paste("The weighted average of washer volume", WeightedAvgWashVol))

# Plotting Number of washers per mm.

