# ggvis package
library(ggvis)
library(dplyr)

# Scatterplots
head(mtcars)
mtcars %>% ggvis(~wt, ~mpg) %>% layer_points()

mtcars %>% 
  ggvis(~wt, ~mpg) %>% 
  layer_points(size := 25, shape := "diamond", stroke := "red", fill := NA)

# Adding regression line
mtcars %>% 
  ggvis(~wt, ~mpg) %>%
  layer_points() %>%
  layer_smooths()

# Scatterplots with grouping
mtcars %>% 
  ggvis(~wt, ~mpg) %>% 
  layer_points(fill = ~factor(cyl))

# Bar graphs
head(pressure)

pressure %>% 
  ggvis(~temperature, ~pressure) %>%
  layer_bars()

# Changing bars width
pressure %>% 
  ggvis(~temperature, ~pressure) %>%
  layer_bars(width = 15)

# Working with categorical variables on X axis
pressure2 <- pressure %>% mutate(temperature = factor(temperature))

pressure2 %>% ggvis(~temperature, ~pressure) %>%
  layer_bars()

# Line Graphs
pressure %>% ggvis(~temperature, ~pressure) %>% layer_lines()

# Histograms
head(faithful)

faithful %>% ggvis(~eruptions) %>% layer_histograms()

# Modifying attributes
faithful %>% ggvis(~eruptions, fill := "#fff8dc") %>%
  layer_histograms(binwidth = 0.25) %>%
  add_axis("x", title = "eruptions") %>%
  add_axis("y", title = "count")

# Interaction (http://ggvis.rstudio.com/interactivity.html)
mtcars %>% 
  ggvis(~wt, ~mpg, 
        size := input_slider(10, 100),
        opacity := input_slider(0, 1)
  ) %>% 
  layer_points()







