getwd()

# install required packages
install.packages("plotly")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
library(plotly)
library(ggplot2)
library(dplyr)
library(readr)

# load required data
states = read_csv("stateAbbreviations.csv")
USdata_df = read.csv("USdata.csv")

# consolidate fuels into renewable and fossil fuels
fossils=c("NUCLEAR", "COAL", "GAS", "PETROLEUM")
USdata_df$fossils<-rowSums(USdata_df[,fossils])
renewables=c("HYDRO", "GEOTHERMAL", "SOLAR", "WIND", "BIOMASSOTHER")
USdata_df$renewables<-rowSums(USdata_df[,renewables])

#merge data to get state codes and select required data
combinedUSdata = merge(states, USdata_df) %>%
  select(YEAR, STATE, Code, fossils, renewables) %>%
  mutate(hover = paste0(STATE, "\n", renewables, "%"))

# variable to help with colour generation
totalEnergy<- combinedUSdata$fossils + combinedUSdata$renewables

# visualisation code
fontStyle = list(
  family = "IBM Plex Mono",
  size = 15,
  color = "black"
)

label = list(
  bgcolor = "#EEEEEE",
  bordercolor = "transparent",
  font = fontStyle
)

# Create Graph
US_graph = plot_geo(combinedUSdata,
                    locationmode = 'USA-states',
                    frame = ~YEAR) %>%
  add_trace(locations = ~Code,
            z = ~renewables,
            zmin = 0,
            zmax = 100,
            color = ~totalEnergy,
            colorscale= 'Viridis',
            text= ~hover,
            hoverinfo = 'text') %>%
  layout(geo = list(scope='usa'),
         font = list(family = "IBM Plex Mono"),
         title = "% of renewable energy generation in US states 2020") %>%
  style(hoverlabel = label) %>%
  config(displayModeBar = FALSE) %>%
  colorbar(title = "% Renewables",
           ticksuffix = "%")

US_graph