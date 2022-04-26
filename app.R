
################ IMPORT AND CLEAN DATA #########################################################

library(tidyverse) #used for general data manipulation
library(here) #used to set a relative working directory
library(rvest) #used for webscraping
library(rgdal) #read the shape file

here() #this sets the relative working directory to the current file location

###### import data ######################################################


### world happiness index scores 2021 ###

## webscrape data
link <-  "https://countryeconomy.com/demography/world-happiness-index" #website url
page <-  read_html(link)

country <-  page %>% html_nodes("td:nth-child(1)") %>% html_text() #scrapes the country column

happiness_score <-  page %>% html_nodes(".numero+ .numero") %>% html_text() #scrapes the WHI score column

whi <-  data.frame(country, happiness_score, stringAsFactors = FALSE) #combines the scraped columns into a new dataframe


## clean dataframe
whi$country_sub <- str_sub(
  whi$country, 
  start = 1, end = -5
) #removes [+] symbol from country names

whi <- whi %>% 
  select(country_sub, happiness_score) %>% #removes stringASFactors and country columns
  rename(country = country_sub) #renames country_sub back to country

whi$happiness_score <- as.numeric(whi$happiness_score) #convert score from character to integer


## Population Density ##

#read population density
pop <- read.csv(here('data','worldbank_pop.csv'), 
                skip = 4, header = T) #removes first 4 irrelevant cells from file


#remove uneeded dates
pop <- pop %>% 
  select(Country.Name, X2020, Country.Code) %>% #edit pop to only include country.name and 2020
  rename(country = Country.Name,
         pop_density = X2020) #rename country name and 2020 column header

## GDP ##

#read gdp data
gdp <- read.csv(here('data', 'gdp_worldbank.csv'), 
                skip = 4, header = T) #removes first 4 irrelevant cells from file

#remove uneeded variables
gdp <- gdp %>% 
  select(Country.Name, X2020, Country.Code) %>% #edit pop to only include country.name and 2020
  rename(country = Country.Name,
         gdp = X2020) #rename country name and 2020 column header


## Covid ##

#read covid stringency data

covid <- read.csv(here('data', 'covid-stringency-index.csv')) #reads data

#create new column to average the data and rename columns
covid <- covid %>% group_by(Code) %>% #groups the countries together
  summarise(avg_covid_score = mean(stringency_index)) %>% #finds average stingency index score across 
  rename(Country.Code = Code) #renames country column 


##### join data to become one df ###########################

#### create cleaning functions #######

## Find country row index function ##
which_country <- function(df, name){ #this function inserts the df name and country name to return its row index in the df
  which(df$country == name, arr.ind=TRUE)
}

## change name function ##
country_change <- function(df, index, name) { #this inputs the df name, row index as found using the which_country function and the new name to update the value with
  df[index, 1] = name  #updates column one of index x with name
  return(df) #returns a new df with the updated value to be assigned back to the df
}


#Join data to check NA values
check <- left_join(whi, gdp, by= 'country') #left joins first dataframe to left


#some country names are incongruent so these will need to be changed in either df in order to match

#whi
#Ivory Coast
index <- which_country(whi, 'Ivory Coast')
whi <- country_change(whi, index, 'Cote d\'Ivoire')

#Myanmar
index <- which_country(whi, 'Burma - Myanmar')
whi <- country_change(whi, index, 'Myanmar')

#Swaziland
index <- which_country(whi, 'Swaziland')
whi <- country_change(whi, index, 'Eswatini')

#gdp and pop
#The Congo
index <- which_country(gdp, 'Congo, Rep.')
gdp <- country_change(gdp, index, 'Republic of the Congo')

#Egypt
index <- which_country(gdp, 'Egypt, Arab Rep.')
gdp <- country_change(gdp, index, 'Egypt')

#The Gambia
index <- which_country(gdp, 'Gambia, The')
gdp <- country_change(gdp, index, 'The Gambia')

#Hong Kong
index <- which_country(gdp, 'Hong Kong SAR, China')
gdp <- country_change(gdp, index, 'Hong Kong')

#Iran
index <- which_country(gdp, 'Iran, Islamic Rep.')
gdp <- country_change(gdp, index, 'Iran')

#Kyrgyzstan
index <- which_country(gdp, 'Kyrgyz Republic')
gdp <- country_change(gdp, index, 'Kyrgyzstan')

#South Korea
index <- which_country(gdp, 'Korea, Rep.')
gdp <- country_change(gdp, index, 'South Korea')

#Laos
index <- which_country(gdp, 'Lao PDR')
gdp <- country_change(gdp, index, 'Laos')

#Russia
index <- which_country(gdp, 'Russian Federation')
gdp <- country_change(gdp, index, 'Russia')

#Venezuela
index <- which_country(gdp, 'Venezuela, RB')
gdp <- country_change(gdp, index, 'Venezuela')

#Yemen
index <- which_country(gdp, 'Yemen, Rep.')
gdp <- country_change(gdp, index, 'Yemen')

#Slovakia
index <- which_country(gdp, 'Slovak Republic')
gdp <- country_change(gdp, index, 'Slovakia')

#palestine
index <- which_country(gdp, 'West Bank and Gaza')
gdp <- country_change(gdp, index, 'State of Palestine')

##### fully join the data #####

join1 <- full_join(whi, gdp, by= 'country') #left joins first dataframe to left

#Taiwan need country codes inserted to be joined to pop and covid data
index <- which_country(join1, 'Taiwan')
join1[index, 4] = 'TWN'

# full join all data by Country.Code
df <- full_join(join1, pop, by = 'Country.Code')
df <- full_join(df, covid, by = 'Country.Code')

df <- df %>% select(-c(country.y, country = country.x)) #rename columns


### Create Table to show number of missing values per column ###
total_missing <- sum(is.na(df))
country_missing <- sum(is.na(df$country))
country_code_missing <- sum(is.na(df$Country.Code))
happiness_score_missing <- sum(is.na(df$happiness_score))
pop_density_missing <- sum(is.na(df$pop_density))
gdp_missing <- sum(is.na(df$gdp))
avg_covid_score_missing <- sum(is.na(df$avg_covid_score))

missing <-  data.frame(country_code_missing, country_missing,  happiness_score_missing, pop_density_missing, gdp_missing, avg_covid_score_missing, total_missing) # this data frame shows number of NAs per variable


## add logged pop_density column ##

df$pop_density_log = df$pop_density #create duplicate column
df$pop_density_log = log(df$pop_density_log) #log entire column


## add thousands seperator and round to 2dp##
df$commagdp = df$gdp
df$commagdp <- format(round(as.numeric(df$commagdp), 2), nsmall = 1, big.mark = ",") 


###### merged data to a geospatial shape file ###########

# Read this shape file with the rgdal library. 
world_spdf <- readOGR( 
  dsn= paste0(here('data','world_shape_file')), 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

#Join DF to spatial data
#Must use merge function when joining a spatial and non spatial DF

#Full join to detect non-congruent names
worldCountries <- merge(world_spdf, df, by.x = "ISO3", by.y = "Country.Code", all.x = TRUE) #all = TRUE dictates a left join from world_spdf


#### convert geospatial data to normal df so ggplot can read it ####

## create worldCountries df
scatterdf <- worldCountries@data %>%  
  select(c(ISO3, NAME, happiness_score, gdp, pop_density, avg_covid_score, pop_density_log, commagdp)) #omits geospatial data

scatterdf <- scatterdf %>%  rename(Country = NAME)

########################### VARIABLES #########################################################

#for map variables
library(leaflet) #create leaflet map
library(RColorBrewer) #create custom palette for map
library(dplyr) #for data manipulation

#for ggplotly
library(ggplot2) #creation of base ggplot graph
library(plotly) #ggplotly function
library(readr) #saves the ggplotly widget
library(ggthemes) #allows for custom ggplot themes
library(htmlwidgets) #saves ggplotly to html
library(sysfonts) #downloads google font to R from computer

# import google font from computer. Ensure all ttf fonts are downloaded from the font_download folder first.
font_add_google("Source Sans Pro")


################### dashboard variables ###########################

#### map variables #########

### general aesthetics ####

### label information ###
mytext <- paste(
  "Country: ", worldCountries@data$NAME,"<br/>", #label key and data
  "Happiness Score: ", round(worldCountries@data$happiness_score, 2), "<br/>", #data rounded to 2 dp
  "GDP per Capita: $", worldCountries@data$commagdp, "<br/>",
  "Population Density: ", round(worldCountries@data$pop_density, 2), "<br/>",
  "Covid Stringency: ", round(worldCountries@data$avg_covid_score, 2), "<br/>",
  sep=""
) %>%
  lapply(htmltools::HTML)


### label design ###
label <- labelOptions( 
  style = list("font-weight" = "normal", padding = "3px 8px"), 
  textsize = "13px", 
  direction = "auto"
) 

### Highlight Options ###
highlight <- highlightOptions(
  weight = 2, #line width
  color = "white",
  fillOpacity = 1.0,
  opacity = 1.0,
  bringToFront = TRUE)

#### Create Variables for WHI ###########

#custom colour palette
mypalettewhi <- colorBin( palette="YlOrRd", domain = worldCountries@data$happiness_score, bins = 9) #colorBin indicates that the palette will be placed into bins

#### Create Variables for gdp #########

#custom colour palette
mypalettegdp <- colorBin( palette="YlGn", domain=worldCountries@data$gdp, bins = 9)

#custom highlight options as gdp has white lines instead of black
highlightgdp <- highlightOptions(
  weight = 2,
  color = "black",
  fillOpacity = 1.0,
  opacity = 1.0,
  bringToFront = TRUE)


### Create Variables for Population Density #######

#custom colour palette
mypalettepop <- colorBin( palette="Purples", domain=worldCountries@data$pop_density_log, bins = 9)

### Create Variables for covid ################

#Custom colour palette
mypalettecovid <- colorBin(palette="Blues", domain = worldCountries@data$avg_covid_score, bins = 9)

#### dataframe for data explorer #################################

#select only relevant columns
cleantable <- worldCountries@data %>% 
  select(c(NAME, ISO3, happiness_score, gdp, pop_density, avg_covid_score))

#rename columns to intuitive names
cleantable <- cleantable %>% rename(country = NAME,
                                    population_density = pop_density,
                                    covid_score = avg_covid_score)



####### ggplotly graphs #########################################################

### Create global variables for pop up label settings ###

## label font
font = list(
  size = 15,
  color = "white"
)

## label size and color
labelplot = list(
  bgcolor = "#232F34",
  bordercolor = "transparent",
  font = font
)

####### create ggplotly graphs ##################################

#### GDP ############################################

### ggplot ###

#create base graph
scatter_gdp <- ggplot(scatterdf, aes(x = gdp, y = happiness_score, label = Country)) + #adding text removes geom_smooth
  geom_point(colour = '#005a32', alpha = 0.7, size = 2.5) +
  geom_smooth(method = 'lm', se = FALSE, colour = 'gold2', alpha = 0.6, size = 1)

#add labels
scatter_gdp <- scatter_gdp +
  labs(title = "Relationship Between a Country's GDP per Capita and Happiness", 
       x = "Gross Domestic Product per Capita (US$)",
       y = "World Happiness Index Score") 

#add theme
scatter_gdp <- scatter_gdp + 
  theme_gdocs() + #custom theme from ggtheme
  theme(
    text = element_text(family = "Source Sans Pro"),
    plot.title = element_text(size = 16, face = 'bold'),
    plot.subtitle = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12))


## create ggplotly ##
graph_gdp.interactive = ggplotly(scatter_gdp, tooltip = 'Country')%>%
  style(hoverlabel = labelplot) %>%
  layout(
    font = font,
    xaxis = list(fixedrange = TRUE), #disables ability to scroll around plot
    yaxis = list(fixedrange = TRUE,
                 title = list(x= 0, xanchor = 'left', xref = 'container'))) %>% #edits title position
  config(displayModeBar = FALSE, showTips = FALSE) #disables fetures bar


#save graph
saveWidget(graph_gdp.interactive, here('graphs', "gdp_interactive.html"))


#### Population Density ############################################

## ggplot ##

#create base graph
scatter_pop <- ggplot(scatterdf, aes(x = pop_density, y = happiness_score, label = Country)) +
  geom_point(colour = '#6a51a3', alpha = 0.7, size = 2.5) +
  geom_smooth(method = 'lm', se = FALSE, colour = 'gold2', alpha = 0.6, size = 1)

#add labels
scatter_pop <- scatter_pop +
  labs(title = "Relationship Between a Country\'s Population Density and Happiness",
       x = "Population Density (km\u00B2)",
       y = "World Happiness Index Score")

scatter_pop <- scatter_pop + 
  theme_gdocs() + #custom theme
  theme(
    text = element_text(family = "Source Sans Pro"),
    plot.title = element_text(size = 16, face = 'bold'),
    plot.subtitle = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12))

## create ggplotly ##
graph_pop.interactive = ggplotly(scatter_pop, tooltip = 'Country')%>%
  style(hoverlabel = labelplot) %>%
  layout(font = font, 
         xaxis = list(fixedrange = TRUE), 
         yaxis = list(fixedrange = TRUE)) %>%
  config(displayModeBar = FALSE, showTips = FALSE)


#save graph
saveWidget(graph_pop.interactive, here('graphs', "pop_interactive.html"))

#### Population Logged #########################################################

### ggplot ###

#create base graph
scatter_poplog <- ggplot(scatterdf, aes(x = pop_density_log, y = happiness_score, label = Country)) +
  geom_point(colour = '#6a51a3', alpha = 0.7, size = 2.5) +
  geom_smooth(method = 'lm', se = FALSE, colour = 'gold2', alpha = 0.6, size = 1)

#add labels
scatter_poplog <- scatter_poplog +
  labs(title = "Relationship Between a Country\'s Population Density and Happiness \n Adjusted for Outliers (Log Transformed)",
       x = "Logged Population Density (km\u00B2)",
       y = "World Happiness Index Score") 

#add theme
scatter_poplog <- scatter_poplog + 
  theme_gdocs() + 
  theme(
    text = element_text(family = "Source Sans Pro"),
    plot.title = element_text(size = 16, face = 'bold'),
    plot.subtitle = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12))

## create ggplotly ##
graph_poplog.interactive = ggplotly(scatter_poplog, tooltip = 'Country')%>%
  style(hoverlabel = labelplot) %>%
  layout(font = font, 
         xaxis = list(fixedrange = TRUE), 
         yaxis = list(fixedrange = TRUE)) %>%
  config(displayModeBar = FALSE, showTips = FALSE)

#save graph
saveWidget(graph_poplog.interactive, here('graphs', "poplog_interactive.html"))

#### Covid #########################################

#create base graph
scatter_covid <- ggplot(scatterdf, aes(x = avg_covid_score, y = happiness_score, label = Country)) + #adding text removes geom_smooth
  geom_point(colour = '#084594', alpha = 0.7, size = 2.5) +
  geom_smooth(method = 'lm', se = FALSE, colour = 'gold2', alpha = 0.6, size = 1)

#add labels
scatter_covid <- scatter_covid +
  labs(title = "Relationship Between a Country's Covid-19 Lockdown Severity and Happiness", 
       x = "Covid-19 Stringency Index Score \n (Higher scores Indicate more Extreme Measures)",
       y = "World Happiness Index Score") 

#add theme
scatter_covid <- scatter_covid + 
  theme_gdocs() + 
  theme(
    text = element_text(family = "Source Sans Pro"),
    plot.title = element_text(size = 16, face = 'bold'),
    plot.subtitle = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12))

#create ggplotly
graph_covid.interactive = ggplotly(scatter_covid, tooltip = 'Country')%>%
  style(hoverlabel = labelplot) %>%
  layout(font = font, 
         xaxis = list(fixedrange = TRUE), 
         yaxis = list(fixedrange = TRUE)) %>%
  config(displayModeBar = FALSE, showTips = FALSE)

#save graph
saveWidget(graph_covid.interactive, here('graphs', "covid_interactive.html"))

####### map graphs ##########################################

#### WHI Histogram ####
whi_map <- ggplot(scatterdf, aes(x = happiness_score)) + 
  geom_histogram(colour = 'white', fill = '#b10026', bins = 35) +
  theme_classic() +
  theme(
    axis.text.y=element_blank(),  #remove y axis labels
    axis.ticks.y=element_blank(),  #remove y axis ticks
    text = element_text(family = "Source Sans Pro"),
    plot.title = element_text(face = 'bold')
  ) + 
  labs(title = "Global Distribution of Happiness", 
       x = "World Happiness Score",
       y = "Number of Countries") 

#save graph
ggsave(here('graphs', 'happiness_hist.png'), plot = whi_map, height = 15, width = 26, units = 'cm')

###### gdp scatter ######
gdp_map <- ggplot(scatterdf, aes(x = gdp, y = happiness_score)) + 
  geom_point(colour = '#005a32', alpha = 0.7, size = 2.5) +
  geom_smooth(method = 'lm', se = FALSE, colour = 'gold2', alpha = 0.6, size = 1) +
  theme_classic() +
  theme(
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.text.y=element_blank(),  #remove y axis labels
    axis.ticks.y=element_blank(),  #remove y axis ticks
    text = element_text(family = "Source Sans Pro"),
    plot.title = element_text(face = 'bold')
  ) + 
  labs(title = "GDP per Capita vs Global Happiness", 
       x = "GDP per Capita (US$)",
       y = "World Happiness Score") 

#save graph
ggsave(here('graphs', 'gdp_vs_happiness.png'), plot = gdp_map, height = 15, width = 26, units = 'cm')

##### pop density scatter ####
pop_map <- ggplot(scatterdf, aes(x = pop_density_log, y = happiness_score, label = Country)) +
  geom_point(colour = '#6a51a3', alpha = 0.7, size = 2.5) +
  geom_smooth(method = 'lm', se = FALSE, colour = 'gold2', alpha = 0.6, size = 1) +
  theme_classic() +
  theme(
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.text.y=element_blank(),  #remove y axis labels
    axis.ticks.y=element_blank(),  #remove y axis ticks
    text = element_text(family = "Source Sans Pro"),
    plot.title = element_text(face = 'bold')
  ) + 
  labs(title = "Population Density (logged) vs\n Global Happiness", 
       x = "Population Density (log transformed)",
       y = "World Happiness Score") 

ggsave(here('graphs', 'population_vs_happiness.png'), plot = pop_map, height = 15, width = 26, units = 'cm')

##### covid scatter ####
covid_map <- ggplot(scatterdf, aes(x = avg_covid_score, y = happiness_score, label = Country)) + #adding text removes geom_smooth
  geom_point(colour = '#084594', alpha = 0.7, size = 2.5) +
  geom_smooth(method = 'lm', se = FALSE, colour = 'gold2', alpha = 0.6, size = 1) +
  theme_classic() +
  theme(
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.text.y=element_blank(),  #remove y axis labels
    axis.ticks.y=element_blank(),  #remove y axis ticks
    text = element_text(family = "Source Sans Pro"),
    plot.title = element_text(face = 'bold')
  ) + 
  labs(title = "Covid-19 Lockdown Severity vs\n Global Happiness", 
       x = "Covid-19 Stringency Score",
       y = "World Happiness Score")

#save graph
ggsave(here('graphs', 'covid_severity_vs_happiness.png'), plot = covid_map, height = 15, width = 26, units = 'cm')


############### DASHBOARD CODE ##############################################################

library(shiny) #creates shiny app
library(leaflet) #creates leaflet map
library(RColorBrewer) #allows for custom pallettes
library(dplyr) #data manipulation
library(shinydashboard) #create shiny dashboard
library(htmltools) #for creating html
library(DT) #add data table to dashboard
library(shinythemes) #adds premade themes
library(plotly) #render plotly

#### UI #############################################################


ui <- navbarPage(theme = shinytheme('flatly'), collapsible = TRUE,
                 
                 
                 HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">World Happiness Dashboard</a>'), id="nav",
                 
                 windowTitle = "World Happiness Dashboard",
                 
                 
                 ## First tab content. Inserts map within box ##################
                 tabPanel("Map", icon = icon("globe"),
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                          leafletOutput("map", #output is leaflet map
                                        height="100%", width = "100%"),
                          
                          ### adds tab panel to map ###
                          
                          ## settings of tab panel for graphs
                          absolutePanel(
                            id = "controls", class = "panel panel-default",
                            bottom = 26, right = 26, fixed=FALSE, #sets default position
                            width = 300, height = 270, #sets size
                            draggable = TRUE,  #allows to be draggable
                            ## fills tab panel with plot             
                            plotOutput("scatterplot", height = 270, width = 300)
                          ),
                 
                 # tab panel for title
                 absolutePanel(id = "title", class = "panel panel-default",
                               top = 92, left = 65, width = 590, fixed=TRUE,
                               draggable = FALSE, height = "auto",
                               
                               (h4(strong("Global Levels of GDP per Capita, Population Density and Covid-19 Lockdown Severity, and their Relation to World Happiness"))), style="color:#045a8d"),
)
),
                 
                 
                 ## second tab content. Creates scatter plots ###################
                 tabPanel("Scatter Plots", icon = icon("chart-line"), #tab group name
                          fluidRow(
                            ## uses a tab box as multiple subtabs within the box ##
                            tabBox( 
                              width = 12, height = 500, #tab box size
                              id="tabchart1",
                              
                              ## tab panel one uses headings to create text 
                              tabPanel("Overview", 
                                       #overview heading
                                       HTML('<h1 style = "color:orange">Overview</h1>'), 
                                       
                                       h3('These scatter plots show each of the three variables\' relationships with a country\'s world happiness index score.'), 
                                       
                                       h3('Navigate each variable using the corresponding tab and hover over datapoints to reveal the country it represents.'), 
                                       
                                       h3('An alternative scatter plot for population density is provided, to show the relationship if the effects of outliers (Hong Kong and Singapore) are minimised. This was achieved by performing a log transformation on population density.')),
                              
                              #subsequent panels display plotly graphs as created previously
                              tabPanel("GDP per Capita", 
                                       plotlyOutput("plot_gdp", height = "100%", width = "100%")),
                              
                              tabPanel("Population Density", 
                                       plotlyOutput("plot_pop", height = "100%", width = "100%")),
                              
                              tabPanel("Population Density (Adjusted)", 
                                       plotlyOutput("plot_pop_adj", height = "100%", width = "100%")),
                              
                              tabPanel("Covid-19 Stringency", 
                                       plotlyOutput("plot_covid", height = "100%", width = "100%"))
                            )
                          )
                 ),
                 
                 
                 ## third tab content. Creates a table ######################
                 tabPanel('Data Explorer', icon = icon("list", lib = "glyphicon"),
                          fluidPage(
                            HTML('<h1 style = "color:orange">Data Explorer</h1>'), #main title in orange
                            h4('Click on column headings or use the search tool to explore the dataset'), #subtitle created using heading
                            box(
                              width = NULL, height = 510, #width = null to fill out the page
                              status='primary', #highlights in blue
                              dataTableOutput("maptable") #maptable is output
                            )
                          )
                 ),
                 
                 
                 ## Final Tab Content. Textbox for overview ############
                 tabPanel('About', icon = icon("info"),
                          
                          fluidPage(width = 60, #tab is an open page document
                                    

                                    
                                    #HTML functionality to display text and change colour and format the page    
                                    HTML('<center><h1 style = "background-color:orange; color:white">Dashboard Information</h1></center>'),
                                    #adds smiling world image
                                    img(src="https://i.guim.co.uk/img/media/16ff6eca7df464c60bfa1d87f1388b80249c8ae2/0_775_3753_2252/master/3753.jpg?width=940&quality=45&auto=format&fit=max&dpr=2&s=d1d049bd2716c11ecb029329290c766b", 
                                        height="50%", width="50%", #reduces image size
                                        align = 'right'), #positions image to right
                                
                                    
                                    #heading functions used to write content
                                    h4("This dashboard uses a map to visualise global levels of happiness, GDP per capita, population density and covid-19 response stringencies. The map features supporting graphs to visualise each variable\'s relationship with global happiness."),
                                    
                                    h4("Moreover, full scale interactive scatter plots are provided, to further invetsigate the relationships a country\'s GDP per capita, population density and Covid-19 response measures have with its happiness."),
                                    
                                    h4("Finally, a data explorer is provided for investigation of particular countries or searching of countries with extreme scores on these factors."),
                                    
                                    h4("Each visualisation is interactive, so the reader is encouraged to freely explore them."),
                                    
                                    
                                    
                                    br(), br(), br(), br(), br(), #br() indicates breaks. E.g. two breaks creates larger space
                                    
                                    ## Variables heading ##
                                    HTML('<center><h2 style = "background-color:orange; color:white">Variables</h2></center>'),
                                    
                                    br(),
                                    
                                    #WHI
                                    HTML('<h3 style = "color:orange">World Happiness Index Report</h2>'),
                                    h4("The World Happiness Report is an annual report that inspects the state of global happiness in 156 countries. It is carried out by the United Nations in collaboration with a group of universities, and measures subjective ratings of happiness within a country."),
                                    
                                    h4("Data is primarily used from the Gallup World Poll. Nationally representative samples of respondents are asked to think of a ladder, with the best possible life for them being a 10, and the worst possible life being a 0. They are then asked to rate their own current lives on that 0 to 10 scale. The report correlates the life evaluation results with various life factors. These life factor variables are: GDP, life expectancy, generosity, social support, freedom and corruption."),
                                    
                                    h4("This project analysed data from the World Happiness Report 2021", a("(link here).", href= "https://worldhappiness.report/ed/2021/", target="_blank")),
                                    
                                    #adds url link
                                    h4("Data was web-scraped from the website countryeconomy.com", a("(link here).", href= "https://countryeconomy.com/demography/world-happiness-index", target="_blank")), #target = blank so link opens in new window
                                    
                                    br(),
                                    
                                    ## GDP ##
                                    HTML('<h3 style = "color:orange">Gross Domestic Product (GDP) per Capita</h2>'),
                                    h4('Gross domestic product is a monetary measure of the market value of all the final goods and services produced in a specific time period by countries. GDP per capita shows a country\'s GDP divided by its total population. It is therefore a measure of a country\'s relative wealth.'),
                                    
                                    h4("This project measured GDP in US$."),
                                    
                                    #URL link
                                    h4("Data was sourced from the World Bank, for the year 2020", a("(link here).", href= "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD", target="_blank")),
                                    br(),
                                    
                                    ## pop density ##
                                    HTML('<h3 style = "color:orange">Population Density</h2>'),
                                    
                                    h4("Population density is the concentration of individuals in a specific country."),
                                    h4("Population density was measured per km\u00B2 of land area."),
                                    
                                    #URL link
                                    h4("Data was sourced from the World Bank, for the year 2020", a("(link here).", href= "https://data.worldbank.org/indicator/EN.POP.DNST?view=chart", target="_blank")),
                                    
                                    br(),
                                    
                                    ## covid ##
                                    HTML('<h3 style = "color:orange">Covid-19 Stringency</h2>'),
                                    
                                    h4("The Oxford Covid-19 Government Response Tracker (OxCGRT) collects systematic information on policy measures that governments have taken to tackle COVID-19. It is published by the University of Oxford and Blavatnik School of Government. It is essentially a measure of the severity of a country\'s covid-19 lockdown measures."), 
                                    
                                    h4("The stringency index is a composite measure based on nine response indicators including school closures, workplace closures, and travel bans, rescaled to a value from 0 to 100 (100 = strictest). This project averaged data records from 21st January 2020 till 20th March 2022, to define a country\'s average Covid-19 response stringency during this time."),
                                    
                                    h4("Data was sourced from the Covid-19 Government Response Tracker project, published by the University of Oxford and Blavatnik School of Government", a("(link here).", href= "https://www.bsg.ox.ac.uk/research/research-projects/covid-19-government-response-tracker", target="_blank")),
                                    
                                    br(),
                                    
                                    ## data and code heading ##
                                    HTML('<center><h2 style = "background-color:orange; color:white">Data and Code</h2></center>'), 
                                    
                                    br(),
                                    
                                    #URL link
                                    h4("Data and R code for this dashboard can be sourced from this github link", a("(link here).", href= "https://github.com/lukejenner6/PSY6422_Project", target="_blank"))
                          )
                 )
)


#### Server ###################
server <- function(input, output, session) {
  
  
  #### output ####
  ## output: leaflet map
  output$map <- renderLeaflet({
    
    map <- worldCountries %>% #geospatial data frame
      leaflet() %>%
      addTiles() %>%
      
      setView( lat=10, lng=0 , zoom=2) %>% #sets default map pan
      
      setMaxBounds( lng1 = -180, lat1 = -87, #stops the map being dragged out of bounds
                    lng2 = 200, lat2 = 97 ) %>% 
      
      ### happiness map #########
    addPolygons(
      data = worldCountries,
      fillColor = ~mypalettewhi(happiness_score), #custom palette
      stroke=TRUE, 
      fillOpacity = 0.9, #how faint ploygons are
      color="white", #line colour
      weight=0.7, #size of lines
      label = mytext, #label text variable
      labelOptions = label, #label aesthetics variable
      highlightOptions = highlight,
      group = "World Happiness Index" #group identifier
    ) %>%
      
      ### GDP #################
    addPolygons(
      data = worldCountries,
      fillColor = ~mypalettegdp(gdp), 
      stroke=TRUE, 
      fillOpacity = 0.9, 
      color="black", #black is added as highlight as yellows will blend
      weight=0.7,
      label = mytext,
      labelOptions = label,
      highlightOptions = highlightgdp,
      group = "GDP per Capita"
    ) %>%
      
      ### population #################
    addPolygons(
      data = worldCountries,
      fillColor = ~mypalettepop(pop_density_log), #log is used to diminish extreme values
      stroke=TRUE, 
      fillOpacity = 0.9, 
      color="white", 
      weight=0.7,
      label = mytext,
      labelOptions = label,
      highlightOptions = highlight,
      group = 'Population Density'
    ) %>% 
      
      ### covid stingency ##############
    addPolygons( 
      data = worldCountries,
      fillColor = ~mypalettecovid(avg_covid_score), 
      stroke=TRUE, 
      fillOpacity = 0.9, 
      color="white", 
      weight=0.7,
      label = mytext,
      labelOptions = label,
      highlightOptions = highlight,
      group = "Covid-19 Stringency Score"
    ) %>%
      
      #### controls group layers #########
    addLayersControl(
      baseGroups = c("World Happiness Index", "GDP per Capita", "Population Density", "Covid-19 Stringency Score"), #base groups indicates these will be toggled between groups
      options = layersControlOptions(collapsed = FALSE) 
    ) %>%
      
      #### creates default legend for WHI to be shown ####
    addLegend( 
      values=~happiness_score, #data in legend  
      opacity=0.9, 
      title = "World Happiness<br /> Index Score", 
      position = "bottomleft",
      colors = c('#ffffb2', '#fed976', '#feb24c', '#fd8d3c', '#fc4e2a', '#e31a1c', '#b10026'), #custom palette
      labels = c("Less Happy", "", "", "", "", "", "More Happy"), #custom labels
      group = "World Happiness Index" #group it belongs to
    ) %>%
      
      ### determines which default group is shown upon opening app ####
    hideGroup(c('World Happiness Index','GDP per Capita', 'Population Density', 'Covid-19 Stringency Score')) %>%
      showGroup('World Happiness Index')
  })
  
  #### update legend when the selected layer group changes ######################
  
  observeEvent(input$map_groups, {
    my_map <- leafletProxy("map") %>% clearControls() #refreshes map
    
    #### Adds happiness Legend if group = WHI ####
    if (input$map_groups == 'World Happiness Index'){
      my_map <- my_map %>%
        addLegend(
          opacity=0.9, 
          title = "World Happiness<br /> Index Score", 
          position = "bottomleft",
          colors = c('#ffffb2', '#fed976', '#feb24c', '#fd8d3c', '#fc4e2a', '#e31a1c', '#b10026'),
          labels = c("Less Happy", "", "", "", "", "", "More Happy"),
          values = worldCountries$happiness_score
        )
      
      #### GDP Legend ####
    }else if (input$map_groups == 'GDP per Capita'){ 
      my_map <- my_map %>%
        addLegend( 
          opacity=0.9, 
          title = "GDP per Capita (US$)", 
          position = "bottomleft",
          colors = c('#ffffd3', '#d9f0a3', '#addd8e', '#7bce7c', '#41ab5d', '#238443', '#005a32'),
          labels = c("Lower GDP", "", "", "", "", "", "Greater GDP"),
          values = worldCountries$gdp
        )
      
      #### pop density legend ####
    }else if (input$map_groups == 'Population Density'){
      my_map <- my_map %>%
        addLegend( 
          opacity=0.9, 
          title = "Population Density<br />  (per km\u00B2)", 
          position = "bottomleft",
          colors = c('#f2f0f7', '#dadaeb', '#bcbddc', '#9e9ac8', '#807dba', '#6a51a3', '#4a1486'),
          labels = c("Less Dense", "", "", "", "", "", "More Dense"),
          values = worldCountries$pop_density_log
        )
      
      ### covid legend ###
    }else{
      my_map <- my_map %>%
        addLegend( 
          opacity=0.9, 
          title = "Covid-19 Lockdown<br /> Stringency Score", 
          position = "bottomleft",
          colors = c('#eff3ff', '#c6dbef', '#9ecae1', '#6baed6', '#4292c6', '#2171b5', '#084594'),
          labels = c("Less Stringent ", "", "", "", "", "", "More Stringent"),
          values = worldCountries$avg_covid_score
        )
    }
  })
  
  
  ####### render ggplot for each group ###### 
  
  output$scatterplot <- renderPlot({
    
    if (input$map_groups == 'World Happiness Index'){
      print(whi_map)
      
      #### GDP graph ####
    }else if (input$map_groups == 'GDP per Capita'){ 
      print(gdp_map)
      
      #### pop density legend ####
    }else if (input$map_groups == 'Population Density'){
      print(pop_map)
      
      ### covid legend ###
    }else{
      print(covid_map)
    }
  })
  
  ####### plotly graphs #######
  output$plot_gdp <- renderPlotly({graph_gdp.interactive})
  output$plot_pop <- renderPlotly({graph_pop.interactive})
  output$plot_pop_adj <- renderPlotly({graph_poplog.interactive})
  output$plot_covid <- renderPlotly({graph_covid.interactive})
  
  ###### add data table ##################
  output$maptable <- renderDataTable(
    cleantable, #table to render
    options = list(
      pageLength = 10, #default page length
      lengthMenu = c(10, 15, 20), #filter settings
      initComplete = JS( #colours heading black and text white
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
        "}")
    )
  )  
}



#### Run App ####
shinyApp(ui = ui, server = server)
