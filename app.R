#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)


#Suicide data
# setwd("E:/Desktop/Shiny")
suicide <- read_csv("./suicide.csv")

#*Country name and country code
country_code <-
  read.csv(
    "https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv"
  ) %>%
  select(COUNTRY, CODE) %>%
  rename("country" = COUNTRY)

#Load terrorism
terrorism <- read_csv("./terrorism.csv")

#Remove useless columns
suicide <- suicide[,-(8:9)]
suicide <- suicide[,-(7)]
suicide <- suicide[,-(9)]
suicide

suicide_years <- suicide %>%
  group_by(country) %>%
  summarize(rows = n(),
            years = rows / 12) %>%
  arrange(years)
suicide_years

#Filter countries with more than 15 years of data
suicide_years <- suicide_years %>%
  filter(years > 15)
suicide_years
summary(suicide_years)

#Double check the data
suicide_years_sum <- suicide_years %>%
  filter(years <= 15)
sum(suicide_years_sum$rows)

#Generate new dataframe
suicide <- suicide %>%
  filter(country %in% suicide_years$country)

#Compute the median
suicide_median <- suicide %>%
  group_by() %>%
  summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    "suicides_100k_pop" = suicides_no / population * 100000
  )

#The trend of suicide
suicide_worldwide <- suicide %>%
  group_by(year) %>%
  summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    "suicides_100k_pop" = suicides_no / population * 100000
  )

#***************************SEX
#*
# Male suicide information
suicide_sex_male <- suicide %>%
  filter(sex == "male") %>%
  group_by(year) %>%
  summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    "suicides_100k_pop" = suicides_no / population * 100000
  )
# Female suicide information
suicide_sex_female <- suicide %>%
  filter(sex == "female") %>%
  group_by(year) %>%
  dplyr::summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    "suicides_100k_pop" = suicides_no / population * 100000
  )


#Suicide Population
suicide_sex <- suicide %>%
  group_by(year, sex) %>%
  summarise(suicides_no = sum(suicides_no),
            population = sum(population))


#***************************Age

#The suicide rate of different age groups
suicide_age <- suicide %>%
  group_by(year, age) %>%
  summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    "suicides_100k_pop" = suicides_no / population * 100000
  )
# #Re-order data
suicide_age$age <- factor(
  suicide_age$age,
  levels = c(
    "5-14 years",
    "15-24 years",
    "25-34 years",
    "35-54 years",
    "55-74 years",
    "75+ years"
  )
)

suicide_age_1 <- suicide %>%
  filter(age == "5-14 years") %>%
  group_by(year) %>%
  summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    "suicides_100k_pop" = suicides_no / population * 100000
  )

suicide_age_2 <- suicide %>%
  filter(age == "15-24 years") %>%
  group_by(year) %>%
  summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    "suicides_100k_pop" = suicides_no / population * 100000
  )

suicide_age_3 <- suicide %>%
  filter(age == "25-34 years") %>%
  group_by(year) %>%
  summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    "suicides_100k_pop" = suicides_no / population * 100000
  )

suicide_age_4 <- suicide %>%
  filter(age == "35-54 years") %>%
  group_by(year) %>%
  summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    "suicides_100k_pop" = suicides_no / population * 100000
  )

suicide_age_5 <- suicide %>%
  filter(age == "55-74 years") %>%
  group_by(year) %>%
  summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    "suicides_100k_pop" = suicides_no / population * 100000
  )

suicide_age_6 <- suicide %>%
  filter(age == "75+ years") %>%
  group_by(year) %>%
  summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    "suicides_100k_pop" = suicides_no / population * 100000
  )
#***************************
#***************************Countries
#***************************

#Rename country name
suicide$country[suicide$`country` == 'Russian Federation'] <-
  'Russia'
suicide$country[suicide$`country` == 'Republic of Korea'] <-
  'South Korea'
suicide$country[suicide$`country` == 'Saint Vincent and Grenadines'] <-
  'Saint Vincent and the Grenadines'

# Join country code
suicide <- left_join(suicide, country_code, by = "country")
summary(suicide)

#Rename country code
suicide$CODE[suicide$`country` == 'Bahamas'] <- 'BHM'
suicide$CODE[suicide$`country` == 'South Korea'] <- 'KOR'

#Suicide map data
suicide_maps <- suicide %>%
  group_by(country, year, CODE) %>%
  summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    suicides_100k_pop = sum(suicides_no) / population * 100000
  )

#check null value
suicide_maps %>% filter(is.na(CODE))

#**********country
suicide_country <- suicide %>%
  group_by(country) %>%
  summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    suicides_100k_pop = sum(suicides_no) / population * 100000
  )


#Country - Chart A
suicide_per <- suicide %>%
  group_by(country, sex) %>%
  summarise(
    suicides_no = sum(suicides_no),
    population = sum(population),
    suicides_100k_pop = sum(suicides_no) / population * 100000
  ) %>%
  spread(sex, suicides_no) %>%
  arrange(male - female)

suicide_per[is.na(suicide_per)] <- 0

suicide_per <- suicide_per %>%
  group_by(country) %>%
  summarise(
    male = sum(male),
    female = sum(female),
    male_pop = round(sum(male / (male + female) * 100), 2),
    female_pop = round(sum(female / (male + female)) * 100, 2)
  )

#***************************Terrorism

#Select columns and filter useless data
terrorism_selected <- terrorism %>%
  select(iyear, country_txt, success) %>%
  filter(success != 0) %>%
  filter(iyear >= 1987) %>%
  filter(iyear <= 2016) %>%
  group_by(iyear, country_txt) %>%
  summarise(success = sum(success)) %>%
  rename("country" = country_txt) %>%
  rename("year" = iyear) %>%
  rename("Terrorism" = success)

#Rejoin data with suicide
suicide_terrorism <-
  left_join(suicide_maps, terrorism_selected, by = c("country", 'year')) %>%
  filter(!is.na(Terrorism))

# The homepage title
titlename <-
  tags$a(
    tags$img(
      src = "https://www.gpbctv.com/uploads/20210515/zip_16210566333CWxhT.jpg",
      height = 40,
      width = 40
    ),
    "Why people commit suicide?",
    style = "color: black; font-size: 28px"
  )

#Suicide rate information
suicide_rate_median_info <-
  tags$a(
    tags$img(
      src = "https://www.gpbctv.com/uploads/20210515/zip_16210566333CWxhT.jpg",
      height = 40,
      width = 40
    ),
    " 13.46%",
    style = "color: blue; font-size: 28px"
  )


# Define UI for application that draws a histogram
ui <- fluidPage(dashboardPage(
  #set up dashboard color
  skin = "red",
  dashboardHeader(title = titlename,
                  titleWidth = 450),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Worldwide", tabName = "worldwide"),
      menuItem("Sex", tabName = "sex"),
      menuItem("Age", tabName = "age"),
      menuItem("Countries", tabName = "countries"),
      menuItem("Maps", tabName = "maps"),
      menuItem("Suicide and GDP", tabName = "suicide_and_gdp"),
      menuItem("Suicide and Terrorism", tabName = "suicide_and_terrorism")
    )
  ),
  
  dashboardBody(
    tabItems(
      #Worldwide page
      tabItem(
        tabName = "worldwide",
        plotlyOutput("suicide_worldwide", height = 920, width = 1400)
      ),
      
      # SEX page
      tabItem(tabName = "sex",
              fluidRow(
                box(
                  background = "red",
                  plotlyOutput("suicide_sex_1"),
                  height = 520
                ),
                box(
                  title = h3("Please select year for CHART A and CHART B"),
                  sliderInput(
                    "yearslider_sex",
                    label = h4("Year Range"),
                    min = 1987,
                    max = 2016,
                    value = c(1987, 2016)
                  ),
                  background = "red"
                )
              ),
              
              fluidRow(
                box(
                  title = "CHART A",
                  background = "red",
                  plotlyOutput("suicide_sex_2"),
                  height = 520
                ),
                box(
                  title = "CHART B",
                  background = "red",
                  plotlyOutput("suicide_sex_3"),
                  height = 520
                )
              )),
      
      # Age page
      tabItem(tabName = "age",
              fluidRow(
                box(
                  background = "red",
                  plotlyOutput("suicide_age_1"),
                  height = 520
                ),
                
                box(
                  title = h3("Please select year for CHART A and CHART B"),
                  sliderInput(
                    "yearslider_age",
                    label = h4("Year Range"),
                    min = 1987,
                    max = 2016,
                    value = c(1987, 2016)
                  ),
                  background = "red"
                )
              ),
              
              fluidRow(
                box(
                  title = "CHART A",
                  background = "red",
                  plotlyOutput("suicide_age_2"),
                  height = 520
                ),
                box(
                  title = "CHART B",
                  background = "red",
                  plotlyOutput("suicide_age_3"),
                  height = 520
                )
              )),
      
      
      # Countries page
      tabItem(
        tabName = "countries",
        box(
          plotlyOutput("suicide_countries_1"),
          height = 470,
          background = "red"
        ),
        fluidRow(
          box(
            title = "The global average suicide rate between 1985 and 2016:",
            suicide_rate_median_info,
            background = "red"
          ),
          box(
            selectInput(
              "countries",
              label = h3("Please select countries for CHART A and CHART B"),
              choices = c(suicide_country$country),
              selected = 1,
              multiple = T
            ),
            background = "red"
          ),
          box(
            sliderInput(
              "yearslider_countries",
              label = h3("Please select year for CHART B"),
              min = 1987,
              max = 2016,
              value = c(1987, 2016)
            ),
            background = "red"
          )
        ),
        fluidRow(
          box(
            title = "CHART A",
            background = "red",
            plotlyOutput("suicide_countries_2"),
            height = 520
          ),
          box(
            title = "CHART B",
            background = "red",
            plotlyOutput("suicide_countries_3"),
            height = 520
          )
        )
      ),
      
      # Maps page
      tabItem(
        tabName = "maps",
        background = "red",
        fluidRow(
          box(
            plotlyOutput("suicide_maps_1"),
            height = 500,
            background = "red"
          ),
          box(
            sliderInput(
              "yearslider_maps",
              label = h3("Please select year for maps and  tree maps"),
              min = 1985,
              max = 2016,
              value = 1999
            ),
            background = "red"
          )
        ),
        fluidRow(box(
          plotlyOutput("suicide_maps_2"),
          width = 12,
          height = 750,
          background = "red"
        ))
      ),
      
      # GDP page
      tabItem(
        tabName = "suicide_and_gdp",
        box(
          plotlyOutput("suicide_GDP"),
          width = 12,
          height = 920,
          background = "red"
        )
      ),
      
      # Terrorism page
      tabItem(
        tabName = "suicide_and_terrorism",
        box(
          plotlyOutput("suicide_terrorism"),
          width = 12,
          height = 920,
          background = "red"
        )
      )
    )
    
  )
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Line chart for worldwide suicide rate.
  output$suicide_worldwide <- renderPlotly({
    
    plot_ly <- plot_ly(
      data = suicide_worldwide,
      x = ~ year,
      y = ~ suicides_100k_pop,
      type = "scatter",
      mode = "markers+lines",
      hoverinfo = "text",
      color = ~ suicides_100k_pop,
      colors = "Reds",
      name = "Suicide Rate",
      text = paste(
        "Year: ",
        suicide_worldwide$year,
        "<br>",
        "Suicide Rate: ",
        round(suicide_worldwide$suicides_100k_pop, 2),
        "%"
      )
    ) %>%
      #add a median line
      add_trace(
        y = suicide_median$suicides_100k_pop,
        mode = "lines",
        name = "Median",
        text = paste(
          "The avergae suicide rate: ",
          round(suicide_median$suicides_100k_pop, 2),
          "%"
        )
      ) %>%
      layout(
        xaxis = list(rangeslider = list()),
        yaxis = list(title = "The suicide rate 100k/popuplation"),
        title = "The trend of suicide rate between 1985 and 2016",
        height = 890
      ) %>%
      colorbar(title = "Value")
    plot_ly
  })
  
  #**********************SEX**********************
  
  #Line chart for suicide rate 100k/pop accoring to sex
  output$suicide_sex_1 <- renderPlotly({
    
    plot_ly <- plot_ly(
      data = suicide_sex_male,
      x = ~ year,
      y = ~ suicides_100k_pop,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      color = ~ suicides_100k_pop,
      colors = "Reds",
      name = "Male suicide Rate",
      text = paste(
        "Year: ",
        suicide_sex_male$year,
        "<br>",
        "Gender: Male",
        "<br>",
        "Suicide Rate: ",
        round(suicide_sex_male$suicides_100k_pop, 2),
        "%"
      )
      
    ) %>% #add a median line
      add_trace(
        y = suicide_median$suicides_100k_pop,
        mode = "lines",
        name = "Median",
        text = paste(
          "The avergae suicide rate: ",
          round(suicide_median$suicides_100k_pop, 2),
          "%"
        )
      ) %>%
      add_trace(
        data = suicide_sex_female,
        x = ~ suicide_sex_female$year,
        y = ~ suicides_100k_pop,
        name = "Female suicide Rate",
        text = paste(
          "Year: ",
          suicide_sex_female$year,
          "<br>",
          "Gender: Female",
          "<br>",
          "Suicide Rate: ",
          round(suicide_sex_female$suicides_100k_pop, 2),
          "%"
        )
      ) %>%
      layout(
        xaxis = list(rangeslider = list()),
        yaxis = list(title = "The suicide rate 100k/popuplation"),
        title = "The suicide rate lines for men and women",
        height = 450
      ) %>%
      colorbar(title = "Value")
    
    plot_ly
    
  })
  
  #Bar chart for suicide rate 100k/pop accoring to sex and years
  output$suicide_sex_2 <- renderPlotly({
    suicide_sex_1 <- suicide_sex %>%
      filter(year >= input$yearslider_sex[1]) %>%
      filter(year <= input$yearslider_sex[2]) %>%
      group_by(sex) %>%
      summarise(
        suicides_no = sum(suicides_no),
        population = sum(population),
        "suicides_100k_pop" = suicides_no / population *
          100000
      )
    
    plot_ly <- plot_ly(
      data = suicide_sex_1,
      x = ~ sex,
      y = ~ suicides_100k_pop,
      type = "bar",
      
      color = ~ sex,
      colors = "Set1",
      # name = "Suicide Rate",
      hoverinfo = "text",
      text = paste(
        "Sex: ",
        suicide_sex_1$sex,
        "<br>",
        "Suicide Rate: ",
        round(suicide_sex_1$suicides_100k_pop, 2),
        "%"
      )
    ) %>% layout(
      xaxis = list(title = "Gender"),
      yaxis = list(title = "The suicide rate 100k/popuplation"),
      title = "The suicide rate 100k/population accoring to sex",
      height = 450
    ) %>%
      colorbar(title = "Sex")
    plot_ly
  })
  
  #Pie chart for the number of people commit suicide and sex ratio of suicides
  output$suicide_sex_3 <- renderPlotly({
    suicide_sex_3 <- suicide_sex %>%
      filter(year >= input$yearslider_sex[1]) %>%
      filter(year <= input$yearslider_sex[2]) %>%
      group_by(sex) %>%
      summarise(
        suicides_no = sum(suicides_no),
        population = sum(population),
        "suicides_100k_pop" = suicides_no / population *
          100000
      )
    plot <- plot_ly(
      type = "pie",
      data = suicide_sex_3,
      labels = ~ sex,
      values = ~ suicides_no ,
      textinfo = 'label+percent',
      insidetextorientation = 'radial'
    ) %>%
      layout(title = "The number of people commit suicide and sex ratio of suicides",
             autosize = F,
             height = 450)
    
    ggplotly(plot)
    
  })
  
  
  #**********************Age**********************
  
  #Line chart for suicide rate 100k/pop accoring to age
  output$suicide_age_1 <- renderPlotly({
    x <- list(title = "Year")
    y <- list(title = "The suicide rate 100k/popuplation")
    
    plot_ly <- plot_ly(
      data = suicide_age_1,
      x = ~ year,
      y = ~ suicides_100k_pop,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      # color = ~ age,
      # colors = "Reds",
      name = "5-14 years",
      text = paste(
        "Age: ",
        "5-14 years",
        "<br>",
        "Suicide Rate: ",
        round(suicide_age_1$suicides_100k_pop, 2),
        "%"
      )
    ) %>% #add a median line
      add_trace(
        y = suicide_median$suicides_100k_pop,
        mode = "lines",
        name = "Median",
        text = paste(
          "The avergae suicide rate: ",
          round(suicide_median$suicides_100k_pop, 2),
          "%"
        )
      ) %>%
      add_trace(
        data = suicide_age_2,
        x = ~ year,
        y = ~ suicide_age_2$suicides_100k_pop,
        name = "15-24 years",
        text = paste(
          "Age: ",
          "15-24 years",
          "<br>",
          "Suicide Rate: ",
          round(suicide_age_2$suicides_100k_pop, 2),
          "%"
        )
      ) %>%
      add_trace(
        data = suicide_age_3,
        x = ~ year,
        y = ~ suicide_age_3$suicides_100k_pop,
        name = "25-34 years",
        text = paste(
          "Age: ",
          "25-34 years",
          "<br>",
          "Suicide Rate: ",
          round(suicide_age_3$suicides_100k_pop, 2),
          "%"
        )
      ) %>%
      add_trace(
        data = suicide_age_4,
        x = ~ year,
        y = ~ suicide_age_4$suicides_100k_pop,
        name = "35-54 years",
        text = paste(
          "Age: ",
          "35-54 years",
          "<br>",
          "Suicide Rate: ",
          round(suicide_age_4$suicides_100k_pop, 2),
          "%"
        )
      ) %>%
      add_trace(
        data = suicide_age_5,
        x = ~ year,
        y = ~ suicide_age_5$suicides_100k_pop,
        name = "55-74 years",
        text = paste(
          "Age: ",
          "55-74 years",
          "<br>",
          "Suicide Rate: ",
          round(suicide_age_5$suicides_100k_pop, 2),
          "%"
        )
      ) %>%
      add_trace(
        data = suicide_age_6,
        x = ~ year,
        y = ~ suicide_age_6$suicides_100k_pop,
        name = "75+ years",
        text = paste(
          "Age: ",
          "75+ years",
          "<br>",
          "Suicide Rate: ",
          round(suicide_age_6$suicides_100k_pop, 2),
          "%"
        )
      ) %>%
      layout(
        xaxis = list(rangeslider = list()),
        yaxis = y,
        title = "The suicide rate lines for different age groups",
        height = 450
      ) %>%
      colorbar(title = "Value")
    plot_ly
  })
  
  #Bar chart for suicide rate 100k/pop accoring to age groups and years
  output$suicide_age_2 <- renderPlotly({
    suicide_age_2 <- suicide_age %>%
      filter(year >= input$yearslider_age[1]) %>%
      filter(year <= input$yearslider_age[2]) %>%
      group_by(age) %>%
      summarise(
        suicides_no = sum(suicides_no),
        population = sum(population),
        suicides_100k_pop = suicides_no / population *
          100000
      )
    
    plot_ly <- plot_ly(
      data = suicide_age_2,
      x = ~ age,
      y = ~ suicides_100k_pop,
      type = "bar",
      color = ~ age,
      colors = "Set1",
      name = ~ age,
      hoverinfo = "text",
      text = paste(
        "Age: ",
        suicide_age_2$age,
        "<br>",
        "Suicide Rate: ",
        round(suicide_age_2$suicides_100k_pop, 2),
        "%"
      )
    ) %>% layout(
      xaxis = list(title = "Age"),
      yaxis = list(title = "The suicide rate 100k/popuplation"),
      title = "The suicide rate 100k/population accoring to age groups",
      height = 450
    ) %>%
      colorbar(title = "Age groups")
    
    ggplotly(plot_ly)
  })
  
  #Pie chart for the number of people commit suicide and age groups ratio of suicides
  output$suicide_age_3 <- renderPlotly({
    suicide_age_3 <- suicide_age %>%
      filter(year >= input$yearslider_age[1]) %>%
      filter(year <= input$yearslider_age[2]) %>%
      group_by(age) %>%
      summarise(
        suicides_no = sum(suicides_no),
        population = sum(population),
        "suicides_100k_pop" = suicides_no / population *
          100000
      )
    plot <- plot_ly(
      type = "pie",
      data = suicide_age_3,
      labels = ~ age,
      values = ~ suicides_no ,
      textinfo = 'label+percent',
      insidetextorientation = 'radial'
    ) %>%
      layout(title = "The number of people commit suicide and age ratio of suicides",
             autosize = F,
             height = 450)
    ggplotly(plot)
    
  })
  
  
  #********************Countries******************
  
  #Bar chart for global average suicide rate between 1987 and 2016
  output$suicide_countries_1 <- renderPlotly({
    suicide_country <- suicide_country %>%
      arrange(suicides_100k_pop) %>%
      mutate(country = factor(country, levels = country))
    plot <- plot_ly(
      data = suicide_country,
      y = ~ suicides_100k_pop,
      x = ~ country,
      mode = "bar",
      color = ~ country,
      colors = "Reds",
      hoverinfo = "text",
      
      text = paste(
        "Country: ",
        suicide_country$country,
        "\n",
        "Suicide Rate: ",
        round(suicide_country$suicides_100k_pop, 2),
        "%"
      )
    ) %>%
      layout(
        xaxis = list(rangeslider = list()),
        yaxis = list(title = "The suicide rate 100k/popuplation"),
        title = "The global avergae suicide rate between 1987 and 2016.",
        autosize = T
      )
    
    ggplotly(plot)
  })
  
  #Bar chart for the percentage of sex suicide number from 1987 to 2016
  output$suicide_countries_2 <- renderPlotly({
    suicide_per <- suicide_per %>% filter(country %in% input$countries)
    plot <- plot_ly(
      data = suicide_per,
      x = ~ country,
      y = ~ male_pop ,
      name = 'Male',
      type = "bar",
      hoverinfo = "text",
      text = paste(
        "Country: ",
        suicide_per$country,
        "\n",
        "Sex:",
        "male",
        "\n",
        "Suicide number: ",
        suicide_per$male,
        "\n",
        "Percentage:",
        suicide_per$male_pop,
        "%"
      )
    ) %>%
      add_trace(
        y = ~ female_pop,
        name = 'Female',
        hoverinfo = "text",
        text = paste(
          "Country: ",
          suicide_per$country,
          "\n",
          "Sex:",
          "female",
          "\n",
          "Suicide number: ",
          suicide_per$female,
          "\n",
          "Percentage:",
          suicide_per$female_pop,
          "%"
        )
      ) %>%
      layout(
        xaxis = list(rangeslider = list()),
        yaxis = list(title = 'Percentage (%)'),
        title = "The suicide number and percentgae for countries selected from 1987 to 2016",
        barmode = "stack",
        autosize = F
      )
    
    ggplotly(plot)
    
  })
  
  #Line chart for the suicide rate according to countries selected and year selected
  output$suicide_countries_3 <- renderPlotly({
    suicide_countries_3 <- suicide %>%
      filter(country %in% input$countries) %>%
      filter(year >= input$yearslider_countries[1]) %>%
      filter(year <= input$yearslider_countries[2]) %>%
      group_by(country, year) %>%
      summarise(
        suicides_no = sum(suicides_no),
        population = sum(population),
        suicides_100k_pop = sum(suicides_no) / population * 100000
      )
    
    plot_ly <- plot_ly(
      data = suicide_countries_3,
      x = ~ year,
      y = ~ suicides_100k_pop,
      type = "scatter",
      mode = "markers+lines",
      hoverinfo = "text",
      color = ~ country,
      colors = "Set1",
      name = ~ country,
      text = paste(
        "Year: ",
        suicide_countries_3$year,
        "<br>",
        "Suicide Rate: ",
        round(suicide_countries_3$suicides_100k_pop, 2),
        "%"
      )
    ) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "The suicide rate 100k/popuplation"),
        title = "The suicide rate line chart for countries selected and year selected."
      )
    plot_ly
    
  })
  
  
  #**********************Maps*********************
  
  #World maps for suicide rate and number 
  output$suicide_maps_1 <- renderPlotly({
    suicide_maps <-
      suicide_maps %>% filter(year == input$yearslider_maps)
    suicide_maps$hover <-
      with(
        suicide_maps,
        paste(
          "Year",
          year,
          '<br>',
          "Country: ",
          country,
          '<br>',
          "Suicide Number: ",
          suicides_no,
          '<br>',
          "Suicide Rate: ",
          round(suicides_100k_pop, 2),
          "%"
        )
      )
    plot <- plot_ly(
      suicide_maps,
      type = 'choropleth',
      locations = ~ CODE,
      z = ~ suicides_100k_pop,
      text =  ~ hover,
      colorscale = "Blues"
    ) %>% layout(title = "The suicide rate all over the world.",
                 autosize = F) %>%
      colorbar(title = "Suicide Rate")
    
    
    ggplotly(plot)
  })
  
  #Tree maps for suicide rate and number according to years selected
  output$suicide_maps_2 <- renderPlotly({
    suicide_maps <-
      suicide_maps %>% filter(year == input$yearslider_maps)
    suicide_maps$hover <-
      with(
        suicide_maps,
        paste(
          "Year",
          year,
          '<br>',
          "Suicide Number: ",
          suicides_no,
          '<br>',
          "Suicide Rate: ",
          round(suicides_100k_pop, 2),
          "%"
        )
      )
    
    plot <- plot_ly(
      suicide_maps,
      type = 'treemap',
      ids = ~ country,
      labels = ~ country,
      values = ~ suicides_100k_pop,
      text = ~ hover,
      parents = ~ NA
    ) %>%
      layout(
        title = "The suicide rate and number all over the world.",
        autosize = F,
        width = 1500,
        height = 700
      )
    ggplotly(plot)
  })
  
  #**********************GDP**********************
  
  # The relationship between suicide rate and gdp for year
  output$suicide_GDP <- renderPlotly({
    suicide_gdp <- suicide %>%
      group_by(country, year) %>%
      summarise(
        suicides_no = sum(suicides_no),
        population = sum(population),
        "Suicide Rate" = round(suicides_no / population *
                                 100000, 2),
        gdp_for_year = sum(`gdp_for_year ($)`) / 12,
        GDP = sum(`gdp_per_capita ($)`) / 12
      ) %>% rename(Year = year) %>% rename(Population = population) %>% rename(Country = country) %>%
      rename("Suicide Number" = suicides_no)
    
    gdp_plot <-
      ggplot(
        suicide_gdp,
        aes(
          x = GDP,
          y = `Suicide Rate`,
          color = Country,
          size = Population,
          "Suicide Number: " = `Suicide Number`
        )
      ) +
      geom_point() +
      geom_smooth(
        method = lm ,
        color = "red",
        fill = "#69b3a2",
        se = TRUE
      ) +
      scale_size(name = "Circle Size for total Population") +
      xlab("GDP per capita for year") +
      ylab("The suicides 100k/pop") +
      ggtitle("The relationship between suicide rate and gdp for year") +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(gdp_plot, height = 890)
  })
  
  #*******************Terrorism*******************
  # The relationship between suicide rate and terrorism for year
  output$suicide_terrorism <- renderPlotly({
    suicide_terrorism <- suicide_terrorism %>%
      rename(Year = year) %>%
      rename(Population = population) %>%
      rename(Country = country) %>%
      rename("Suicide Rate" = suicides_100k_pop) %>%
      rename("Suicide Number" = suicides_no)
    
    terrorism_plot <-
      ggplot(
        suicide_terrorism,
        aes(
          x = Terrorism,
          y = `Suicide Rate`,
          color = Country,
          size = Population,
          "Suicide Number: " = `Suicide Number`
        )
      ) +
      geom_point() +
      geom_smooth(
        method = lm ,
        color = "red",
        fill = "#69b3a2",
        se = TRUE
      ) +
      scale_size(name = "Circle Size for total Population") +
      xlab("The number of terrorism") +
      ylab("The suicides 100k/pop") +
      ggtitle("The relationship between suicide rate and terrorism for year") +
      theme(plot.title = element_text(hjust = 0.5))
    ggplotly(terrorism_plot, height = 890)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
