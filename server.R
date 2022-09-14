library(maps)
library(dplyr)
library(tidyr)
library(htmltools)
library(leaflet)
library(shiny)


# Alex
map <- map("county", "washington", fill = TRUE, plot = FALSE)
counties <- read.csv("uscities.csv") %>%
  filter(state_id == "WA") %>%
  group_by(county_name) %>%
  summarize(avg_lat = mean(lat), avg_lng = mean(lng))

counties$county_name <- paste(counties$county_name, "County")

crime_rate_df <- read.csv("crime_rate_US_by_county.csv", stringsAsFactors = FALSE)
crime_rate_df$state <- substring(crime_rate_df$county_name, nchar(crime_rate_df$county_name) - 1, nchar(crime_rate_df$county_name))

crime_rate_WA_df <- crime_rate_df %>%
  filter(state == "WA")

crime_rate_WA_df$crime_rate_per_1000000 <- crime_rate_WA_df$crime_rate_per_100000 / 10

unemployment_df <- read.csv("unemployment.csv", stringsAsFactors = FALSE) %>%
  select(X.1, X.7) %>%
  rename("County" = X.1 , "Unemploy_rate"= X.7) %>%
  tail(-2) %>%
  head(-2)

crime_rate_WA_df$county_name <- substr(crime_rate_WA_df$county_name, 1, nchar(crime_rate_WA_df$county_name) - 4)
unemployment_df$County <- substr(unemployment_df$County, 1, nchar(unemployment_df$County) - 4)

unemployment_df <- rename(unemployment_df, county_name = County)

crime_unemp_df <- crime_rate_WA_df %>%
  left_join(unemployment_df, by = "county_name") %>%
  select(county_name, crime_rate_per_100000, crime_rate_per_1000000, Unemploy_rate) %>%
  left_join(counties, by = "county_name") %>% 
  mutate(diff = crime_rate_per_1000000 - Unemploy_rate)

crime_unemp_long <- crime_unemp_df %>% 
  rename(County = county_name) %>% 
  select(County, crime_rate_per_1000000, Unemploy_rate, diff) %>%
  gather(key = Category, value = value, -County)

# Xinjie's Data
crime_vs_type <- mutate(crime_rate_WA_df, Murder = MURDER / population, Rape = RAPE / population, Robbery = ROBBERY / population, "Agg Assault" = AGASSLT / population, Burglary = BURGLRY / population, Larceny = LARCENY / population, "MV Theft" = MVTHEFT / population, Arson = ARSON / population) %>% select(crime_rate_per_1000000, Murder, Rape, Robbery, "Agg Assault", Burglary, Larceny, "MV Theft", Arson) %>% gather(type, value, -crime_rate_per_1000000)
color_key <- c("Murder" = 'red', "Rape" = 'blue', "Robbery" = 'green', "Agg Assault" = 'black', "Burglary" = 'orange', "Larceny" = 'turquoise', "MV Theft" = 'purple', "Arson" = 'greenyellow')
color_index <- c("Murder", "Rape", "Robbery", "Agg Assault", "Burglary", "Larceny", "MV Theft", "Arson")


#FUNCTION: ADD ALL EDITS HERE


my_server <- function(input_list, output_list) {
  # Alex
  output_list$q1_interact_map <- renderLeaflet({
   map <- leaflet(data = map) %>% addTiles() %>%
      addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

   if(input_list$stat_type_a == "Both"){
     map <- map %>%
       addCircleMarkers(~counties$avg_lng, ~counties$avg_lat, popup = paste(crime_unemp_df$county_name, paste("Crime Rate:", crime_unemp_df$crime_rate_per_1000000),
                                                                            paste("Unemployment Rate:", crime_unemp_df$Unemploy_rate), sep = ", "), color = "red")
   }

   if(input_list$stat_type_a == "Crime Rate (per 100k)"){
     map <- map %>%
       addCircleMarkers(~counties$avg_lng, ~counties$avg_lat, popup = paste(crime_unemp_df$county_name, paste("Crime Rate:", crime_unemp_df$crime_rate_per_1000000), sep = ", "), color = "red")
   }

   if(input_list$stat_type_a == "Unemployment Rate (%)"){
     map <- map %>%
      addCircleMarkers(~counties$avg_lng, ~counties$avg_lat, popup = paste(crime_unemp_df$county_name,
                                                                           paste("Unemployment Rate:", crime_unemp_df$Unemploy_rate), sep = ", "), color = "red")
   }

   return(map)
  })

  output_list$q1_plot <- renderPlot({
    q1_plot <- ggplot(data = crime_unemp_long, mapping = aes(x = County, y = value, color = Category)) +
    geom_point() +
    geom_path(data = filter(crime_unemp_long, Category != "diff"), mapping = aes(group = County, color = "#ccebc5"), show.legend = FALSE) +
    scale_color_discrete(breaks = c("crime_rate_per_1000000","Unemploy_rate", "diff"),
                         labels = c("Crime Rate /1000k", "Unemployment Rate (percent)", "Difference between Crime Rate & Unemployment")) +
    labs(title = "Crime Rate vs. Unemployment Rate by County in WA", y = "Value", color = "Category") +
    theme(axis.text.x = element_text(size = 5.5, angle = 90), legend.position = c(.19, .885))
    return(q1_plot)
  })
  
  output_list$q1_analysis <- renderTable({
    crime_mean <- round(mean(crime_unemp_df$crime_rate_per_1000000), 3)
    crime_sd <- round(sd(crime_unemp_df$crime_rate_per_1000000), 3)
    unemp_mean <- round(mean(crime_unemp_df$Unemploy_rate), 3)
    unemp_sd <- round(sd(crime_unemp_df$Unemploy_rate), 3)
    diff_mean <- round(mean(crime_unemp_df$diff), 3)
    diff_sd <- round(sd(crime_unemp_df$diff), 3)
    
    if(input_list$stat_type_b == "Both"){
      general_stats <- paste("Mean of Crime Rate = ", crime_mean, ", Standard Deviation of Crime Rate = ", crime_sd, ", Mean of Unemployment = ", 
                             unemp_mean, ", Standard Deviation of Unemployment = ", unemp_sd, ", Mean of difference = ", diff_mean, 
                             ", Standard Deviation of Unemployment = ", diff_sd) 
    }
    
    if(input_list$stat_type_b == "Crime Rate (per 1000k)"){
      general_stats <- paste("Mean of Crime Rate = ", crime_mean, ", Standard Deviation of Crime Rate = ", crime_sd) 
    }
    
    if(input_list$stat_type_b == "Unemployment Rate (%)"){
      general_stats <- paste("Mean of Unemployment = ", unemp_mean, ", Standard Deviation of Unemployment = ", unemp_sd) 
    }
    
    intro <- "This question is dirrectly related to our topic. Looking for correlation between crime rate and unemployment rate. 
    Crime rate in this analysis using unit by 1000000 population. Unemployment rate is by percentage of the population."
    method <- "I select the crime rate and unemployment rate for each county in WA from the datasets, calculate the difference between them, 
    then plot them using both point and path geometry. Point geometry clearly show the stats while path geometry provides visual 
    aids to the difference between crime rate and unemployment rate."
    summary <- "From these two datasets, the crime rate and unemployment show no strong correlation. If there is correlation, the line plot will 
    be relatively same length by each county. Also, the difference should be relatively close, but the graph shows no similarity. The stats of 
    difference also shows large discrepancy. A reason that there is no correlation may be unemployment rate only shows percentage of population
    without job and actively looking for a job, criminals may not fall into that category. By this analysis, offering job opportunities cannot solve the crime problem, 
    government need to seek other solutions to prevent crimes."
    q1_ana_df <- data_frame("General Stats" = general_stats, "Intro" = intro, "Method" = method, "Summary" = summary)
    q1_ana_long <- gather(q1_ana_df," ", " ")
    return(q1_ana_long)
  })
  
  
  # (Xinjie) Q3 - Crime Rate vs Change Interactive Plot
  filtered_color_key <- reactive({
    logical <- !(color_index %in% input_list$stat_type2)
    replace(color_key, logical, NA)
  })
  
  output_list$change_plot <- renderPlot({
    ggplot(data = crime_vs_type) +
      geom_point(mapping = aes(y = value, x = crime_rate_per_1000000, color = type)) +
      geom_line(mapping = aes(y = value, x = crime_rate_per_1000000, color = type)) +
      scale_color_manual(name = "Crime Type", values = filtered_color_key()) +
      labs(title = "Crime Rate vs. Crime Type Per Capita", x = "Crime Rate (crime per 1 million population)", y = "Crime Type Per Capita") +
      scale_x_continuous(limits = input_list$slider_key)
  })
  
  output_list$selected_var <- renderText(
    if(length(input_list$stat_type2) == 0) {
      paste("Nothing Selected!")
    } else {
    paste(input_list$stat_type2, collapse = ", ")
    }
  )
  
  output_list$selected_range <- renderText(
    paste(input_list$slider_key, collapse = " -> ")
  )
  
  output_list$greatest_change <- renderText(
    if(length(input_list$stat_type2) == 0) {
      paste("Nothing to Analyze!")
    } else {
      min <- input_list$slider_key[[1]]
      max <- input_list$slider_key[[2]]
      
      only_minmax_change <- filter(crime_vs_type, crime_rate_per_1000000 >= min & crime_rate_per_1000000 <= max) %>% group_by(type) %>% filter(crime_rate_per_1000000 == max(crime_rate_per_1000000) | crime_rate_per_1000000 == min(crime_rate_per_1000000)) %>% summarize(min = min(value), max = max(value)) %>% mutate(change = max - min)
      
      tableFinal <- filter(only_minmax_change, type %in% input_list$stat_type2) %>% select(type, change) %>% filter(change == max(change))
      greatestType <- pull(tableFinal, type)
      greatestChange <- pull(tableFinal, change)
      
      HTML(paste("Based on the current selection,", strong(greatestType), "is the crime with the", em("greatest change"), "of", strong(greatestChange), "crimes per capita."))
    }
  )
  
  output_list$change_table <- renderTable({
    min <- input_list$slider_key[[1]]
    max <- input_list$slider_key[[2]]
    
    only_minmax_change <- filter(crime_vs_type, crime_rate_per_1000000 >= min & crime_rate_per_1000000 <= max) %>% group_by(type) %>% filter(crime_rate_per_1000000 == max(crime_rate_per_1000000) | crime_rate_per_1000000 == min(crime_rate_per_1000000)) %>% summarize(min = min(value), max = max(value)) %>% mutate(change = max - min)
    
    filter(only_minmax_change, type %in% input_list$stat_type2) %>% select(type, change)
  }, digits = 10
  )
  

  # Question 2 Data Wrangling(dhandeep)

  output_list$rob_plot <- renderPlot({

    type_plot <- input_list$select
    num_counties <- input_list$num_county

    top_five_pop <- crime_rate_WA_df %>%
      mutate(burg_robb_combined = BURGLRY+ROBBERY) %>%
      arrange(-population) %>%
      head(num_counties) %>%
      rename("County" = county_name) %>%
      select(1,2,16,18,22,27)

    up_unemp <- unemployment_df %>%
      rename("County" = county_name)

    unemp_top_pop <- top_five_pop %>%
      left_join(up_unemp, by = "County") %>%
      rename("unemployment_rate" = Unemploy_rate, "crime_rate" = crime_rate_per_100000)

   if(type_plot == 1) {
    plot_rob <- ggplot(data = unemp_top_pop, mapping = aes(x= unemployment_rate, y = burg_robb_combined, fill = County )) +
      geom_col() +
      labs(title = "Robbery+Burglry attempts vs  Unemployment Rate", x = "Unemployemnt Rate", y = "Burglry + Robbery attempts")

         return(plot_rob)
   }

   if(type_plot == 2) {
        plot_rob <- ggplot(data = unemp_top_pop, mapping = aes(x = unemployment_rate, y = BURGLRY, fill = County )) +
          geom_col() +
          labs(title = "Burglry attempts vs Unemployment Rate", x = "Unemployment Rate", y = "Burglry attempts")

          return(plot_rob)
    }

    if(type_plot == 3) {
      plot_rob <- ggplot(data = unemp_top_pop, mapping = aes(x =  unemployment_rate, y = ROBBERY , fill = County )) +
        geom_col() +
        labs(title = "Robbery attempts vs Unemployment Rate", x = "Unemployment Rate", y = "Robbery attempts")

        return(plot_rob)
    }

    return(plot_rob)

    })

  output_list$crime_table <- renderTable({
    
    type_plot <- input_list$select_a
    num_counties <- input_list$num_count
    
  if(type_plot == 1) {
    table_crime <- crime_rate_WA_df %>%
      mutate(burg_robb_combined = BURGLRY+ROBBERY) %>%
      arrange(-population) %>%
      head(num_counties) %>%
      rename("County" = county_name) %>%
      select(1,22,27)
    
    up_unemp <- unemployment_df %>%
      rename("County" = county_name)
    
    unemp_top_pop <- table_crime %>%
      left_join(up_unemp, by = "County") %>%
      rename("Unemployment Rate" = Unemploy_rate, "Burglary & Robberies" = burg_robb_combined, "Population" = population)
    
    return(unemp_top_pop)
      
  }
  
    if(type_plot == 2) {
      table_crime <- crime_rate_WA_df %>%
        arrange(-population) %>%
        head(num_counties) %>%
        rename("County" = county_name) %>%
        select(1,18,22)
     
       up_unemp <- unemployment_df %>%
        rename("County" = county_name)
      
      unemp_top_pop <- table_crime %>%
        left_join(up_unemp, by = "County") %>%
        rename("Unemployment Rate" = Unemploy_rate, "Burgalry" = BURGLRY,"Population" = population) 
        
    
    return(unemp_top_pop)
  }
  
    if(type_plot == 3)  {
      table_crime <- crime_rate_WA_df %>%
        arrange(-population) %>%
        head(num_counties) %>%
        rename("County" = county_name) %>%
        select(1,16,22)
     
       up_unemp <- unemployment_df %>%
        rename("County" = county_name)
      
      unemp_top_pop <- table_crime %>%
        left_join(up_unemp, by = "County") %>%
        rename("Unemployment Rate" = Unemploy_rate, "Robbery" = ROBBERY, "Population" = population)
    
    return(unemp_top_pop) 
  
  }  
    return(unemp_top_pop)
      
  })
  output_list$text_one <- renderText({
   
    part_one <- "This analysis can help understand the correlation between the number of burglary and robbery attempted crimes in a county with respect to the unemployment rate. 
    The unemployment rate refers to the number of individuals who are unemployed but actively looking for work. 
    In this analysis, the attempts of robbery and burglary are combined into one variable. There are also different two more plots, which are burglary and robbery individually vs unemployment rate, which can help find a correlation between them. The number of counties allows us to see the different numbers of observations and see if there is a correlation with more or fewer counties."

    return(part_one)
    
  })
  
  output_list$text_two <- renderText({
    
    part_two <- "In order to look at the total numbers of robbery and burglary cases, I added the two together and found one value.
    Choosing the top counties in population for my case of study, thus giving more numbers to work with. 
    We also see the plot of just robbery and burglary by themselves to show if there is a correlation for just them individually."
  
    return(part_two)
    
  })
  
  output_list$text_three <- renderText({
    
    part_three <- "Based on the graph, one can see there is no correlation between the unemployment rate and the total number of burglary/robbery attempts.
    For example, looking at King County which has the highest robber/burglary attempts in all the counties has the lowest unemployment rate(3.9) compared to the other nine counties. 
    Whereas the county with the highest unemployment rate (7.5), has a lower burglary te(which is about 3000). Using these two observations proves the point being made, which is that there is no correlation between attempts and the unemployment rate.
    More observations can be made which leads to the same answer as before. We might have a stereotype for counties that have a higher unemployment rate will have higher robbery and burglary rate, however, we can see by the trend that this is not true. 
    As we see by the three different plots, there is no correlation between the unemployment rate and robbery/burglary. For example, if we look at King County in all three different plots(with the highest number of counties configuration, 25), it has the highest number of attempts for either robbery, burglary and both combined, but has the lowest unemployment rate in comparison to rest of the counties with 3.9.
    Thus, proving there is no correlation between unemployment and the crime." 
    
    return(part_three)
    
  })
  
  output_list$prob_domain <- renderText({
    prob_domain <- "In this analysis report, we analyze correlation between crime rate and unemployment rate in each county of Washington State. Crime rate is defined by number of crimes / population, the most used unit is crime rate per 100000 population. 
    Unemployment rate is by percentage of the population in this analysis. This domain is worth analyzing because if these two elements show strong correlation, can help us understand and possibly attempt to solve this problem.
    The questions we choosed play a pivotal role in our daily lives,  crime rate or unemployment. Thus, by asking these questions we can analyze and make assumptions that can help each of us or even on a global scale."
  
    return(prob_domain)  
  })
  
}
