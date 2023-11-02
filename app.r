#https://jckober5.shinyapps.io/provoMealTool/

library(RMySQL) #Use R code in sync with MYSQL
library(DBI) #allow Database connection
library(shiny) #base Shiny for deployment
library(shinyjs) #incorporate JS commands tailored to Shiny
library(bslib) #apply bootstrap themes
library(plotly) #make ggplots interactive
library(ggplot2) #create charts utilizing ggplot2
library(readr)

# UI
#source("mySQLConnection.R")
# Function to establish database connection
dbcon <- function() {
  con <- DBI::dbConnect(RMySQL::MySQL(),
                        host = "#####",
                        port = 3306,
                        user = "jkober",
                        password = "####"
  )
  return(con)
}

ui <- fluidPage(
  useShinyjs(),
  
  #BS Theme
  theme = bs_theme(
    version = 5,
    bg = "#100410ff",
    fg = "#B8BCC2",
    primary = "#aa3bafff",
    secondary = "#aa3bafff",
    base_font = font_google("Prompt"),
    heading_font = font_google("Proza Libre"),
  ),
  
  fluidRow(column(width = 12, align = "center",
                  
                  #generate logo
                  tags$a(href = 'https://koberstudio.com',
                         tags$image(src = "https://koberstudio.com/divide_music/Logo.svg", height = "200px", width = "200px", alt = "", deleteFile = FALSE)),    
                  
                  #Intro text before submission
                  h1(textOutput("intro")),
                  
                  #Form Inputs
                  selectInput(inputId = "new" 
                              , label = "Would you prefer a newer released song?"
                              , choices = c("Yes", "No", "Does Not Matter")
                              , selected = "Does Not Matter"
                  ),
                  selectInput(inputId = "views" 
                              , label = "How important is the number of video views to you?"
                              , choices = c("Very Important", "Not Important")
                              , selected = "Not Important"
                  ),
                  selectInput(inputId = "likes" 
                              , label = "How important is the number of video likes to you?"
                              , choices = c("Very Important", "Not Important")
                              , selected = "Not Important"
                  ),
                  textInput(inputId = "anime" 
                              , label = "What is your favorite anime? (Optional)"
                              , value = ""
                  ),
                  
                  #Submission Button
                  actionButton(inputId = "submit", label = "Submit", class = "btn-success", icon = icon("align-right")),
                  
                  #Success text when form is submitted
                  h1(textOutput("result")),
                  h3(uiOutput("url")),
                  h5(textOutput("description")),
                  uiOutput("frame"),
                  
                  #Refresh button hidden until the form is submitted
                  hidden(
                    actionButton("refresh","Reload the Survey", class = "btn-info", icon = icon("refresh"))
                  ),
                  
                  #Chart for form data
                  plotlyOutput("musicPlot")
  ))
)

# Server
server <- function(input, output, session) {
  # Establish database connection
  con <- dbcon()
  intro <- "What song from Divide Music to watch?"
  output$intro <- renderText({
    intro
  })
  # Submit data to database
  observeEvent(input$submit, {
    new <- input$new
    views <- input$views
    likes <- input$likes
    anime <- input$anime
  
  #Read in the songs from Divide Music
  df <- DBI::dbGetQuery(con, "select * from external_data.vw_divide_music")
  
  #Filter data based on survey answers
  if(anime != ''){
    df2 <- subset(df,grepl(tolower(anime), tolower(df$video_name)))
  }else{
    df2 <- df
  }
  
  if(new == 'Yes'){
    df3 <- df2[order(df2$video_created_date, decreasing = TRUE),]
    df3 <- head(df3, ceiling(nrow(df3)/2))
  }else if(new == 'No'){
    df3 <- df2[order(df2$video_created_date, decreasing = FALSE),]
    df3 <- head(df3, ceiling(nrow(df3)/2))
  }else{
    df3 <- df2
  }
  
  if(views == 'Very Important'){
    df4 <- df3[order(df3$video_views, decreasing = TRUE),]
    df4 <- head(df4, ceiling(nrow(df4)/2))
  }else{
    df4 <- df3
  }
  
  if(likes == 'Very Important'){
    df5 <- df4[order(df4$video_likes, decreasing = TRUE),]
    df5 <- head(df5, ceiling(nrow(df5)/2))
  }else{
    df5 <- df4
  }
  
  #Generate random result of where to eat with it's description and address
  if(nrow(df5) > 0){
    result <- sample(df5$video_name, size = 1)
    id <- subset(df5, df5$video_name == result)$video_id
    created <- subset(df5, df5$video_name == result)$video_created_date
    video_views <- subset(df5, df5$video_name == result)$video_views
    video_likes <- subset(df5, df5$video_name == result)$video_likes
    url <- paste('',a("Watch Video", href= subset(df5, df5$video_name == result)$video_url))
#
  } else{
    result <- "No Video found"
    id <- NULL
    created <- NULL
    views <- NULL
    likes <- NULL
    url <- NULL
  }
  result <- gsub("'", "",result)
  result <- gsub('"', '',result)
  
  #Query to pass to MYSQL Table
  query <- paste0("insert into forms.divide_music_survey 
                            (video_recommended
                        	, video_id
                        	, survey_completed_at
                        	, anime_choice
                        	, view_preference
                        	, like_preference
                        	, created_preference_new
                        )
                    values (",
                  "'", result, "',",
                  "'", id, "',",
                  "'", Sys.Date(), "',",
                  "'", anime, "',",
                  "'", views, "',",
                  "'", likes, "',",
                  "'", new,"');"
  )
  cat(query)
  DBI::dbGetQuery(con, statement = query)
  
  #Hide Inputs & Submit button once the form is submitted
  shinyjs::hide("new")
  shinyjs::hide("views")
  shinyjs::hide("likes")
  shinyjs::hide("anime")
  shinyjs::hide("submit")
  
  #Text upon completion of the form
  output$result <- renderText({
    result
  })
  output$url <- renderText({
    url
  })
  output$description <- renderText({
    paste('Created:',created,'  Views:',video_views,'  Likes:',video_likes)
  })
  
  output$frame <- renderUI({
    video <- tags$iframe(src=subset(df5, df5$video_name == result)$video_url, height=600, width=535)
    #print(my_test)
    video
  })
  
  #Show Refresh Button when form is submitted
  shinyjs::show("refresh")
  
  #Show GGPlot for Songs Recommended
  df <- DBI::dbGetQuery(con, "select * from forms.divide_music_survey ")
  count <- rep(1, nrow(df))
  df <- cbind(df, count)
  musicPlot <- aggregate(count~video_recommended, data = df, sum)
  text <- paste(musicPlot$count,'recommended to watch',musicPlot$video_recommended)
  
  #Render Chart
  output$musicPlot <- renderPlotly({
    ggplotly(ggplot(data = musicPlot, aes(x = reorder(video_recommended, count), y = count, fill = count, text = text)) +
               geom_bar(stat = "identity", color = "#000000", alpha = .4, width = .75, show.legend = FALSE) +
               coord_flip() +
               theme(panel.background = element_rect(fill = "#202123"),
                     plot.background = element_rect(fill = "#202123"),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     title = element_text(colour = '#ffffff'),
                     axis.title.x = element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     axis.ticks.y = element_blank()) +
               ylim(0,max(musicPlot$count)+5) +
               ggtitle("What we have recommended to watch?"), tooltip = "text"
    )
  }) 
  })
  
  #Reload form when refresh button is pressed
  observeEvent(input$refresh, {
    session$reload()
  })
  
  # Disconnect from database on app close
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}


# Run the app
shinyApp(ui, server)

