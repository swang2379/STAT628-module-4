library(shiny)
library(ggplot2)
library(cluster)
library(dplyr)
library(factoextra)

podcast_data<-readRDS("data use in app.rds")
scaled_data <- podcast_data %>%
  select(Special_Score, normalized_score) 

ui <- fluidPage(
  titlePanel("Podcast Clustering and Recommendations"),
  sidebarLayout(
    sidebarPanel(
      selectInput("episode", "Choose an Episode", choices = podcast_data$Episodename),
      numericInput("num_clusters", "Number of Clusters", value = 3, min = 2, max = 10),
      h4("Contact Information:"),
      p("For any questions, please contact app maintainer:"),
      p("Chenyu Jiang: cjiang232@wisc.edu; Siyu Wang: swang2379@wisc.edu")
    ),
    mainPanel(
      plotOutput("cluster_plot"),
      tableOutput("nearest_table")
    )
  )
)

server <- function(input, output) {
  # Reactive clustering
  reactive_clusters <- reactive({
    kmeans(scaled_data, centers = input$num_clusters, nstart = 25)
  })
  
  
  # Cluster Plot
  output$cluster_plot <- renderPlot({
    selected_episode <- input$episode
    selected_data <- podcast_data %>%
      filter(Episodename == selected_episode)
    
    plot <- fviz_cluster(reactive_clusters(), data = scaled_data,
                         geom = "point",
                         ellipse.type = "norm",
                         ggtheme = theme_minimal())
    plot +
      geom_point(
        data = selected_data,
        aes(x = Special_Score, y = normalized_score),
        color = "red",
        size = 5,
        shape = 8  # 星形或其他图标
      )
  })
  
  
  # Nearest Neighbors
  output$nearest_table <- renderTable({
    selected_episode <- input$episode
    selected <- podcast_data %>%
      filter(Episodename == selected_episode)%>%
      select(Special_Score, normalized_score) 
    distances <- apply(scaled_data, 1, function(x) sqrt(sum((x - selected)^2)))
    nearest <- podcast_data %>%
      mutate(distance = distances) %>%
      arrange(distance) %>%
      slice(2:6)  # Exclude self
    nearest[,1:6]
  })
}

shinyApp(ui = ui, server = server)