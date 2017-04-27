package_check <- require("dplyr")
if (package_check == FALSE) {
  install.packages('dplyr')
}
library("dplyr")

package_check <- require("GGally")
if (package_check == FALSE) {
  install.packages('GGally')
}
library("GGally")

package_check <- require("ggparallel")
if (package_check == FALSE) {
  install.packages('ggparallel')
}
library("ggparallel")

package_check <- require("d3heatmap")
if (package_check == FALSE) {
  install.packages('d3heatmap')
}
library("d3heatmap")

package_check <- require("plotly")
if (package_check == FALSE) {
  install.packages('plotly')
}
library("plotly")


library(shiny)
library(ggplot2)





df_fb <- read.csv('dataset_Facebook.csv', sep = ';')
df_fb <- na.omit(df_fb)

new_df <- df_fb %>% 
          dplyr::select(-c(Type, Page.total.likes, Post.Month)) %>%
          dplyr::slice(1:25) %>% dplyr::rename(Lifetime.Post.Page.Likers = Lifetime.Post.reach.by.people.who.like.your.Page, Lifetime.Likes.page.engage = Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post, Lifetime.Post.Impressions = Lifetime.Post.Impressions.by.people.who.have.liked.your.Page) 

df_fb$PostWeekday <- as.factor(df_fb$Post.Weekday)
df_fb$Post.Month <- as.factor(df_fb$Post.Month)

new_df2 <- df_fb %>% dplyr::select(c(Lifetime.Post.Total.Reach, PostWeekday, Lifetime.Post.Consumptions, Total.Interactions))

data <- df_fb

ui <- fluidPage(
  titlePanel("Facebook Data"),
  mainPanel( 
    tabsetPanel(
      tabPanel('Scatter Plot Matrix',
        plotOutput("scatterplot", height = 500, width = 950),
        selectInput("paid", "Show Paid or Not Paid", c("Not Paid", "Paid"))),
      tabPanel(title = "HeatMap",
        selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
        checkboxInput("cluster", "Apply clustering"),
        d3heatmapOutput("heatmap"), height = 600, width = 1000),
      tabPanel('Parallel Coordinates Map', plotlyOutput("parallel")))
    ))
   

server <- function(input, output, session) {
  paid_select <- reactive({
    switch(input$paid,
           "Paid" = df_fb[df_fb$Paid == 1,],
           "Not Paid" = df_fb[df_fb$Paid ==0,])
  })
  output$scatterplot = renderPlot({
    data1 <- paid_select()
    ggpairs(data=data1, # data.frame with variables
            columns=c(1,17,18, 19), # columns to plot, default to all.
            title="Facebook Scatter Plot Matrix", 
            mapping = aes(color = PostWeekday))
  })

  output$heatmap <- renderD3heatmap({
    d3heatmap(scale(new_df),
              colors = input$palette,
              width = 10,
              xaxis_height = 135,
              yaxis_width = 35,
              xaxis_font_size = 10,
              dendrogram = if (input$cluster) "both" else "none"

    ) })
  
  output$parallel = renderPlotly({
    t <- list(
      family = "sans serif",
      size = 18)
      new_df2 %>% plot_ly(
              type = 'parcoords',
              line = list(color = ~PostWeekday
                          ),
              dimensions = list(
                list(range = c(0,60000),
                     label = 'Post Reach Total', values = ~Lifetime.Post.Total.Reach),
                list(range = c(0,5000),
                     label = 'Post Consumptions ', values = ~Lifetime.Post.Consumptions),
                list(range = c(0,2000),
                     label = 'Total Interactions', values = ~Total.Interactions)
              ) 
                
      ) %>% layout(autosize = F, width = 900, height = 600, font = t)

            
})}

shinyApp(ui = ui, server = server)
