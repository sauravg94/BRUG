load("processed_sfo_crime_data.rdata")
str(sfo_crime_data)
new_crime<-sfo_crime_data %>% sample_n(1000)
head(new_crime)

new_crime$DayOfWeek <- ordered(new_crime$DayOfWeek, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# new_crime %>% group_by(DayOfWeek) %>% count(Crime_Category) %>% 
#   ggplot(aes(reorder(Crime_Category,n),n,fill=Crime_Category))+
#   geom_col()+facet_wrap(~DayOfWeek)+
#   coord_flip()
# 
# new_crime %>% group_by(Incident_Time_Tag) %>% count(Crime_Category) %>% ggplot(aes(fct_reorder(Crime_Category,n),n,fill=Crime_Category))+
#   geom_col(stat="identity")+
#   facet_wrap(~Incident_Time_Tag)+
#   coord_flip()

str(new_crime)

max(new_crime$Incident_Date)

new_crime$Crime_Category<-as.factor(new_crime$Crime_Category)

new_crime %>% group_by(Crime_Category) %>% count()
  
# shiny start
ui<-fluidPage(
  titlePanel("SFO Crime App"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(inputId = "date",
                     label = "Choose a date range",
                     start = min(new_crime$Incident_Date),
                     end = max(new_crime$Incident_Date),
                     min = min(new_crime$Incident_Date),
                     max = max(new_crime$Incident_Date),
                     format = "yyyy-mm-dd"
                     ),
      selectInput(inputId = "resolution",
                         label = "Choose a resolution type",
                         choices = levels(new_crime$Resolution),
                         selected = levels(new_crime$Resolution)[1]),
      selectInput(inputId = "category",
                         label = "Choose a crime category",
                         choices=levels(new_crime$Crime_Category),
                         selected = levels(new_crime$Crime_Category)[1]),
      selectInput(inputId = "day",
                  label = "Choose day of week",
                  choices = levels(new_crime$DayOfWeek),
                  selected = levels(new_crime$DayOfWeek)[1])
    ),
    mainPanel(dataTableOutput(outputId = "table"),
              leafletOutput(outputId = "map",width = 600,height = 400))
  )
)
server<-function(input,output){
  filData<-reactive({
    req(input$resolution)
    # req(input$category)
    new_crime %>% sample_n(300) %>% 
      filter(Incident_Date>=input$date[1],
             Incident_Date<=input$date[2],
             Resolution==input$resolution,
             Crime_Category==input$category,
             DayOfWeek==input$day)
  })
  output$table<-renderDataTable({
    filData() %>% group_by(Incident_Time_Tag) %>% count()
  })
  output$map<-renderLeaflet({
      filData() %>% group_by(Incident_Time_Tag) %>% leaflet() %>% 
      addTiles() %>% 
      addMarkers(lng = ~longitude,lat = ~latitude,popup = ~Incident_Date)
  })
}
shinyApp(ui=ui,server=server)