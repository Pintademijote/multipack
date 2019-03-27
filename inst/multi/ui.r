ui <- fluidPage(
  tabsetPanel(
    tabPanel("Analysis Models", Fluid=TRUE,
             titlePanel("Analyse multi-echelle"),
             sidebarLayout(
               sidebarPanel(
                 helpText("Afficher les relations entre metriques et la distance,
                          et le rsquared des modeles (abondance SP Vs metrique) a chaque echelle"),

                 fileInput("file_prop", h5("Fichier proprieties")),
                 fileInput("file_chloe", h5("Sortie CSV")),
                 fileInput("file_carab", h5("Carab")),
                 selectInput("metric",  h5("Metric"), choices = NULL),
                 selectInput("scales",  h5("Scale"), choices = NULL),
                 selectInput("species",  h5("Species"), choices = NULL)


                 ),
               mainPanel(
                 plotlyOutput("plot1", height = "400px") %>% withSpinner(color="#0dc5c1"),
                 plotlyOutput("plot2", height = "400px") %>% withSpinner(color="#0dc5c1"),
                 plotlyOutput("plot3", height = "400px") %>% withSpinner(color="#0dc5c1"),
                 plotlyOutput("plot4", height = "400px") %>% withSpinner(color="#0dc5c1")

               )
             )
    ),
    tabPanel("Map", Fluid=TRUE,
             titlePanel("Carte"),
             sidebarLayout(
               sidebarPanel(
                 shinyDirButton("directory_map",
                                "Add your path to the folder where ascii are stored", "Please select a folder",
                                FALSE),
                 fileInput("file_ascii", h5("Fichier Asci"), multiple=FALSE, accept = c(
                   "text/asc",
                   "text/comma-separated-values,text/plain",
                   ".asc")),
                 fileInput("file_points", h5("Coordonnees Points")),
                 actionButton("do", "Click Me")


               ),
               mainPanel(

                 leafletOutput("mymap",width = "100%", height = 600) %>% withSpinner(color="#0dc5c1"),
                 dataTableOutput('table_carab'),
                 textOutput("selected")

               )
             )
    ),
    tabPanel("Cor", Fluid=TRUE,
             titlePanel("Cor"),
             sidebarLayout(
               sidebarPanel(
                 helpText("Correlation"),
                 shinyDirButton("directory",
                                "Add your path to the folder where ascii are stored", "Please select a folder",
                                FALSE),
                 actionButton("analysis", "Run analysis"),
                 actionButton("test_toto", "analysis"),
                 fileInput("random_points", h5("Chloe output for random points"))


               ),
               mainPanel(
                 plotOutput("cor_plot1", height = "600") %>% withSpinner(color="#0dc5c1"),
                 plotOutput("cor_plot2", height = "600") %>% withSpinner(color="#0dc5c1")

               )
             )
    )
  )

)
