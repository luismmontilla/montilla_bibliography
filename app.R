library(shiny)
library(bib2df)
library(tidyverse)
library(igraph)
library(visNetwork)


#
items <- bib2df('papers.bib')

edge_list <- items$AUTHOR %>% 
    purrr::map(combn, m = 2) %>% 
    flatten_chr() %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    data.frame()

g <- graph_from_data_frame(edge_list, directed = FALSE) 

E(g)$weight <- 1

g <- simplify(g, edge.attr.comb="sum")

gvis <- toVisNetworkData(g)

gvis$edges$value <- gvis$edges$weight
#

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    #titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            visNetworkOutput("network")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$network <- renderVisNetwork({
        visNetwork(nodes = gvis$nodes, 
                   edges = gvis$edges) %>% 
            visEdges(physics = FALSE) %>% 
            visInteraction(navigationButtons = TRUE)
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
