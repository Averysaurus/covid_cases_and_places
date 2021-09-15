
library(shiny)
library(tidyverse)


# import data.
cov_ex <- read_csv("Output.csv")

# tidy up column names
cov_ex <- cov_ex %>% 
    rename(building = `Building Name from Building Data (special handling)`,
           area_name = `Floor Or Area Name (Dynamically Suppress for Privacy)`,
           case_id = `Positive Case Id`, test_date = `Test Date`)

# create nodes. 
people_nodes <- cov_ex %>% 
    distinct(case_id) %>% 
    mutate(case_id = as.character(case_id)) %>% 
    rename(label = case_id)

building_nodes <- cov_ex %>% 
    distinct(building) %>% 
    rename(label = building)

cov_nodes <- full_join(people_nodes, building_nodes, by = "label") %>% 
    rowid_to_column("id") 

# create edges. 
case_per <- cov_ex %>% 
    group_by(building, case_id) %>% 
    summarise(weight = n()) %>% 
    ungroup() %>% 
    mutate(case_id = as.character(case_id))

cov_edges <- case_per %>% 
    left_join(cov_nodes, by = c("case_id" = "label")) %>% 
    rename(from = id)

cov_edge_fin <- cov_edges %>% 
    left_join(cov_nodes, by = c("building" = "label")) %>% 
    rename(to = id)

nodes_d3 <- mutate(cov_nodes, id = id - 1)
edges_d3 <- mutate(cov_edge_fin, from = from - 1, to = to - 1)

library(networkD3)

# script for click on node object.
MyClickScript <- 
    'd3.select(this).select("circle").transition()
.duration(750)
.attr("r", 30)'


ui <- bootstrapPage(
    # stlying and text
    tags$style(type = "text/css",
               "html, body {width:100%;height:100%;font-family: Courier New, sans-serif;}"),
    absolutePanel(
        top = 10, right = 10, style = "z-index:500; text-align: right;",
        tags$h1("COVID19 at UC Berkeley: Cases & Places"),
        tags$h4(HTML(paste("Explore connections with a few days worth of cumulative COVID19 exposure-risk data.")))),
        absolutePanel(
            top = 120, right = 20, style = "z-index:500; text-align: right;",
          tags$h5(HTML(paste("Hover on a node to explore its neighbors. Nodes with numbers represent test-positive cases and names are the buildings visited."))),
          tags$h5(HTML(paste("You can scroll around, zoom in and out. Click and drag a node to get a better sense of its connection with the network. <p> "))),
        tags$h5(HTML(paste("<p>Data Source: <a href='https://coronavirus.berkeley.edu/dashboard/workplace-exposure/'> UCB Workplace Exposure Dashboard</a> <p><p>"))),
        HTML(paste("<p> <p> Author: <a href= 'https://www.linkedin.com/in/averysaurus/' > Avery Richards, MPH(c)</a> <p>")),
    HTML(paste("<p> <p> Git <a href= 'https://github.com/Averysaurus/covid_cases_and_places' > repo</a> <p>"))),
    
    # network object
    forceNetworkOutput("cov_net", width = "100%", height = "100%")
)


server <- function(input, output) {
    # reactive for object
    output$cov_net <- renderForceNetwork({forceNetwork(Links = edges_d3,
                                                       Nodes = nodes_d3,
                                   Source = "from", Target = "to",  
                                   NodeID = "label", Group = "id", 
                                   Value = "weight", opacity = .85, 
                                   fontSize = 25, zoom = TRUE, 
                                   clickAction = MyClickScript, legend = F)})
}

 
shinyApp(ui = ui, server = server)
