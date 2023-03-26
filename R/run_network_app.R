#' @import shiny shinyWidgets shinythemes
#' @import igraph tidygraph ggraph
#' @import plotly
#' @import devtools
#' @import dplyr tidyr
#' @export
run_network_app <- function() {

  node_choices <- c(member_meta_info$Member.ID)
  names(node_choices) <- c(member_meta_info$Full.Name)

  org_choices <- organization_meta_info$Org.ID
  names(org_choices) <- organization_meta_info$Name

  group_choices <- c(unique(as.character(member_meta_info$Profession)))
  group_choices <- group_choices[!is.na(group_choices)]
  #grouping_var <- "Profession"

  network_type <- "kk"

  node_var <- "Member.ID"
  node_labels <- "Full.Name"
  edge_var <- "Org.ID"
  edge_labels <- "Name"

  full_data <- affiliation_dates %>%
    dplyr::left_join(member_meta_info) %>%
    dplyr::left_join(organization_meta_info)

  ShinyApp(


    shinyApp(
      ui = tagList(
        navbarPage(
          "1989 Polish Round Table",
          theme = "journal",
          tabPanel("Setup",
                   # selectInput('edge_type',
                   #             'How should connections be calculated?',
                   #             choices = c("Organization ID",
                   #                         "Group + Subgroup"),
                   #             selected = 1
                   # ),
                   sidebarPanel(
                     sliderInput('event_length',
                                 'How long should connections from Events be considered to last?',
                                 value = 1,
                                 min = 1, max = 600,
                                 sep = ""),

                     selectInput('lifelong',
                                 'Should connections last forever after initial affiliation?',
                                 choices = c("Yes, all connections" = "all",
                                             "Events only" = "events",
                                             "No" = "none"),
                                 selected = "No"),

                     selectInput('cross',
                                 'Show cross-connections only?',
                                 choices = c("Yes" = TRUE, "No" = FALSE),
                                 selected = "No"
                     ),

                     pickerInput('edge_remove',
                                 'Remove organizations?',
                                 choices = org_choices,
                                 options = list(`actions-box` = TRUE),
                                 multiple = T
                     ),
                     ## Go button
                     actionButton("setup", "Done with Setup", class = "btn-primary")
                   ),
                   mainPanel(
                     dataTableOutput("dataset")
                   )
          ),
          tabPanel("Explore Social Network",
                   sidebarPanel(
                     ## Go button
                     actionButton("make_network", "Draw Network", class = "btn-primary"),
                     h3("Choose a Date"),
                     div(style="display: inline-block;vertical-align:top; width: 150px;",
                         selectInput('month1',
                                     'Month',
                                     choices = 1:12,
                                     selected = 1
                         )),
                     div(style="display: inline-block;vertical-align:top; width: 150px;",
                         selectInput('day1',
                                     'Day',
                                     choices = 1:31,
                                     selected = 1
                         )),
                     sliderInput('year1',
                                 'Year',
                                 value = 1979,
                                 min = 1945, max = 1989,
                                 sep = ""),
                     # h3("End of Date Range"),
                     # selectInput('month2',
                     #             'Month',
                     #             choices = 1:12
                     # ),
                     # selectInput('day2',
                     #             'Day',
                     #             choices = 1:31
                     # ),
                     # sliderInput('year2',
                     #             'Year',
                     #             value = 1979,
                     #             min = 1945, max = 1989,
                     #             sep = ""),

                     h3("Color Highlighting"),

                     # Highlight a node
                     pickerInput('node_highlight',
                                 'Highlight Individuals(s)',
                                 choices = node_choices,
                                 options = list(`actions-box` = TRUE),
                                 multiple = T
                     ),

                     # Highlight groups
                     pickerInput('group_highlight',
                                 'Highlight Profession',
                                 choices = group_choices,
                                 options = list(`actions-box` = TRUE),
                                 multiple = T
                     ),

                     # Color groups
                     radioButtons('color_by_group',
                                  'Color nodes by:',
                                  choices = c(
                                    "None" = "None",
                                    "Round Table Affiliation" = "RT Affiliation",
                                    "Profession" = "Profession",
                                    "Gender" = "Gender")
                     ),

                     h3("Aesthetics"),

                     # Selecting edge transparency
                     sliderInput('edge_transparency',
                                 'Edge Transparency',
                                 value = 1,
                                 min = 0, max = 1),

                     sliderInput('node_size',
                                 "Node Size",
                                 value = 10,
                                 min = 0, max = 30),

                     h3("Omit Nodes or Edges"),

                     # Removing a node
                     pickerInput('node_remove',
                                 'Remove Node(s)',
                                 choices = node_choices,
                                 options = list(`actions-box` = TRUE),
                                 multiple = T
                     ),

                     # Removing an edge
                     pickerInput('edge_remove',
                                 'Omit an organization from edge Calculation',
                                 choices = org_choices,
                                 options = list(`actions-box` = TRUE),
                                 multiple = T
                     )
                   ),
                   mainPanel(
                     plotlyOutput('my_network')
                     #textOutput("test")
                   )
          ),
          tabPanel("Explore Metrics", "This panel is intentionally left blank")
        )
      ),
      server = function(input, output) {

        #### Setup Options ####

        dat <- eventReactive(input$setup, {

          full_data %>%
            filter(!(Org.ID %in% input$edge_remove)) %>%
            adj_affil_list(input$lifelong,
                           input$event_length)
        })

        output$dataset <- renderDataTable(dat())

        output$text <- renderText(toString(input$group_highlight))

        #### Get Selected Dates ####
        first_date <- reactive(get_date(input$year1, input$month1, input$day1))

        last_date <- reactive(get_date(input$year1, input$month1, input$day1))


        my_graph <- eventReactive(input$make_network, {

          #### Make Graph ####

          dat() %>%
            filter(!(Member.ID %in% input$node_remove)) %>%
            filter(!(Org.ID %in% input$edge_remove)) %>%
            get_edgelist_members(on_cols = list("Primary Group",
                                                c("Primary Group", "Subgroup")),
                                 start = first_date(),
                                 end = last_date())

        })

        #### Calculate layout ####
        prev_layout <- NULL

        my_layout <- reactive({
          get_layout_df(my_graph(),
                        node_meta = member_meta_info,
                        node_var = node_var,
                        prev_layout = prev_layout,
                        algorithm = network_type)
        })

        observeEvent(my_layout(), {
          prev_layout <- isolate(my_layout())
        })

        #### Set node and edge details ####

        node_cols <- reactive({
          make_node_cols(my_layout(),
                         node_var = node_var,
                         group_var = "Profession",
                         grouping_var = input$color_by_group,
                         color_all_groups = input$color_by_group != "None",
                         highlight_groups = input$group_highlight,
                         highlight_nodes = input$node_highlight)
        })


        edge_cols <- reactive({
          make_edge_cols(my_graph(),
                         highlight_nodes = input$node_highlight)
        })


        #### Plot it ####

        output$my_network <- renderPlotly({

          make_network_plot(edge_info = my_graph(),
                            node_layout = my_layout(),
                            date_string = first_date(),
                            node_var = "Member.ID",
                            node_label_var = node_labels,
                            node_cols = node_cols(),
                            edge_cols = edge_cols(),
                            weighted_edges = TRUE,
                            edge_transparency = input$edge_transparency,
                            node_size = input$node_size)

        })
      }
    )
  ) #Shinyapp
} #function
