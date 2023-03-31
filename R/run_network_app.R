#' @import shiny shinyWidgets shinythemes
#' @import igraph tidygraph ggraph graphlayouts
#' @import plotly
#' @import devtools
#' @import dplyr tidyr
#' @import ggplotly
#' @export
run_network_app <- function() {

  node_choices <- c(member_meta_info$Member.ID)
  names(node_choices) <- c(member_meta_info$Full.Name)

  mem_name_choices <- member_meta_info %>%
    get_opts_list(Full.Name)

  mem_rt_choices <- member_meta_info %>%
    get_opts_list(`RT Affiliation`)

  mem_job_choices <- member_meta_info %>%
    get_opts_list(Profession)

  org_choices <- organization_meta_info$Org.ID
  names(org_choices) <- organization_meta_info$Name

  org_name_choices <- organization_meta_info %>%
    get_opts_list(Name)

  org_cat_choices <- organization_meta_info %>%
    get_opts_list(Type_Category)

  org_type_choices <- organization_meta_info %>%
    get_opts_list(Type)

  node_var <- "Member.ID"
  node_labels <- "Full.Name"
  edge_var <- "Org.ID"
  edge_labels <- "Name"

  full_data <- affiliation_dates %>%
    dplyr::left_join(member_meta_info) %>%
    dplyr::left_join(organization_meta_info)

  shinyApp(
      ui = tagList(
        navbarPage(
          "1989 Polish Round Table",
          theme = "journal",
          tabPanel("Setup",
                   sidebarPanel(

                     ## Go button
                     actionButton("setup", "Done with Setup", class = "btn-primary"),

                     h3("Choose which individuals will be shown in the network."),

                     h4("These choices are combined; for example, if you select
                        a profession and a specific person, that specific person will be
                        included whether or not they have that profession."),

                     pickerInput('node_include_rt',
                                 'Round Table affiliations to include:',
                                 choices = mem_rt_choices,
                                 options = list(`actions-box` = TRUE),
                                 multiple = TRUE,
                                 selected = mem_rt_choices
                     ),


                     pickerInput('node_include_job',
                                 'Professions to include:',
                                 choices = mem_job_choices,
                                 options = list(`actions-box` = TRUE),
                                 multiple = TRUE
                     ),

                     pickerInput('node_include_specific',
                                 'Specific individuals to include:',
                                 choices = mem_name_choices,
                                 options = list(`actions-box` = TRUE,
                                                liveSearch = TRUE),
                                 multiple = TRUE
                     ),

                     h3("Choose which organizations will be used to create edge connections."),

                     h4("These choices are combined; for example, if you select
                        a category and a specific org, that specific org will be
                        included whether or not it is in the category."),

                     pickerInput('edge_include_cat',
                                 'Categories of organization to include:',
                                 choices = org_cat_choices,
                                 options = list(`actions-box` = TRUE),
                                 multiple = TRUE,
                                 selected = org_cat_choices
                     ),

                     pickerInput('edge_include_type',
                                 'Types of organizations to include:',
                                 choices = org_type_choices,
                                 options = list(`actions-box` = TRUE,
                                                liveSearch = TRUE),
                                 multiple = TRUE
                     ),

                     pickerInput('edge_include_specific',
                                 'Specific organization to include:',
                                 choices = org_name_choices,
                                 options = list(`actions-box` = TRUE,
                                                liveSearch = TRUE),
                                 multiple = TRUE
                     ),

                     h3("Choose how edges will be computed and weighted."),

                     # How to compute edge weights
                     selectInput('edge_type',
                                 'One edge per organization, or bonus points for umbrella groups?',
                                 choices = c("Subgroup + Umbrella group" = "group_labs",
                                             "Organization ID" = "org_id"
                                             )
                     ),

                     # Make events "linger" for more than a month
                     sliderInput('event_length',
                                 'How many months after are Events be considered to last?',
                                 value = 1,
                                 min = 1, max = 600,
                                 sep = ""),

                     # Treat all connections as persistent
                     selectInput('lifelong',
                                 'Should connections last forever after initial affiliation?',
                                 choices = c("No" = "none",
                                             "Events only" = "events",
                                             "Yes, all connections" = "all"
                                             ),
                                 selected = "No"),

                     selectInput('cross',
                                 'Use cross-RT-group connections only?',
                                 choices = c("No" = FALSE, "Yes" = TRUE),
                                 selected = "No"
                     )
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

                     h3("Change Node Appearance"),

                     # Highlight a node by color
                     uiOutput("node_color_specific"),

                     # Highlight a node by shape
                     uiOutput("node_shape_specific"),

                     # Color groups
                     radioButtons('node_color_by_group',
                                  'Different colors for groups of:',
                                  choices = c(
                                    "None" = "None",
                                    "Round Table Affiliation" = "RT Affiliation",
                                    "Profession" = "Profession",
                                    "Gender" = "Gender")
                     ),

                     # Shape groups
                     # radioButtons('node_shape_by_group',
                     #              'Different shapes for groups of:',
                     #              choices = c(
                     #                "None" = "None",
                     #                "Round Table Affiliation" = "RT Affiliation",
                     #                "Profession" = "Profession",
                     #                "Gender" = "Gender")
                     # ),

                     # Resize nodes by
                     radioButtons('node_size',
                                  'Resize nodes by:',
                                  choices = c(
                                    "None" = "None",
                                    "Overall betweenness" = "betweenness",
                                    "Overall degree" = "degree",
                                    "Cross-group degree" = "cross_degree")
                     ),

                     sliderInput('node_size',
                                 "Base node size",
                                 value = 2,
                                 min = 0, max = 30),

                     h3("Change Edge Appearance"),

                     # Color groups
                     radioButtons('edge_color_cross',
                                  'Different colors for cross-connections?',
                                  choices = c(
                                    "No",
                                    "Yes")
                     ),

                     # Weight
                     radioButtons('edge_size_weight',
                                  'Resize by weight?',
                                  choices = c(
                                    "No",
                                    "Yes")
                     ),

                     # Selecting edge transparency
                     sliderInput('edge_transparency',
                                 'Edge Transparency',
                                 value = 0.3,
                                 min = 0, max = 1),


                     h3("Change graph layout algorithm."),

                     # Removing a node
                     radioButtons('network_layout',
                                 'Algorithm:',
                                 choices = c("fr", "nicely", "kk", "lgl", "mds"),
                                 selected = "kk"
                     ),
                   ),
                   mainPanel(
                     girafeOutput('my_network', width = "700px", height = "700px")
                     #tableOutput('test')
                   )
          ),
          tabPanel("Explore Metrics", "This panel is intentionally left blank")
        )
      ),
      server = function(input, output) {

        #### Setup Options ####

        ## edge_include_cat -> Type_Category
        ## edge_include_type -> Type
        ## edge_include_specific -> Name
        ## node_include_rt -> RT Affiliation
        ## node_include_job -> Profession
        ## node_include_specific -> Full.Name

        dat <- eventReactive(input$setup, {

          full_data %>%
            filter(Type_Category %in% input$edge_include_cat |
                     Type %in% input$edge_include_type |
                     Name %in% input$edge_include_specific) %>%
            filter(`RT Affiliation` %in% input$node_include_rt |
                     Profession %in% input$node_include_job |
                     Full.Name %in% input$node_include_specific) %>%
            adj_affil_list(input$lifelong,
                           input$event_length)
        })

        output$dataset <- renderDataTable(dat())


        #### Get Selected Dates ####
        first_date <- reactive(get_date(input$year1, input$month1, input$day1))

        last_date <- reactive(get_date(input$year1, input$month1, input$day1))

        #### After Setup and Before Network: ####
        ## Narrow down data by date
        ## Get options for drop-downs

        dat_limited <- reactive({
          dat() %>%
            filter(Start.Date <= last_date(),
                   End.Date >= first_date())
          })

        nodes_list <- reactive({
          dat_limited() %>%
            distinct(Member.ID, Full.Name) %>%
            arrange(Full.Name)
          })

        node_name_choices <- reactive({
            setNames(nodes_list()$Member.ID,
                     nodes_list()$Full.Name)
          })

        output$node_color_specific <- renderUI({
          pickerInput('node_color_specific',
                      'Highlight with color:',
                      choices = node_name_choices(),
                      options = list(`actions-box` = TRUE),
                      multiple = TRUE
          )
        })

        output$node_shape_specific <- renderUI({
          pickerInput('node_shape_specific',
                      'Highlight with shape:',
                      choices = node_name_choices(),
                      options = list(`actions-box` = TRUE),
                      multiple = TRUE
          )
        })



        my_edgelist <- reactive({

          #### Make Graph ####
          ## reactive: first_date
          ## reactive: last_date
          ## input: edge_type = group_labs or org_id

          if (input$edge_type == "group_labs") {

            dat_limited() %>%
              get_edgelist_members(on_cols = list("Umbrella",
                                                  c("Umbrella", "Subgroup")),
                                   start = first_date(),
                                   end = last_date())

          } else if (input$edge_type == "org_id") {

           dat_limited() %>%
              get_edgelist_members(on_cols = list("Org.ID"),
                                   start = first_date(),
                                   end = last_date())

          }

        }) %>%
        bindEvent(input$make_network)

        #### Calculate layout ####
        prev_layout <- NULL

        my_node_layout <- reactive({
          get_layout_df(my_edgelist() %>%
                          select(from, to, weight),
                        node_meta = nodes_list(),
                        node_var = node_var,
                        weight_col = "weight",
                        prev_layout = prev_layout,
                        algorithm = input$network_layout) %>%
            left_join(member_meta_info, by = c("name" = "Member.ID")) %>%
            mutate(
              None = "1",  # so that if "None" is selected, things don't change
            )
        })

        observeEvent(my_node_layout(), {
          prev_layout <- isolate(my_node_layout())
        })


        #### Set upnode appearance ####


        ## node_color_by_group: "None" or column name
        ## node_shape_by_group: "None" or column name
        ## node_color_specific: a Member.ID (matches "name")
        ## node_shape_specific: a Member.ID

        node_colors <- reactive({

          if (!is.null(input$node_color_specific)) {

              n <- length(unique(input$node_color_specific))
              these_cols <- ggcolors(n)
              cols <- rep("black", nrow(my_node_layout()))

              for (i in 1:n) {
                cols[my_node_layout()$name == input$node_color_specific[i]] <- these_cols[i]
              }
          } else {

            vals <- my_node_layout()[[input$node_color_by_group]] %>%
              factor() %>%
              as.integer()

            cols <- ggcolors(max(vals))[vals]

          }

          cols

        })

        node_shapes <- reactive({

          shapes = rep(19, nrow(my_node_layout()))

          if (!is.null(input$node_shape_specific)) {
            shapes[my_node_layout()$name == input$node_shape_specific] = 17
          }

          shapes

        })


        node_sizes <- reactive({

          sizes <- rep(input$node_size, nrow(my_node_layout()))
          all_highlighted <- c(input$node_shape_specific, input$node_color_specific)

          if (!is.null(all_highlighted)) {
            sizes[my_node_layout()$name %in% all_highlighted] = 3*input$node_size
          }

          sizes

        })



        #### Set up edge locations and info ####

        my_edgelist_locs <- reactive({

          my_edgelist() %>%
            left_join(my_node_layout() %>% rename_all(~paste0(.x,"_from")),
                      by = c("from" = "name_from")) %>%
            left_join(my_node_layout() %>% rename_all(~paste0(.x,"_to")),
                      by = c("to" = "name_to"))

        })



        #### Set up edge location and appearance ####
        ## edge_color_cross: T/F
        ## edge_size_weight: T/F

        edge_colors <- reactive({
          if (input$edge_color_cross == "Yes") {

             ggcolors(2)[(my_edgelist_locs()$`RT Affiliation_from` != my_edgelist_locs()$`RT Affiliation_to`) + 1]

          } else {

            "black"

          }

        })

        edge_weights <- reactive({
          if (input$edge_size_weight == "No") {

            1

          } else {

            my_edgelist_locs()$weight

          }

        })

        #output$test <- renderTable(my_edgelist_details())


        #### Plot it ####

        output$my_network <- renderGirafe({

          p <- my_node_layout() %>%
            ggplot() +
            geom_segment_interactive(data = my_edgelist_locs(),
                         aes(x = x_from, y = y_from,
                             xend = x_to, yend = y_to,
                             tooltip = edge_orgs),
                         alpha = input$edge_transparency,
                         color = edge_colors(),
                         linewidth = edge_weights()) +
            geom_point_interactive(aes(x = x, y = y,
                                       tooltip = Full.Name),
                       color = node_colors(),
                       shape = node_shapes(),
                       size = node_sizes()) +
            theme_void() +
            theme(aspect.ratio=1) +
            ggtitle(format(first_date(), "%b %d, %y") )

          girafe(ggobj = p) %>%
            girafe_options(
              opts_zoom(min = .5, max = 5),
             opts_tooltip(use_fill = TRUE,
                          use_stroke = TRUE)
            )
        })
      }
    )
} #function
