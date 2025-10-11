#' Default dataset.
#'
#' @format A default dataset includes:
#' \describe{
#'   \item{.id}{device serial number}
#'   \item{Moisture}{moisture data in percentage}
#'   \item{Radius}{radius data in micrometers}
#'   \item{T1}{Temperature 1 of TMS sensors}
#'   \item{T2}{Temperature 2 of TMS sensors}
#'   \item{T3}{Temperature 3 of TMS sensors}
#'   \item{date_time}{Date and time stamp in UTC}
#' }
"df"
#' @title PLOTeR
#'
#' @description
#' `PLOTeR()` Web application for time series data visualization and processing.
#'
#' @details
#' Developed for processing of data from dendrometers and soil moisture sensors.Mostly adapted for data structure of Tomst sensors. Reads data.frame named "df" as default.
#'
#' @examples
#' PLOTeR()
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput,renderDataTable, runExample))
#' @import readr utils ggplot2 dplyr lubridate tidyr dbscan DT shinyjqui reshape2 rlang tibble png
#' @importFrom plyr ldply
#' @importFrom zoo na.approx na.locf rollsum
#' @importFrom stats complete.cases sd na.omit median quantile
#' @importFrom shinyWidgets switchInput dropMenu dropdownButton materialSwitch updateMaterialSwitch
#' @importFrom shinyTime timeInput
#' @importFrom shinyjs hide useShinyjs hidden delay
#' @importFrom gridExtra grid.arrange
#' @importFrom grid grid.newpage grid.draw
#' @importFrom cowplot get_legend get_plot_component
#'
#' @export
PLOTeR = function (){
  # UI ----
  ui <- tagList(
    tags$head(tags$style(HTML(".checkbox-inline {
                          margin-left: 20px;
                          margin-right: 10px;}
                          .checkbox-inline+.checkbox-inline {
                          margin-left: 20px;
                          margin-right: 10px;}
                          #legend_value{height: 3vh}
                          #Auto_save_menu {height: 35px; margin-left: 10px;}
                          #GS_Button_done{background-color:#FFFFFF;
                          color: black}
                            #GS_Button_save{background-color:#FFFFFF;
                            color: black; height: 35px;}
                            #GS_Button_move{background-color:#FFFFFF;
                            color: black; height: 35px;}
                            #GS_Button_cancel{background-color:#FFFFFF;
                            color: black; height: 35px;}
                            #GS_Button_recalculate{background-color:#FFFFFF;
                            color: black; height: 35px;}
                            #GS_Button_summary{background-color:#FFFFFF;
                            color: black; height: 35px;}
                            #cleaner_recalculate_btn{background-color:#999999;
                            color: white}
                            #cleaner_add_btn{background-color:#999999;
                            color: white}
                            #cleaner_refresh_btn{background-color:#999999;
                            color: white}
                            #cleaner_upload_btn{background-color:#999999;
                            color: white}
                            "),
                         type="text/css", ".inline label{ display: table-cell; text-align: center; vertical-align: middle; }
                                   .inline .form-group { display: table-row;}")),

    navbarPage(paste0("PLOTeR ", "1.0.7"), position = "fixed-top",
                id = "navbar",
                    #tabpanel_Data ----
                    tabPanel("Data",
                             tags$style(type="text/css", "body {padding-top: 70px;}"),
                             fluidRow(shinyWidgets::dropdownButton(
                               inputId = "Auto_save_menu",
                               label = '',
                               status = "danger",
                               circle = FALSE,
                               icon = icon("cog", lib = "glyphicon"),
                               size = "sm",
                               inline = T,
                               fluidRow(
                                 column(12, shinyWidgets::materialSwitch("Auto_save_switch","Auto-save", status = "primary", value = F, width = "70%"))
                               )),
                             actionButton("df_uploader", "Upload data.frame", class = "btn-success",style = "height: 35px; margin-left: 10px;"),
                             actionButton("tomst_uploader", "Upload Tomst", class = "btn-success",style = "height: 35px; margin-left: 10px;")
                             ),
                             fluidRow(plotOutput("Data_tab_plot"),
                                      align = "center", style = "margin-left: 20px; margin-right: 20px;"),
                             fluidRow(column(12,uiOutput(outputId = "dynamicdate1"))),
                             fluidRow(column(12, uiOutput(outputId = 'dynamicsliderInput'),
                                             textOutput("SliderText")),
                                             style = "margin-left: 50px; margin-right: 50px;"),
                             fluidRow(column(12,
                                             # actionButton("goButton", "Render plot", class = "btn-primary"),
                                             actionButton("bar43", "Append data", class = "btn-danger"),
                                             actionButton("bar19", "Change interval", class = "btn-danger"),
                                             actionButton("bar17", "Subset data", class = "btn-danger"),
                                             actionButton("bar45", "Drop variable", class = "btn-danger"),
                                             align = "center", style = "margin-bottom: 0px;", style = "margin-top: 9px;",style = "margin-left: 10px;")

                               # column(12,
                               #               column(3,
                               #                      br(),
                               #                      br,
                               #                      # selectInput("variable_prim", "primary axis:",""),
                               #                      align = "left"),
                               #               # column(1, offset = 4, br(),
                               #               #        checkboxInput("sec_ax",HTML(paste0("sec axis",tags$sup("beta"))), value = F)),
                               #               column(3,uiOutput(outputId = "dynamicInput"),
                               #                      align = "right"
                               #               )),
                               #        style = "margin-left: 50px; margin-right: 50px;"
                             ),
                             fluidRow(column(3,
                                             column(3,checkboxInput("bar","All/None", value = F)),
                                             column(3,shinyWidgets::switchInput("meta_switch",size = "mini", onLabel = "AND", offLabel = "OR",onStatus = "info", offStatus = "info", value = T, inline = T, width = "auto"),
                                                    align = "center", style = "margin-bottom: 0px;", style = "margin-top: 9px;"),
                                             column(1,actionButton("refresh_meta", "",icon = icon("refresh", lib = "glyphicon"), status = "primary",align = "center", style="padding:4px 8px; font-size:90%;",style = "margin-top: 9px;")))

                             ),
                             fluidRow(uiOutput(outputId = 'multifilter2')
                             ),
                             fluidRow(
                               h3(textOutput("caption")),
                               dataTableOutput("table"),
                               align = "center", style = "margin-left: 10px; margin-right: 10px;")
                    ),
                    #tabpanel_Plot ----
                    tabPanel("Plot",
                             tags$style(type="text/css", "body {padding-top: 70px;}"),
                             fluidRow(column(7,
                                             div(style = "display: flex; justify-content: left;",
                                                 div(shinyWidgets::dropdownButton(label = 'Dendro',
                                                                    status = "danger",
                                                                    circle = FALSE,
                                                                    icon = icon("stats", lib = "glyphicon"),
                                                                    # fluidRow(
                                                                    #   column(12,actionButton(inputId = 'bar29', label = 'Freeze subtract', width = '100%'))
                                                                    # ),
                                                                    fluidRow(
                                                                      column(12,actionButton(inputId = 'bar30', label = 'GRO_TWD_FREEZE', width = '100%'))
                                                                    ),
                                                                    fluidRow(
                                                                      column(12, actionButton(inputId = 'bar36', label = 'Growing season', width = '100%'))
                                                                    )
                                                 )),
                                                 div(style = "width: 5px;"),
                                                 div(shinyWidgets::dropdownButton(label = 'Edit',
                                                                                  status = "danger",
                                                                                  circle = FALSE,
                                                                                  icon = icon("stats", lib = "glyphicon"),
                                                                                  fluidRow(
                                                                                    column(12,actionButton(inputId = 'bar8', label = 'Auto level-up', width = '100%'))
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(12,actionButton(inputId = 'bar9', label = 'Level-up', width = '100%'))
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(12,actionButton(inputId = 'bar15', label = 'Interpolate', width = '100%'))
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(12,actionButton(inputId = 'bar16', label = 'Move-up', width = '100%'))
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(12,actionButton("bar5", "Delete", width = '100%')
                                                                                    ))
                                                 )),
                                                 div(style = "width: 5px;"),
                                                 div(shinyWidgets::dropdownButton(label = 'Data',
                                                                    status = "danger",
                                                                    circle = FALSE,
                                                                    icon = icon("stats", lib = "glyphicon"),
                                                                    fluidRow(
                                                                      column(12,actionButton("bar10", "Subtract offset", width = '100%')
                                                                      )),
                                                                    # fluidRow(
                                                                    #   column(12,actionButton("bar12", "Subtract TMS offset", width = '100%')
                                                                    #   )),
                                                                    fluidRow(
                                                                      column(12,actionButton("bar14", "Append data", width = '100%')
                                                                      )),
                                                                    fluidRow(
                                                                      column(12,actionButton("bar11", "Subset data", width = '100%')
                                                                      )),
                                                                    fluidRow(
                                                                      column(12,actionButton("bar44", "Deselect brushed", width = '100%')
                                                                      )),
                                                                    fluidRow(
                                                                      column(12,actionButton("fill_gap_action", "Fill NAs", width = '100%')
                                                                      )),
                                                                    fluidRow(
                                                                      column(12,actionButton("remove_stair_action", "Remove stairs", width = '100%')
                                                                      )),
                                                                    fluidRow(
                                                                      column(12,actionButton("Reconstruct_action",  HTML(paste0("Remove noise",tags$sup("beta"))), width = '100%')
                                                                      ))

                                                 )),
                                                 div(style = "width: 5px;"),
                                                 div(shinyWidgets::dropdownButton(label = 'Outputs',
                                                                    status = "info",
                                                                    circle = FALSE,
                                                                    icon = icon("stats", lib = "glyphicon"),
                                                                    fluidRow(
                                                                      column(12,actionButton(inputId = 'bar7', label = 'Summary', width = '100%'))
                                                                    ),

                                                                    fluidRow(
                                                                      column(12,downloadButton("bar2","Export data", style = "width:100%;"))
                                                                    ),
                                                                    fluidRow(
                                                                      column(12,downloadButton("export_plot_btn", "Export plot", style = "width:100%;"))
                                                                    )
                                                 )),
                                                 div(style = "width: 5px;"),
                                                 div(shinyWidgets::dropMenu(actionButton(inputId = "Plotsgeneral",
                                                                                         label = 'Plots',
                                                                                         class = "btn-primary",
                                                                                         circle = FALSE,
                                                                                         icon = icon("cog", lib = "glyphicon")),
                                                                            fluidRow(
                                                                              column(12,align="left",
                                                                                     fluidRow(
                                                                                       column(5,shinyWidgets::materialSwitch("legend_switch","Legend", status = "primary", right = T, value = T)),
                                                                                       column(7,conditionalPanel(condition = "input.legend_switch == true",
                                                                                                                 uiOutput(outputId = 'legend_val')))),
                                                                                     shinyWidgets::materialSwitch("one_by_one_switch","Show all", status = "primary", value = F,right = T),
                                                                                     br(),
                                                                                     shinyWidgets::materialSwitch("cleaner_mode_switch",HTML(paste0("Cleaning mode",tags$sup("beta"))), status = "primary", value = F, right = T)
                                                                              )
                                                                            ),
                                                                            # hideOnClick = "toogle"
                                                                            hideOnClick = T
                                                 )),
                                                 div(style = "width: 5px;"),
                                                 div(shinyWidgets::dropMenu(actionButton(inputId = "Upperplot",
                                                                           label = 'Upper plot',
                                                                           class = "btn-primary",
                                                                           circle = FALSE,
                                                                           icon = icon("cog", lib = "glyphicon")),
                                                              fluidRow(
                                                                column(12,align="left",
                                                                       selectInput("variable_prim", "Variable",""),
                                                                       selectInput("upper_plot_interval_input", "Interval",choices = c("original","min", "hour","day")),
                                                                       shinyWidgets::materialSwitch("group_switch_upper_plot","Group", status = "primary", right = T),
                                                                       conditionalPanel(condition = "input.group_switch_upper_plot == true",
                                                                                        selectInput("Groupby_upper_plot", NULL,""),
                                                                                        selectInput("method_upper_plot", "Method",c("auto", "lm", "glm", "gam", "mean"),
                                                                                                    selected = "auto"),
                                                                                        shinyWidgets::materialSwitch("se_switch_upper_plot","SE",value = T, status = "info", width = "50%"),
                                                                                        shinyWidgets::materialSwitch("regre_switch",HTML(paste0("Regression",tags$sup("beta"))), status = "info")
                                                                       ),
                                                                       column(10,actionButton(inputId = 'subtract_upper_plot', label = "Subtract offset", width = "100%")),
                                                                       column(2,actionButton("bar39", "",icon = icon("refresh", lib = "glyphicon"), status = "primary")),
                                                                       # shinyWidgets::materialSwitch("freeze_switch","Show Freeze", status = "primary", value = F, right = T),
                                                                       # conditionalPanel(condition = "input.freeze_switch == true",
                                                                       #                  selectInput("method3", "Method:", choices = c("Raw", "Fine"), selected = "Raw")
                                                                       #                  # ,
                                                                       #                  # numericInput("bar32", "Minpoints:", min = 3, max = 60, step = 1, value = 20),
                                                                       #                  # numericInput("bar33", "Min T:", min = -10, max = 10, step = 1, value = -5),
                                                                       #                  # numericInput("bar34", "Mean",  min = -10, max = 10, step = 1, value = 5)
                                                                       # ),
                                                                       # shinyWidgets::materialSwitch("anomalies_switch","Show Anomalies", status = "primary", value = F, right = T),
                                                                       # conditionalPanel(condition = "input.anomalies_switch == true",
                                                                       #                  column(6,numericInput("minpoints_val", "Density threshold:", min = 10, max = 200, step = 10, value = 100)),
                                                                       #                  column(6,actionButton("bar38", "",icon = icon("refresh", lib = "glyphicon"), status = "primary", style = 'margin-top:23px'))
                                                                       # ),
                                                                )
                                                              ),
                                                              # hideOnClick = "toogle"
                                                              hideOnClick = T
                                                 )),
                                                 div(style = "width: 5px;"),
                                                 div(shinyWidgets::dropdownButton(inputId = "Lowerplot",
                                                                    label = 'Lower plot',
                                                                    status = "primary",
                                                                    circle = FALSE,
                                                                    icon = icon("cog", lib = "glyphicon"),
                                                                    fluidRow(
                                                                      column(12,align="left",
                                                                             selectInput("variable_prim_lower", "Variable",""),
                                                                             shinyWidgets::materialSwitch("lower_plot_switch","Lower plot", status = "primary"),
                                                                             selectInput("lower_plot_interval_input", "Interval",choices = c("original","min", "hour","day")),
                                                                             shinyWidgets::materialSwitch("group_switch_lower_plot","Group", status = "primary"),
                                                                             conditionalPanel(condition = "input.group_switch_lower_plot == true",
                                                                                              selectInput("Groupby_lower_plot", NULL,""),
                                                                                              selectInput("method_lower_plot", "Method",c("auto", "lm", "glm", "gam", "mean"),
                                                                                                          selected = "auto"),
                                                                                              shinyWidgets::materialSwitch("se_switch_lower_plot","SE",value = T, status = "info", width = "50%")
                                                                             ),
                                                                             column(10,actionButton(inputId = 'subtract_lower_plot', label = "Subtract offset", width = "100%")),
                                                                             column(2,actionButton("bar39", "",icon = icon("refresh", lib = "glyphicon"), status = "primary"))
                                                                      )
                                                                    )))
                                             )
                             ),
                             column(5,
                                    # column(3, offset = 1,uiOutput(outputId = 'one_by_one_group_action')),
                                    # column(4, uiOutput(outputId = 'one_by_one_group'))
                                    uiOutput(outputId = "one_by_one_group")
                             )),
                             fluidPage(
                               br(),
                               fluidRow(verbatimTextOutput(outputId='ggplot_warnings'),
                                        tags$head(tags$style("#ggplot_warnings{color:red; font-size:12px; font-style:italic;
                                                             overflow-y:scroll; max-height: 50px; background: ghostwhite;}"))),
                               useShinyjs(),
                               # fluidRow(column(6,offset = 2 ,hidden(verbatimTextOutput(outputId = "moveupInput_div"))),
                                fluidRow(column(6,offset = 2 ,hidden(numericInput(inputId = "moveupInput_div", label = "", value = 0))),
                                        tags$head(tags$style("#moveupInput_div{color:black; font-size:14px;
                                                             background: ghostwhite;}")),
                                        column(2,offset = 2 ,hidden(actionButton('moveupInput', "Confirm", width = "100%"))),
                                        tags$head(tags$style("#moveupInput{color:white; font-size:14px;
                                                             background: green;}"))
                               )
                             ),
                             fluidPage(
                               fluidRow(align = "center",
                                        uiOutput(outputId = 'legend_sizable')),
                               fluidRow(uiOutput(outputId = "dynamicInput_upperplot")),
                               fluidRow(uiOutput(outputId = 'dynamicInput_lowerplot')),
                               fluidRow(align = "center",
                                        uiOutput(outputId = 'dynamicInput_cleaner'))
                             ),
                               # fluidRow(column(
                               #   12,
                                 # conditionalPanel(
                                 #   condition = "input.cleaner_mode_switch == true",
                                 #   column(
                                 #     3,
                                 #     selectInput(
                                 #       "quantile_correct_input",
                                 #       "Method",
                                 #       c("Quantiles", "Z-score"),
                                 #       selected = "Quantiles"
                                 #     ),
                                 #     selectInput(
                                 #       "compare_within",
                                 #       "Compare within",
                                 #       c(
                                 #         "within days and across ids",
                                 #         "within ids and across days",
                                 #         "across days and ids"
                                 #       ),
                                 #       selected = "within days and across ids"
                                 #     )
                                 #   ),
                                 #   column(
                                 #     3,
                                 #     shinyWidgets::materialSwitch(
                                 #       "compare_to_selected",
                                 #       "Compare to selected",
                                 #       value = F,
                                 #       status = "info",
                                 #       width = "50%"
                                 #     ),
                                 #     shinyWidgets::materialSwitch(
                                 #       "ampl_switch",
                                 #       "Check amplitude",
                                 #       value = T,
                                 #       status = "info",
                                 #       width = "50%"
                                 #     ),
                                 #     shinyWidgets::materialSwitch(
                                 #       "mean_switch",
                                 #       "Check mean",
                                 #       value = F,
                                 #       status = "info",
                                 #       width = "50%"
                                 #     ),
                                 #     shinyWidgets::materialSwitch(
                                 #       "rollingwindow_switch",
                                 #       "Remove isolated days",
                                 #       value = F,
                                 #       status = "info",
                                 #       width = "50%"
                                 #     ),
                                 #     shinyWidgets::materialSwitch(
                                 #       "cleaner_levelup_switch",
                                 #       "Level-up",
                                 #       value = F,
                                 #       status = "info",
                                 #       width = "50%"
                                 #     ),
                                 #     shinyWidgets::materialSwitch(
                                 #       "accept_nas2_switch",
                                 #       "Across NAs",
                                 #       value = F,
                                 #       status = "info",
                                 #       width = "50%"
                                 #     ),
                                 #     conditionalPanel(condition = "cleaner_levelup_switch == TRUE",
                                 #                      column(
                                 #                        12,
                                 #                        numericInput(
                                 #                          "cleaner_levelup_threshold",
                                 #                          "Level over:", value = 1000, min = 100, max = 9000, step = 100
                                 #                        )
                                 #                      )),
                                 #     conditionalPanel(condition = "rollingwindow_switch == TRUE",
                                 #                      column(
                                 #                        12,
                                 #                        numericInput(
                                 #                          "roll_window",
                                 #                          "Remove isolated days:", value = 3, min = 1, max = 100, step = 1
                                 #                        )
                                 #                      )),
                                 #   ),
                                 #   column(
                                 #     3,
                                 #     conditionalPanel(condition = "input.quantile_correct_input == 'Quantiles'",
                                 #                      column(
                                 #                        12,
                                 #                        numericInput(
                                 #                          "quantile_upper_threshold",
                                 #                          "quantile upper",
                                 #                          value = .95,
                                 #                          min = .5,
                                 #                          max = 1,
                                 #                          step = .01
                                 #                        ),
                                 #                        numericInput(
                                 #                          "quantile_lower_threshold",
                                 #                          "quantile lower",
                                 #                          value = .05,
                                 #                          min = 0,
                                 #                          max = .5,
                                 #                          step = .01
                                 #                        )
                                 #                      )),
                                 #     conditionalPanel(condition = "input.quantile_correct_input == 'Z-score'",
                                 #                      column(
                                 #                        12,
                                 #                        numericInput(
                                 #                          "z_score_threshold",
                                 #                          "z score",
                                 #                          value = 3,
                                 #                          min = 0,
                                 #                          max = 5,
                                 #                          step = .5
                                 #                        )
                                 #                      )),
                                 #     numericInput(
                                 #       "remove_all_threshold",
                                 #       "delete all if missing more than (%)",
                                 #       value = 75,
                                 #       min = 5,
                                 #       max = 100,
                                 #       step = 1
                                 #     )
                                 #   ),
                                 #   column(2,
                                 #          actionButton(inputId = "cleaner_recalculate_btn", label = "Recalcuate", width = "50%"),
                                 #          actionButton(inputId = "cleaner_add_btn", label = "Add", width = "50%"),
                                 #          actionButton(inputId = "cleaner_refresh_btn", label = "Refresh", width = "50%"),
                                 #          actionButton(inputId = "cleaner_upload_btn", label = "Upload", width = "50%")
                                 #   )
                                 # )
                               # )
                               # ),
                               fluidRow(align = "center",
                                        uiOutput(outputId = 'dynamicInput_GS')),
                             fluidRow(
                               column(width = 12,
                                      wellPanel(
                                        h4("Points selected by brushing:"),
                                        dataTableOutput("plot_brushed_points"))
                               )
                             )
                    )

  )
)





  # Server ----
  server <- function(input, output, session) {
    pos <- 1
    envir = as.environment(pos)

    options(shiny.maxRequestSize=30*1024^2)

    freeze_show_first <- GRO <- freeze_show2_first <- Radius_first <- T1_freeze2 <- lead_trail2 <- lead_trail1 <-
      T1_freeze <- freeze_show2 <- cluster_zero2 <- cluster_zero<- cluster <- x <- man_int_var <- man_lev_var <-
      Radius.x <- Radius.y <- Radius <- freeze_show2 <- data_se <-
      data_mean <- .id <- n_to_remove <- date_time <- first <- last <- fill_lin <- fill_avg <- na_order<-
      left <- right <- fill_avg_right <- fill_avg_left <- fill_avg2 <- na_n <- fill_avg_mean <- Index_level2 <- nas <-
      meanx <- meany <- Variable <- rnum  <- beforeafter <- mins <-  inflect <- GS <- GS_start <- GS_end <- jumps <-
      level_day <- level_week <- rect_ids <- stair_variable <- jump <- tms_calib_data <- NULL

    Sys.setenv(LANGUAGE="en")
    Sys.setlocale("LC_TIME", "English")
    #dashboards ----
    #inputs ----
    #data subsetting
    # lead_trail_nas_filter <- function(x) {cumsum(!is.na(x)) != 0 & rev(cumsum(!is.na(rev(x)))) != 0} - do not use, by filtering one variable, another could be also trimed though there are no NAs
    # d <- reactiveValues(b = df %>% group_by(.id) %>% filter(if_any(where(is.numeric), ~lead_trail_nas_filter(.))) %>% droplevels()  %>% as.data.frame())
    d <- reactiveValues(b = df %>% group_by(.id) %>% droplevels() %>% as.data.frame())
    observe({
      # d$a = d$b[which(d$b[['date_time']] >= input$period[1] & d$b[['date_time']] <= input$period[2] & d$b[['.id']] %in% input$.id),]  %>% dplyr::arrange(.id, date_time) %>% group_by(.id) %>% filter_if(is.numeric, ~lead_trail_nas_filter(.)) %>% as.data.frame()
      d$a = d$b[which(d$b[['date_time']] >= input$period[1] & d$b[['date_time']] <= input$period[2] & d$b[['.id']] %in% input$.id),]
    })
    #metafile
    df_meta = reactive({
      d$b %>% select_if(is.factor) %>% distinct() %>% relocate(.id) %>% as.data.frame()
    }) %>% debounce(1000)
    #dynamic function to filter data based on multiple inputs----
    min_max <- function(x, method = "min", na.rm = T, inf_fix = NA) {if(method %in% c("min", "max")) {
      if(method == "min"){
        if(length(x)>0 & !all(is.na(x))) min(x, na.rm = na.rm) else inf_fix
      }else{
        if(length(x)>0 & !all(is.na(x))) max(x, na.rm = na.rm) else inf_fix
      }
    } else {
      warning("wrong method")
    }
    }
    output$multifilter2 = renderUI({
      div(style = "margin-left: 10px; margin-right: 10px",
      lapply(1:length(isolate(df_meta())), function(i) {
        checkboxGroupInput(paste0(colnames(isolate(df_meta()))[i]),
                           paste0(colnames(isolate(df_meta()))[i]),
                           choices = sort(levels(df_meta()[[colnames(isolate(df_meta())[i])]])),
                           selected = character(0),
                           inline = T)
      })
      )
    })
    #dynamic filter3
    filt = reactive({
      dfs <- lapply(colnames(dplyr::select(isolate(df_meta()),-.id)), function(d) {
        isolate(df_meta()) %>% filter(isolate(df_meta())[[d]] %in% input[[d]]) %>% droplevels() %>% select(.id) %>% as.data.frame()
      })
      if (length(dfs) > 1){
        dfs = dfs[sapply(dfs, nrow)>0]
        if(isTRUE(input$meta_switch)){
          filtered = as.data.frame(dfs[[1]])
          if(length(dfs)>1){
          for(i in 1:(length(dfs)-1)) {
            filtered = filtered %>% dplyr::filter(.id %in% dfs[[i+1]]$.id)}
            }
          filtered = filtered %>% select(.id) %>% distinct()
        }else{
          filtered = plyr::ldply(dfs, data.frame)
          filtered = filtered %>% select(.id) %>% distinct()
        }
      } else {
        filtered <- as.data.frame(dfs[[1]])
      }
      filtered = droplevels(filtered)
      return(filtered$.id)
    })
    len = reactive({range(d$b[['date_time']], na.rm =T)}) %>% debounce(1000)
    #dynamic input for date_time range----
    debounced_id = reactive({input$.id}) %>% debounce(1000)
    output$dynamicsliderInput = renderUI({
      div(style = "margin-left: 50px; margin-right: 50px",
      sliderInput("period",
                  "Select period:",
                  value = c(isolate(len()[1]),isolate(len()[2])),
                  min = isolate(len()[1]),
                  max = isolate(len()[2]),
                  timeFormat = "%Y-%m-%d",
                  width = "100%",
                  timezone = "+0000")
      )
    })
    observeEvent(debounced_id(),{
      len2 = reactive({
        if(is.null(input$.id)){
          range(d$b[['date_time']], na.rm =T)
        } else {
          range(d$b[d$b[[".id"]] %in% input$.id, "date_time"], na.rm =T)
        }
      })
      updateSliderInput(session = session, "period",
                        # value = c(keepvalueslider1(),keepvalueslider2()),
                        min = isolate(len2())[1],
                        max = isolate(len2())[2],
                        timeFormat = "%Y-%m-%d",
                        timezone = "+0000")
    })
    #dynamic manual slider input
    output$dynamicdate1 = renderUI({
      div(
        style = "display: flex;
    justify-content: center;",
        div(shiny::dateInput("daterange1", NULL, format = "yyyy-mm-dd",startview = "decade",
                      min = isolate(len())[1], max= isolate(len())[2], value= as.Date(isolate(len())[1]),
                      width = "100px")),
        div(style = "width: 5px;"),
        div(shinyTime::timeInput("time1", NULL,seconds = F, minute.steps = 15, value = input$period[1])),
        div(style = "width: 5px;"),
        div(actionButton("tuneslider", "Tune date")),
        div(style = "width: 5px;"),
        div(shinyTime::timeInput("time2", NULL,seconds = F, minute.steps = 15, value = input$period[2])),
        div(style = "width: 5px;"),
        div(shiny::dateInput("daterange2",NULL, format = "yyyy-mm-dd",startview = "decade",
                      min = isolate(len())[1], max= isolate(len())[2], value= as.Date(isolate(len())[2]),
                      width = "100px"))
      )
    })
    observeEvent(input$period,{
      updateDateInput(session, "daterange1", value= as.Date(input$period[1]))
      updateDateInput(session, "daterange2", value= as.Date(input$period[2]))
    })
    observeEvent(input$tuneslider,{
      starting = strptime(paste(input$daterange1,' ', format(input$time1, '%H:%M:%S')),"%Y-%m-%d %H:%M:%S", tz = 'UTC')
      ending =  strptime(paste(input$daterange2,' ', format(input$time2, '%H:%M:%S')),"%Y-%m-%d %H:%M:%S", tz = 'UTC')
      updateSliderInput(session,'period', value = c(starting, ending),timeFormat = "%Y-%m-%d")
    })
    #axis choices removing columns including only NA
    f <- reactive({
      f_col = colnames(Filter(is.numeric, select_if(d$a,function(x){!all(is.na(x))})))
    })
    sele_prim <- reactive({
      if(input$variable_prim %in% f()){
        return(input$variable_prim)
      } else {
        return(NULL)
      }
    })
    sele_prim_lower <- reactive({
      if(input$variable_prim_lower %in% f()){
        return(input$variable_prim_lower)
      } else {
        return(NULL)
      }
    })
    sele_sec <- reactive({
      if(is.null(input$variable_sec)){
        return(NULL)
      } else {
        if(input$variable_sec %in% f()){
          return(input$variable_sec)
        } else {
          return(NULL)
        }
      }

    })
    # dynamic input for secondary axis
    output$dynamicInput <- renderUI({
      if(isTRUE(input$sec_ax)){
        div(style = "text-align:left;",
            selectInput("variable_sec", "secondary axis:",
                            choices = f(), selected = sele_sec())
        )

      } else {
        return(NULL)
      }

    })
    #dynamic input for primary axis
    observe({
      updateSelectInput(session, "variable_prim",
                        choices = f(), selected = sele_prim())
    })
    observe({
      updateSelectInput(session, "variable_prim_lower",
                        choices = f(), selected = sele_prim_lower())
    })
    #dynamic input for grouping
    #axis choices removing columns including only NA
    f2 <- reactive({
      c("none",colnames(Filter(is.factor, d$a)))
    })
    observe({
      updateSelectInput(session, "Groupby_upper_plot",
                        choices = f2())
    })
    observe({
      updateSelectInput(session, "Groupby_lower_plot",
                        choices = f2())
    })
    #input for one_by_one select
    output$one_by_one_group <- renderUI({

      div(style = "display: flex; justify-content: right; margin-right: 10px;",
          div(if(isFALSE(input$one_by_one_switch & length(input$.id)>=1)){
            splitLayout(cellWidths = c("50%", "50%"),
                        actionButton("bar25", "", icon = icon("minus", lib = "glyphicon")),
                        actionButton("bar26", "", icon = icon("plus", lib = "glyphicon")))
          } else {
            return(NULL)
          }),
          div(style = "width: 5px;"),
          div(style = "width: 200px", if(isFALSE(input$one_by_one_switch & length(input$.id)>=1)){
            selectInput("one_by_one_group_select", NULL, width = "100%",choices = input$.id, multiple = F)
          } else {
            return(NULL)
          })
      )
    })

    observe({
      updateSelectInput(session, "one_by_one_group_select",
                        choices = input$.id)
    })
    observeEvent(input$bar25, {
      one_by_one_list = data.frame(name = input$.id, rank = seq(1,length(input$.id)))
      one_by_one_selected = one_by_one_list[which(one_by_one_list$name == input$one_by_one_group_select),"rank"]
      if(one_by_one_selected <= 1){
        one_by_one_prev = input$.id[1]
      } else {
        one_by_one_prev = input$.id[one_by_one_selected-1]
      }
      updateSelectInput(session, "one_by_one_group_select",
                        choices = input$.id,
                        selected = one_by_one_prev)
    })
    observeEvent(input$bar26, {
      one_by_one_list = data.frame(name = input$.id, rank = seq(1,length(input$.id)))
      one_by_one_selected = one_by_one_list[which(one_by_one_list$name == input$one_by_one_group_select),"rank"]
      if(one_by_one_selected >= length(input$.id)){
        one_by_one_prev = input$.id[length(input$.id)]
      } else {
        one_by_one_prev = input$.id[one_by_one_selected+1]
      }
      updateSelectInput(session, "one_by_one_group_select",
                        choices = input$.id,
                        selected = one_by_one_prev)
    })
    observe({
      input$refresh_meta
      updateCheckboxGroupInput(
        session, ".id",
        # choices = levels(isolate(d$b[[".id"]])),
        selected = if(any(sapply(colnames(dplyr::select(isolate(df_meta()),-.id)), function(d) {
          !is.null(input[[d]])
        }))){filt()}else{character(0)},
        inline = T
      )
    })
    observeEvent(input$bar,{
      if(isFALSE(input$bar)){
        lapply(1:length(Filter(is.factor, isolate(df_meta()))), function(i) {
          updateCheckboxGroupInput(session,
                                   paste0(colnames(isolate(df_meta()))[i]),
                                   # choices = NULL,
                                   selected = character(0),
                                   inline = T
          )
        })
      } else {
        updateCheckboxGroupInput(
          session, ".id", choices = sort(levels(d$b[[".id"]])),
          selected = levels(d$b[[".id"]]),inline = T
        )
      }
    }, ignoreInit = T)
    # dynamic input of upper plot
    output$dynamicInput_upperplot <- renderUI({
      if(req(input$navbar) == "Plot"){
        plotOutput('Plot_tab_upper_plot',
                   dblclick = if(isTRUE(plot_GS$active) & isFALSE(input$GS_switch_plot)){NULL}else{"RadiusPlot_dblclick"},
                   brush = if(isTRUE(plot_GS$active) & isFALSE(input$GS_switch_plot)){NULL}else{brushOpts(
                     id = "RadiusPlot_brush",
                     resetOnNew = FALSE, direction = if(isTRUE(plot_GS$active)){"x"}else{"xy"})})
      }
    })
    # dynamic input of lower plot
    output$dynamicInput_lowerplot <- renderUI({
      if(req(input$navbar) == "Plot" & isTRUE(input$lower_plot_switch)){
        plotOutput('Plot_tab_lower_plot',
                   dblclick = if(isTRUE(plot_GS$active) & isTRUE(input$GS_switch_plot)){NULL}else{"RadiusPlot_dblclick"},
                   brush = if(isTRUE(plot_GS$active) & isTRUE(input$GS_switch_plot)){NULL}else{brushOpts(
                     id = "RadiusPlot_brush",
                     resetOnNew = F, direction = if(isTRUE(plot_GS$active)){"x"}else{"xy"})})
      } else {
        return(NULL)
      }
    })
    # dynamic input of second plot in modal window
    #output$dynamicInput3 <- renderUI({
    #if(isTRUE(input$lower_plot_switch)){
    #plotOutput("plot_export2")
    #} else {
    #return(NULL)
    #}
    #})
    # GS menu----
    output$dynamicInput_GS <- renderUI({
      if(isTRUE(plot_GS$active)){
        tags$div(
          actionButton("GS_Button_done", "Done"),
          actionButton("GS_Button_move", "Move/Add"),
          actionButton("GS_Button_delete", "Delete"),
          actionButton("GS_Button_recalculate", "Recalculate"),
          actionButton("GS_Button_save", "Save"),
          actionButton("GS_Button_cancel", "Cancel"),
          actionButton("GS_Button_summary", "Summary"),
          tags$style(HTML(type = "text/css", ".bootstrap-switch {margin-left: 20px; height: 35px; width: 200px}")),
          shinyWidgets::switchInput("GS_switch_plot", onLabel = "Upper", offLabel = "Lower",onStatus = "info",
                      offStatus = "info", value = T, inline = T, handleWidth = 99, labelWidth = 2),
          numericInput("gam_k_value", label = "k", value = 20, min = 2, max = 30, step = 1, width = "80px"),
          tags$hr()
        )
      } else {
        return(NULL)
      }
    })
    # GS done ----
    observeEvent(input$GS_Button_done,{
      shinyWidgets::updateMaterialSwitch(session, "lower_plot_switch", value = F)
      plot_GS$active = FALSE
    })
    GS_Button_save2 = reactive({list(input$GS_Button_done,input$GS_Button_save)})
    observeEvent(input$GS_Button_cancel,{
      shinyWidgets::updateMaterialSwitch(session, "lower_plot_switch", value = F)
      plot_GS$active = FALSE
      if(!is.null(d$c)){
        d$c = NULL
      }
    })
    observeEvent(GS_Button_save2(),{
      if(as.numeric(input$GS_Button_done)+as.numeric(input$GS_Button_save) == 0){
        NULL
      }else{
        shiny::withProgress(
          message = paste0("Processing..."),
          detail = "Comparing outputs",
          value = 0,
          {
            shiny::incProgress(1/10, detail = "Comparing outputs")
            if(nrow(na.omit(d$c["GS_start"]))>=1){
              df_gs = isolate(d$c) %>% select(.id, date_time, GS_start, GS_end)
              if(any(colnames(df) %in% c("GS_start", "GS_end"))){
                df = df %>% left_join(df_gs, by = c("date_time", ".id"))
                if(any(colnames(df) %in% "GS_start.x")) {
                  df[df$date_time %in% df_gs$date_time & df$.id %in% unique(df_gs$.id) ,"GS_start.x"] = NA
                  df = df %>% dplyr::mutate(GS_start = as.POSIXct(ifelse(is.na(GS_start.y), GS_start.x, GS_start.y),origin = '1970-01-01', tz = "UTC"))
                }
                if(any(colnames(df) %in% "GS_end.x")) {
                  df[df$date_time %in% df_gs$date_time & df$.id %in% unique(df_gs$.id) ,"GS_end.x"] = NA
                  df = df %>% dplyr::mutate(GS_end = as.POSIXct(ifelse(is.na(GS_end.y), GS_end.x, GS_end.y),origin = '1970-01-01', tz = "UTC"))
                }
                df = df %>% select(-tidyr::matches(c("GS.*\\.x", "GS.*\\.y")))
              }else{
                df = df %>% dplyr::left_join(df_gs, by = c(".id", "date_time"))
              }
              rm(df_gs)
              shiny::incProgress(5/10, detail = "Assigning to df")
              df = droplevels(df)
              assign('df', df, envir = envir)
              d$b <- df
            } else {
              NULL
            }
            shiny::incProgress(10/10, detail = "Saved")
          })
      }
    },ignoreNULL = FALSE, ignoreInit = TRUE)
    observeEvent(input$GS_Button_recalculate,{
      showModal(
        modalDialog(
        selectInput("GS_recalculate_method",
                    label = "Method:",
                    choices = c("fit_variable_rate","fit_model_rate")),
        numericInput("upper_gro_thr",
                    "Total growth threshold (%):",
                    value = 98, min = 0, max = 100, step = 1),
        numericInput("no_growth_thr",
                     HTML("No growth threshold (&mu;m):"),
                     value = 2, min = 0, max = 10, step = .5),
        selectInput("GS_Input_excl_month", "Ignore months:", selected = NULL, multiple = T, choices = c("1","2","3","4","5","6","7","8","9","10","11","12"), width = "200px"),
        shinyWidgets::materialSwitch("GS_remove_freeze", label = "Remove freeze days?", status = "info"),
        easyClose = T,
        footer = tagList(actionButton("GS_Button_recalculate_OK", "Recalculate", class = "btn-success"),
                         modalButton("Cancel")),
        size = "l"
      ))
    })

    observeEvent(input$GS_Button_recalculate_OK,{
      shiny::withProgress(
        message = paste0("Processing..."),
        detail = "Analysing GS",
        value = 0,
        {
        if(length(levels(d$a[[".id"]]))>0){
          if(isTRUE(input$GS_remove_freeze)){
            if("T1" %in% colnames(d$a)) {
              shiny::incProgress(2/10, detail = "Filtering freeze days")
              data_fit = d$a %>% dplyr::mutate(day_freeze = lubridate:: floor_date(date_time, "day")) %>%
                dplyr::mutate(Variable_freeze_orig = !!rlang::sym(input$variable_prim)) %>%
                dplyr::mutate(Variable_freeze = !!rlang::sym(input$variable_prim)) %>%
                group_by(.id, day_freeze) %>% mutate(Variable_freeze = ifelse(any(T1 < 0), NA, Variable_freeze)) %>%
                group_by(.id) %>% mutate(Variable_freeze = zoo::na.approx(Variable_freeze, na.rm = F)) %>%
                ungroup() %>%
                mutate(Variable_freeze = if_else(is.na(Variable_freeze_orig), NA, Variable_freeze)) %>%
                select(-Variable_freeze_orig, -input$variable_prim, -day_freeze) %>%
                plyr::rename(., c("Variable_freeze" = input$variable_prim)) %>%as.data.frame()
                # data_fit = freeze_show() %>% dplyr::select(.id, date_time, freeze_show2) %>% plyr::rename(., c("freeze_show2" = input$variable_prim))
            } else {
              shiny::incProgress(2/10, detail = "Data uploaded")
              data_fit = d$a
            }
          } else {
            shiny::incProgress(4/10, detail = "Data uploaded")
            data_fit = d$a
          }
          shiny::incProgress(2/10, detail = "Rates and model data")
          data_fit = data_fit %>% dplyr::mutate(date_time = floor_date(date_time, "day"), year = as.factor(year(date_time)), month = as.factor(month(date_time))) %>%
            select(.id, input$variable_prim, date_time, year, month) %>%
            dplyr::rename(Variable = input$variable_prim) %>%
            {if(!is.null(input$GS_Input_excl_month)){ dplyr::mutate(.data = ., Variable = ifelse(month %in% input$GS_Input_excl_month, NA, Variable))} else {.}} %>%
            dplyr::group_by(.id, year) %>%
            dplyr::filter(!all(is.na(Variable))) %>% droplevels() %>%
            group_by(.id, year, date_time) %>%
            dplyr::summarise_if(is.numeric, mean, na.rm = T) %>%
            as.data.frame()
          data_fit = data_fit %>% dplyr::group_by(.id, year) %>%
            mutate(Variable = Variable - first(na.omit(Variable))) %>%
            dplyr::mutate(Variable_rate = (Variable - dplyr::lag(Variable,14))/14) %>%
            droplevels() %>% dplyr::group_by(.id, year) %>% dplyr::mutate(rows = 1:n()) %>%
            as.data.frame()
          # finding GS start
          data_start = data_fit %>%
            # drop out variable rates lower than input$no_growth_thr microns per day since these are not considered as growth
            mutate(Variable_rate = if_else(Variable_rate < input$no_growth_thr, 0, Variable_rate)) %>%
            tidyr::drop_na(Variable_rate) %>% droplevels() %>%
            dplyr::group_by(.id, year) %>%
            # gam model with defined k value
            dplyr::mutate(model_rate = as.numeric(tryCatch(mgcv::predict.gam(mgcv::gam(formula = Variable_rate ~ s(rows, bs = "cs", k = input$gam_k_value), method = "REML")),error = function(e) return(0))))  %>%
            # detecting intersection with 0 within model rate or variable rate
            dplyr::mutate(inflect = c(diff(sign(model_rate)) != 0,FALSE), inflect_rad = c(diff(sign(Variable_rate)) != 0, FALSE)) %>%
            # marking data before or after maximum model rate
            # first quarter between min and max
            dplyr::mutate(beforeafter = lubridate::floor_date(min_max(date_time[Variable == min_max(Variable, "min")], "min")+((min_max(date_time[Variable == min_max(Variable, "max")], "max") - min_max(date_time[Variable == min_max(Variable, "min")], "min"))/2), "day")) %>%
            dplyr::mutate(model_rate = if_else(date_time <= beforeafter, model_rate, NA)) %>%
            dplyr::mutate(beforeafter = as.factor(ifelse(date_time <= date_time[which(model_rate == min_max(model_rate, method = "max"))],"Before", "After"))) %>%
            # dplyr::mutate(beforeafter = as.factor(ifelse(date_time <= date_time[which(Variable_rate == min_max(Variable_rate, method = "max"))] | beforeafter == as.character("Before"),"Before", "After"))) %>%
            # drop year where before is missing
            ungroup() %>%
            dplyr::filter(beforeafter == "Before") %>%
            droplevels() %>%
            dplyr::group_by(.id, year) %>%
            # marking data lower than 5% of maximum model rate
            dplyr::mutate(lows = if_else(model_rate <= 0.05 * min_max(model_rate, method = "max"), TRUE, FALSE)) %>%
            dplyr::mutate(mins = if_else(model_rate < 0.05 * min_max(model_rate, method = "max"),model_rate, NA)) %>%
            dplyr::mutate(mins = if_else(model_rate == min_max(mins, method = "min"), date_time, NA)) %>%
            dplyr::mutate(mins = if_else(mins == last(na.omit(mins)), mins, NA)) %>%
            # marking data lower than 5% of maximum variable rate
            dplyr::mutate(lows_rad = if_else(Variable_rate <= 0.05 * min_max(Variable_rate, method = "max"), TRUE, FALSE)) %>%
            dplyr::mutate(mins_rad = if_else(Variable_rate < 0.05 * min_max(Variable_rate, method = "max"),NA,Variable_rate)) %>%
            dplyr::mutate(mins_rad = if_else(Variable_rate == min_max(mins_rad, method = "min"), date_time, NA)) %>%
            dplyr::mutate(mins_rad = if_else(mins_rad == last(na.omit(mins_rad)), mins_rad, NA)) %>%
            # adding dates to GS corresponding with inflection point and lower than 5% threshold of growing rate
            # version with model rate
            dplyr::mutate(inflect = if_else(inflect == TRUE & lows == TRUE, date_time, NA)) %>%
            dplyr::mutate(inflect_rad = if_else(inflect_rad == TRUE & lows_rad == TRUE, date_time, NA)) %>%
            {if(input$GS_recalculate_method == "fit_model_rate"){
              # filtering to last day in case of before maximum growth dates
              dplyr::mutate(.data = ., GS_start = if_else(date_time == last(na.omit(inflect)), inflect, NA))
            }else{
              # filtering to first inflect_rad after the last inflect from model
              dplyr::mutate(.data = ., inflect_rad = if_else(date_time >= last(na.omit(inflect)), inflect_rad, NA)) %>%
                dplyr::mutate(GS_start = if_else(date_time == first(na.omit(inflect_rad)), inflect_rad, NA))
            }} %>%
            {if(input$GS_recalculate_method == "fit_model_rate"){
              dplyr::mutate(.data = ., GS_start = case_when(all(is.na(GS_start)) ~ mins,.default = GS_start))
            }else{
              dplyr::mutate(.data = ., GS_start = case_when(all(is.na(GS_start)) ~ mins_rad, .default = GS_start))
            }} %>%
            # guarantee to have a single date
            dplyr::mutate(GS_start = if_else(GS_start == min_max(GS_start), GS_start, NA))%>%
            dplyr::ungroup() %>%
            # dplyr::select(.id, date_time, GS_start) %>%
            as.data.frame()
          # detecting GS end
          data_fit = data_fit %>%
            # calculating GRO rate since already detected GS_start
            dplyr::left_join(data_start %>% select(.id, date_time, GS_start), by = c(".id", "date_time")) %>%
            group_by(.id, year) %>%
            # calculating GRO, GRO_rate and adding 95% growth threshold
            mutate(GRO = if_else(date_time<first(na.omit(GS_start)), NA, Variable)) %>% mutate(GRO = GRO - first(na.omit(GRO))) %>%
            mutate(Variable = if_else(date_time<first(na.omit(GS_start)), NA, Variable)) %>% mutate(Variable = Variable - first(na.omit(Variable))) %>%
            mutate(GRO = if_else(is.na(GRO) & date_time < first(na.omit(GS_start)), 0, if_else(is.na(GRO),NA ,cummax(if_else(is.na(GRO), -Inf, GRO))))) %>%
            mutate(GRO_rate = (GRO - dplyr::lag(GRO,14))/14) %>%
            mutate(GRO_rate = if_else(GRO_rate < input$no_growth_thr, 0, GRO_rate)) %>%
            mutate(GRO_upp_thr = (input$upper_gro_thr*0.01)*min_max(GRO, "max"))
          rm(data_start)
          data_fit = data_fit %>% tidyr::drop_na(GRO_rate) %>% droplevels() %>% dplyr::group_by(.id, year) %>%
            # gam model with default k value
            dplyr::mutate(model_rate = as.numeric(tryCatch(mgcv::predict.gam(mgcv::gam(formula = GRO_rate ~ s(rows, bs = "cs", k = 20), method = "REML")),error = function(e) return(0))))  %>%
            # detecting inflection points and intersections with 0 within model
            dplyr::mutate(inflect = ifelse(c(FALSE, diff(diff(model_rate)>0)>0, F), TRUE, FALSE)) %>%
            dplyr::mutate(inflect2 = c(diff(sign(model_rate)) != 0, FALSE))  %>%
            dplyr::mutate(inflect = if_else(inflect == TRUE, inflect, inflect2)) %>%
            # detecting intersections with 0 within variable rate
            dplyr::mutate(inflect_rad = c(FALSE, diff(sign(GRO_rate)) != 0))  %>%
            # marking data before or after model maximal rate
            dplyr::mutate(beforeafter = as.factor(ifelse(date_time <= date_time[which(model_rate == min_max(model_rate, method = "max"))],"Before", "After"))) %>%
            # drop year where before or after is missing
            dplyr::group_by(.id, year) %>%
            dplyr::filter(any("Before" %in% beforeafter & "After" %in% beforeafter)) %>% droplevels() %>%
            # marking data lower than 5% of maximum model rate
            dplyr::mutate(lows = if_else(model_rate <= 0.05 * min_max(model_rate, method = "max"), TRUE, FALSE)) %>%
            dplyr::mutate(lows_rad = if_else(GRO_rate <= 0.05 * min_max(GRO_rate, method = "max"), TRUE, FALSE)) %>%
            # and finding minimas
            # dplyr::group_by(beforeafter,.add = TRUE) %>%
            dplyr::mutate(mins = ifelse(model_rate < 0.05 * min_max(model_rate, method = "max"),model_rate, NA)) %>%
            dplyr::mutate(mins = if_else(model_rate == min_max(mins, method = "min"), TRUE, NA)) %>%
            dplyr::mutate(mins_rad = ifelse(GRO_rate < 0.05 * min_max(GRO_rate, method = "max"),GRO_rate, NA)) %>%
            dplyr::mutate(mins_rad = if_else(GRO_rate == min_max(mins_rad, method = "min"), TRUE, NA)) %>%
            dplyr::mutate(inflect = if_else((inflect == TRUE | mins == TRUE) & lows == TRUE, date_time, NA)) %>%
            dplyr::mutate(inflect_rad = if_else(inflect_rad == TRUE & lows_rad == TRUE, date_time, NA)) %>%
            dplyr::group_by(.id, year) %>%
            # detecting end
            # finding data over the upper growth threshold and marking the first one
            #moving GRO 14 days ahead to be comparable with lagged GRO_rate
            dplyr::mutate(inflect = if_else(lag(GRO,14) >= GRO_upp_thr, inflect, NA)) %>%
            dplyr::mutate(inflect_rad = if_else(lag(GRO,14) >= GRO_upp_thr, inflect_rad, NA)) %>%
            dplyr::mutate(inflect = if_else(beforeafter == "before", NA, if_else(date_time == first(na.omit(inflect)), inflect, NA))) %>%
            dplyr::mutate(inflect_rad = if_else(date_time <= first(na.omit(inflect)), inflect_rad, NA)) %>%
            {if(input$GS_recalculate_method == "fit_model_rate"){
              # filtering to first inflection point or intersection in a model above the upper growth threshold
              dplyr::mutate(.data = ., GS_end = if_else(date_time == first(na.omit(inflect)),inflect, NA))
            }else{
              # filtering to first inflect_rad after the last inflect from model
              dplyr::mutate(.data = ., GS_end = if_else(date_time == first(na.omit(inflect_rad)), inflect_rad, NA))
            }} %>%
            dplyr::mutate(GS_end = GS_end - lubridate::days(14))  %>%
            dplyr::mutate(GS_end = lead(GS_end, 14)) %>%
            # if no GSend detected move it to the upper growth threshold
            dplyr::group_by(.id, year) %>% mutate(GS_end = case_when(beforeafter == "Before" ~ GS_end,
                                                                     beforeafter == "After" & any(!is.na(GS_end[beforeafter == "After"])) ~ GS_end,
                                                                     GRO >= GRO_upp_thr & beforeafter == "After" & cumsum(GRO >= GRO_upp_thr) == 1 ~ date_time +days(1))) %>%
            dplyr::ungroup() %>%
            mutate(date_time = as.POSIXct(date_time, format = '%y-%m-%d %H:%M:%S', tz = "UTC")) %>%
            as.data.frame()
          # refit GS_start in case of model method fitting
          data_fit = data_fit %>% {if(input$GS_recalculate_method == "fit_model_rate"){
            dplyr::group_by(.data = ., .id, year) %>%
              # detecting intersection with 0 within model rate or variable rate
              dplyr::mutate(inflect = c(diff(sign(model_rate)) != 0,FALSE)) %>%
              dplyr::mutate(inflect = if_else(beforeafter == "After", NA, inflect)) %>%
              # marking data lower than 5% of maximum model rate
              dplyr::mutate(lows = if_else(model_rate <= 0.05 * min_max(model_rate, method = "max"), TRUE, FALSE)) %>%
              dplyr::mutate(mins = if_else(model_rate < 0.05 * min_max(model_rate, method = "max"),model_rate, NA)) %>%
              dplyr::mutate(mins = if_else(model_rate == min_max(mins, method = "min"), date_time, NA)) %>%
              dplyr::mutate(mins = if_else(mins == last(na.omit(mins)), mins, NA)) %>%
              # adding dates to GS corresponding with inflection point and lower than 5% threshold of growing rate
              # version with model rate
              dplyr::mutate(inflect = if_else(inflect == TRUE & lows == TRUE, date_time, NA)) %>%
              # filtering to last day in case of before maximum growth dates
              dplyr::mutate(GS_start = if_else(date_time == last(na.omit(inflect)), inflect, NA)) %>%
              dplyr::mutate(GS_start = case_when(all(is.na(GS_start)) ~ mins,.default = GS_start)) %>%
              # guarantee to have a single date
              dplyr::mutate(GS_start = if_else(GS_start == min_max(GS_start), GS_start, NA))%>%
              dplyr::ungroup() %>%
              # dplyr::select(.id, date_time, GS_start) %>%
              as.data.frame()}else{.}} %>%
            select(.id, date_time, , year, GS_start, GS_end, model_rate)
          # merging with original rates, trimming GRO and GRO rates and calculate TWD
          data_fit = d$a %>%
            select(where(is.factor), .id, input$variable_prim, date_time) %>%
            dplyr::mutate(date_time = floor_date(date_time, "day"), year = as.factor(year(date_time))) %>%
            dplyr::rename(Variable = input$variable_prim) %>%
            dplyr::group_by(.id, year) %>%
            dplyr::filter(!all(is.na(Variable))) %>% droplevels() %>%
            group_by(.id, year, date_time) %>%
            dplyr::summarise_if(is.numeric, mean, na.rm = T) %>%
            ungroup() %>%
            dplyr::left_join(data_fit, by = c(".id", "date_time", "year")) %>%
            group_by(.id, year) %>%
            dplyr::filter(!all(is.na(Variable))) %>% droplevels() %>%
            mutate(Variable = Variable - Variable[date_time == first(na.omit(GS_start))]) %>%
            mutate(Variable_rate = (Variable - dplyr::lag(Variable,14))/14) %>%
            mutate(GRO = if_else(date_time<first(na.omit(GS_start)), 0, Variable, missing = Variable)) %>%
            mutate(GRO = if_else(is.na(GRO), NA , cummax(if_else(is.na(GRO), -Inf, GRO)))) %>%
            mutate(GRO_rate = (GRO - dplyr::lag(GRO,14))/14) %>%
            mutate(GRO_rate = if_else(GRO_rate < input$no_growth_thr, 0, GRO_rate)) %>%
            mutate(TWD = if_else(is.na(Variable) | GRO <= 0, NA, Variable-GRO)) %>%
            as.data.frame()
          shiny::incProgress(2/10, detail = "Merging data")
          d$c = d$a %>%
            dplyr::select(-tidyr::matches("GS_")) %>%
            dplyr::group_by(.id) %>%
            dplyr::filter(lubridate::hour(date_time) == 00 & lubridate::minute(date_time) == 00) %>%
            ungroup() %>%
            dplyr::mutate(year = as.factor(lubridate::year(date_time))) %>%
            dplyr::select(-any_of(setdiff(colnames(data_fit), c(".id", "date_time", "year")))) %>%
            dplyr::left_join(data_fit, by = c(".id", "date_time", "year")) %>%
            as.data.frame()
          rm(data_fit)
          removeModal()
          shiny::incProgress(2/10, detail = "Done")
          } else {
          NULL
        }
      })
    })
    observeEvent(input$GS_Button_move,{
      if(isFALSE(input$one_by_one_switch)){
        r = row.names(brushedPoints(isolate(d$c)[complete.cases(isolate(d$c)[,input$variable_prim]) & isolate(d$c)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
      }else{
        r = row.names(brushedPoints(isolate(d$c)[complete.cases(isolate(d$c)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
      }
      df3 = isolate(df[r,])
      df3 = droplevels(df3)
      if(is.null(input$RadiusPlot_brush)|length(levels(df3$.id))!= 1){
        showModal(modalDialog(
          title = "GS manual edit!",
          HTML('Brush data first!<br>
               Brush data from only one device!'),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }else{
        showModal(modalDialog(
          title = "GS manual edit!",
          shinyWidgets::switchInput("GS_start_end",onLabel = "Start",
                      offLabel =  "End",labelWidth = 1, offStatus = "danger",
                      value = T),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok_bar16", "OK")
          )
        ))
      }
    })
    observeEvent(input$ok_bar16,{
      if(isFALSE(input$one_by_one_switch)){
        data_fit2 = brushedPoints(isolate(d$c)[isolate(d$c)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time")
        data_fit2 =  data_fit2 %>% mutate(rnames = row.names(.)) %>% filter(!is.na(Variable)) %>% slice(1) %>% droplevels()
      }else{
        data_fit2 = brushedPoints(isolate(d$c), input$RadiusPlot_brush, xvar = "date_time")
        data_fit2 = data_fit2 %>% mutate(rnames = row.names(.)) %>% filter(!is.na(Variable)) %>% slice(1) %>% droplevels()
      }
      if(nrow(data_fit2) < 1 | all(is.na(data_fit2$Variable))){
        removeModal()
        showModal(modalDialog(
          title = NULL,
          HTML("No data selected!"),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }else{
        if(input$GS_start_end){
          d$c[d$c[["year"]] %in% levels(data_fit2$year) & d$c[[".id"]] %in% levels(data_fit2$.id), "GS_start"] = NA
          d$c[data_fit2$rnames, "GS_start"] = data_fit2$date_time
        }else{
          if(isTRUE(input$GS_switch_plot)){
            d$c[d$c[["year"]] %in% levels(data_fit2$year) & d$c[[".id"]] %in% levels(data_fit2$.id), "GS_end"] = NA
            d$c[data_fit2$rnames, "GS_end"] = data_fit2$date_time
          }else{
            d$c[d$c[["year"]] %in% levels(data_fit2$year) & d$c[[".id"]] %in% levels(data_fit2$.id), "GS_end"] = NA
            d$c[as.character(as.numeric(data_fit2$rnames)-14), "GS_end"] = data_fit2$date_time-lubridate::days(14)
          }
        }
        GS_data_frame_refit = d$c[d$c[["year"]] %in% levels(data_fit2$year) & d$c[[".id"]] %in% levels(data_fit2$.id),] %>%
          select(.id, date_time, Variable, year, GS_start) %>%
          droplevels() %>%
          dplyr::group_by(.id, year) %>%
          dplyr::filter(!all(is.na(Variable))) %>% droplevels() %>%
          mutate(GRO = if_else(date_time<first(na.omit(GS_start)), NA, Variable)) %>% mutate(GRO = GRO - first(na.omit(GRO))) %>%
          mutate(GRO = if_else(is.na(GRO) & date_time < first(na.omit(GS_start)), 0, if_else(is.na(GRO),NA ,cummax(if_else(is.na(GRO), -Inf, GRO))))) %>%
          mutate(GRO_rate = (GRO - dplyr::lag(GRO,14))/14) %>%
          mutate(GRO_rate = if_else(GRO_rate < 2, 0, GRO_rate))
        model_fit = GS_data_frame_refit %>% select(.id, date_time, year, GRO_rate) %>% group_by(.id, year) %>% dplyr::mutate(rows = 1:n()) %>%
          tidyr::drop_na(GRO_rate) %>% droplevels() %>% dplyr::mutate(model_rate = as.numeric(tryCatch(mgcv::predict.gam(mgcv::gam(formula = GRO_rate ~ s(rows, bs = "cs", k = 20), method = "REML")),error = function(e) return(0))))
        GS_data_frame_refit = GS_data_frame_refit %>% left_join(model_fit %>% select(-rows, -GRO_rate), by = c(".id", "date_time", "year")) %>% select(-any_of(c("GS_start", "GS_end"))) %>% as.data.frame()
        d$c = d$c %>% left_join(GS_data_frame_refit, by = c(".id", "date_time", "year")) %>%
          mutate(across(ends_with(".y"),~ ifelse(!is.na(.), get(sub(".x", ".y", cur_column())), get(sub(".y", ".x", cur_column()))),.names = "{.col}")) %>% select(-ends_with(".x")) %>% dplyr::rename_all(~sub(".y", "", .x, fixed=T))
        rm(data_fit2, GS_data_frame_refit)
      }
      removeModal()
    })
    observeEvent(input$GS_Button_delete,{
      if(isFALSE(input$one_by_one_switch)){
        r = row.names(brushedPoints(isolate(d$c)[complete.cases(isolate(d$c)[,input$variable_prim]) & isolate(d$c)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
      }else{
        r = row.names(brushedPoints(isolate(d$c)[complete.cases(isolate(d$c)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
      }
      df3 = isolate(df[r,])
      df3 = droplevels(df3)
      if(is.null(input$RadiusPlot_brush)|length(levels(df3$.id))!= 1){
        showModal(modalDialog(
          title = "GS manual edit!",
          HTML('Brush data first!<br>
               Brush data from only one device!'),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }else{
        if(isFALSE(input$one_by_one_switch)){
          data_fit2 = brushedPoints(isolate(d$c)[isolate(d$c)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time")
          data_fit2 =  data_fit2 %>% mutate(rnames = row.names(.)) %>% filter(!is.na(Variable_rate)) %>% droplevels()
        }else{
          data_fit2 = brushedPoints(isolate(d$c), input$RadiusPlot_brush, xvar = "date_time")
          data_fit2 = data_fit2 %>% mutate(rnames = row.names(.)) %>% filter(!is.na(Variable_rate)) %>% droplevels()
        }
          d$c[data_fit2$rnames, "GS_start"] = NA
          if(isTRUE(input$GS_switch_plot)){
            d$c[data_fit2$rnames, "GS_end"] = NA
          }else{
            d$c[as.character(as.numeric(data_fit2$rnames)-14), "GS_end"] = NA
          }
      }
      rm(df3, data_fit2)
    })
# GS_summary_save_names = reactiveValues(name = 0)
    observeEvent(input$GS_Button_summary,{
      shiny::withProgress(
        message = paste0("Processing..."),
        detail = "GS summary",
        value = 0,
        {
          shiny::incProgress(2/10, detail = "rate summary")
          GS_summary_rate = isolate(d$c) %>% group_by(.id, year) %>% filter(date_time >=  if_else(all(is.na(GS_start)), first(na.omit(date_time)), first(na.omit(GS_start)))  & date_time <= if_else(all(is.na(GS_end)), last(na.omit(date_time)), last(na.omit(GS_end)))) %>% dplyr::filter(Variable_rate == min_max(Variable_rate, "max")) %>% dplyr::distinct(Variable_rate, .keep_all = T) %>% dplyr::rename(Max_rate_date = date_time, Max_variable_rate = Variable_rate) %>% dplyr::select(.id, year, Max_rate_date, Max_variable_rate)
          GS_summary_twd = isolate(d$c) %>% group_by(.id, year) %>% filter(date_time >= first(na.omit(GS_start))  & date_time <= last(na.omit(GS_end))) %>% dplyr::filter(TWD == min_max(TWD, "min")) %>% dplyr::distinct(TWD, .keep_all = T) %>% dplyr::rename(TWD_min_date = date_time, TWD_min = TWD) %>% dplyr::select(.id, year, TWD_min_date, TWD_min)
          shiny::incProgress(2/10, detail = "model rate summary")
          GS_summary_rate_model = isolate(d$c) %>% group_by(.id, year) %>% filter(date_time >=  if_else(all(is.na(GS_start)), first(na.omit(date_time)), first(na.omit(GS_start)))  & date_time <= if_else(all(is.na(GS_end)), last(na.omit(date_time)), last(na.omit(GS_end)))) %>% dplyr::filter(model_rate == min_max(model_rate, "max")) %>% dplyr::distinct(model_rate, .keep_all = T) %>% dplyr::rename(Max_model_rate_date = date_time, Max_model_rate = model_rate) %>% dplyr::select(.id, year, Max_model_rate_date, Max_model_rate)
          shiny::incProgress(2/10, detail = "completing GS summary")
          GS_summary = isolate(d$c) %>% group_by(.id, year) %>% dplyr::summarise(GS_start = min_max(GS_start), GS_end  = min_max(GS_end, "max"), GRO_tot_full = if_else(all(is.na(GS_start))|all(is.na(GS_end)), NA, min_max(GRO, "max")-min_max(GRO))) %>% mutate(GS_start_doy = yday(GS_start),GS_end_doy = yday(GS_end)) %>% mutate(GS_length = GS_end_doy-GS_start_doy)
          GS_summary_GS = isolate(d$c) %>% group_by(.id, year) %>% filter(date_time >= first(na.omit(GS_start)) & date_time <= last(na.omit(GS_end))) %>% dplyr::summarise(GRO_tot = min_max(GRO, "max")-min_max(GRO), TWD_tot = if_else(all(is.na(TWD)), NA, sum(na.omit(TWD))))
          GS_summary = GS_summary %>% dplyr::ungroup() %>% dplyr::left_join(GS_summary_twd, by = c(".id", "year")) %>% dplyr::left_join(GS_summary_GS, by = c(".id", "year")) %>% dplyr::left_join(GS_summary_rate, by = c(".id", "year")) %>% dplyr::left_join(GS_summary_rate_model, by = c(".id", "year")) %>% droplevels() %>% relocate(.id, year, GS_start, GS_end, GS_start_doy, GS_end_doy, GS_length, GRO_tot, GRO_tot_full, TWD_tot, TWD_min, TWD_min_date, Max_variable_rate, Max_rate_date, Max_model_rate, Max_model_rate_date) %>% as.data.frame()
          rm(GS_summary_rate, GS_summary_rate_model, GS_summary_GS)
          GS_df_rates = isolate(d$c) %>% dplyr::select(.id, date_time, year, Variable_rate, model_rate, Variable, GRO, TWD, GRO_rate) %>% droplevels() %>% as.data.frame()
          GS_add_GS = GS_summary %>% dplyr::select(.id, year, GS_start, GS_end)
          GS_summary_perc = GS_df_rates %>%
            dplyr::left_join(GS_add_GS, by = c(".id", "year")) %>%
            group_by(.id, year) %>%
            dplyr::filter(date_time >= dplyr::first(na.omit(GS_start)) & date_time <= dplyr::last(na.omit(GS_end))) %>%
            dplyr::mutate(GRO_25_date = (min_max(GRO, "max") - min_max(GRO, "min")) * 0.25,
                   GRO_50_date = (min_max(GRO, "max") - min_max(GRO, "min")) * 0.50,
                   GRO_75_date = (min_max(GRO, "max") - min_max(GRO, "min")) * 0.75,
                   GRO_90_date = (min_max(GRO, "max") - min_max(GRO, "min")) * 0.90,
                   GRO_95_date = (min_max(GRO, "max") - min_max(GRO, "min")) * 0.95) %>%
            dplyr::mutate(GRO_25_date = ifelse(GRO < GRO_25_date, NA, GRO_25_date),
                   GRO_50_date = ifelse(GRO < GRO_50_date, NA, GRO_50_date),
                   GRO_75_date = ifelse(GRO < GRO_75_date, NA, GRO_75_date),
                   GRO_90_date = ifelse(GRO < GRO_90_date, NA, GRO_90_date),
                   GRO_95_date = ifelse(GRO < GRO_95_date, NA, GRO_95_date)) %>%
            dplyr::mutate(GRO_25_date = if_else(is.na(GRO_25_date), NA, date_time),
                   GRO_50_date = if_else(is.na(GRO_50_date), NA, date_time),
                   GRO_75_date = if_else(is.na(GRO_75_date), NA, date_time),
                   GRO_90_date = if_else(is.na(GRO_90_date), NA, date_time),
                   GRO_95_date = if_else(is.na(GRO_95_date), NA, date_time)) %>%
            dplyr::summarise(GRO_25_date = first(na.omit(GRO_25_date)),
                      GRO_50_date = first(na.omit(GRO_50_date)),
                      GRO_75_date = first(na.omit(GRO_75_date)),
                      GRO_90_date = first(na.omit(GRO_90_date)),
                      GRO_95_date = first(na.omit(GRO_95_date))) %>%
            dplyr::mutate(GRO_25_doy = lubridate::yday(GRO_25_date),
                   GRO_50_doy = lubridate::yday(GRO_50_date),
                   GRO_75_doy = lubridate::yday(GRO_75_date),
                   GRO_90_doy = lubridate::yday(GRO_90_date),
                   GRO_95_doy = lubridate::yday(GRO_95_date)) %>% as.data.frame()
          GS_summary = GS_summary %>% left_join(GS_summary_perc, by = c(".id", "year")) %>% as.data.frame()
          rm(GS_summary_perc,GS_add_GS)
          shiny::incProgress(2/10, detail = "adding metadata")
          GS_meta = isolate(d$c) %>% dplyr::select_if(is.factor) %>% dplyr::distinct(.id, year, .keep_all = T)
          GS_meta_date = isolate(d$c) %>% dplyr::select(where(is.factor), date_time) %>% dplyr::distinct(.keep_all = T)
          GS_df_rates = GS_df_rates %>% left_join(GS_meta_date, by = c(".id", "date_time", "year")) %>% as.data.frame()
          GS_summary = GS_summary %>% left_join(GS_meta, by = c(".id", "year")) %>% as.data.frame()
          rm(GS_meta, GS_meta_date)
          shiny::incProgress(2/10, detail = "assigning data")
          if("GS_summary" %in% ls(envir = envir)){
            index = 1
            while(paste0("GS_summary_", index) %in% ls(envir = envir)) {
              index = index+1
            }
            GS_summary_name = paste0("GS_summary_",index)
            # GS_summary_save_names$name = index
          }else{
            GS_summary_name = paste0("GS_summary")
          }
          if("GS_df_rates" %in% ls(envir = envir)){
            index = 1
            while(paste0("GS_df_rates_", index) %in% ls(envir = envir)) {
              index = index+1
            }
            GS_df_rates_name = paste0("GS_df_rates_",index)
          }else{
            GS_df_rates_name = paste0("GS_df_rates")
          }
          assign(x = paste0(GS_summary_name), value = GS_summary, envir = envir)
          assign(x = paste0(GS_df_rates_name), value = GS_df_rates, envir = envir)
          rm(GS_summary,GS_summary_name, GS_df_rates, GS_df_rates_name)
          shiny::incProgress(2/10, detail = "GS summary assigned to environment")
        })
    })
    observe({
      if(isFALSE(input$lower_plot_switch)){
        plot_GS$active = FALSE
      }
    })
    #date_time slider output
    output$range <- renderPrint({ input$slider2 })
    # tables ----
    output$table <- renderDataTable({
      input$goButton
      isolate(as.data.frame(d$a))
    }, selection = "multi")

    #generate an HTML table of brushed points
    output$plot_brushed_points <- renderDataTable({
      if(is.null(sele_prim())){
        datatable(NULL)
      } else {
        if(isFALSE(input$one_by_one_switch)){
          res <- brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time")
          datatable(res)
        } else {
          res <- brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time")
          datatable(res)
        }
      }
    })
    #plots_functions ----
    #inputs for exported plots
    plot_export_funct = function(){
      if(isTRUE(input$legend_switch)){
        if(isTRUE(input$lower_plot_switch)){
          grid.arrange(legend_appear_2(),plot_appear_2(), plot_appear_3(), ncol = 1, heights = c(.4 * legend_rows(),2,2))
        } else {
          #plot_zooming()
          grid.arrange(legend_appear_2(), plot_appear_2(), ncol = 1, heights = c(.4 * legend_rows(),2))
        }
      }else{
        if(isTRUE(input$lower_plot_switch)){
          grid.arrange(plot_appear_2(), plot_appear_3(), ncol = 1, heights = c(2,2))
        } else {
          #plot_zooming()
          plot_appear_2()
        }
      }
    }
    height_funct = function(){
      if(isTRUE(input$lower_plot_switch)){
        30
      } else {
        15
      }
    }
    #zooming
    zooming <- reactiveValues(x = NULL, y = NULL)
    observeEvent(input$RadiusPlot_dblclick, {
      brush <- input$RadiusPlot_brush
      if (!is.null(brush)) {
        if("POSIXct" %in% class(df$date_time)){
          zooming$x <- as.POSIXct(c(brush$xmin, brush$xmax),origin = "1970-01-01 00:00:00 UTC")
          zooming$y <- c(brush$ymin, brush$ymax)
        }else{if("Date" %in% class(df$date_time)){
          zooming$x <- as.Date(c(brush$xmin, brush$xmax),origin = "1970-01-01 00:00:00 UTC")
          zooming$y <- c(brush$ymin, brush$ymax)
        }else{
          zooming$x <- NULL
          zooming$y <- NULL
        }}
      } else {
        zooming$x <- NULL
        zooming$y <- NULL
      }
    })
    # numeric input to manually change legend rows
    output$legend_val = renderUI({
      if(input$one_by_one_switch){
        tags$div(class="inline",numericInput("legend_value", "legend rows:", value = legend_rows(), min   = 1, max = max(legend_rows() * 3, 30), step  = 1,width = "8%"))
      } else {
        tags$div(class="inline",numericInput("legend_value", "legend rows: ", min = 1, max = 30, step = 1, width = "8%", value = 1))
      }
    })
    observeEvent(legend_rows(), {
      updateNumericInput(session, "legend_value", value = legend_rows())
    }, ignoreInit = TRUE)
    #plot functions to Input Data tab
    ploter_logo = png::readPNG(system.file("logos/ploter_logo2.png", package = "PLOTeR"))
    empty_plot_1 = function() {
      ggplot(data = d$a, aes(x=!!rlang::sym("date_time"), y=NULL))+
        theme_bw()+
        annotation_raster(ploter_logo, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    }
    empty_plot_2 = function() {
      ggplot(data = d$a, aes(x=!!rlang::sym("date_time"), y=NULL))+
        theme_bw()
    }
    legend_2 = function() {
      ggplot(data = d$a, aes(x=!!rlang::sym("date_time"), y=NULL))
      theme_bw()
    }

    legend_rows <- reactive({
      dat <- plot_plot_data()
      req(nrow(dat) > 0)
      grp_col <- if (isFALSE(input$group_switch_upper_plot) || input$Groupby_upper_plot == "none") {
        ".id"
      } else {
        input$Groupby_upper_plot
      }
      if (!grp_col %in% names(dat)) return(1L)
      n_groups <- nlevels(droplevels(factor(dat[[grp_col]])))
      max(1L, ceiling(n_groups/18L))
    }) %>% debounce(1000)
    plot_data_plot = function() {
      c = d$a
      sel = input$table_rows_selected
      dat3 = c[sel,]
      if(isFALSE(input$group_switch_upper_plot)){rows = ceiling(length(levels(droplevels(c$.id)))/18)}
      else{
        if(input$Groupby_upper_plot == "none"){
          rows = 1
        }else{
          rows = ceiling(length(levels(droplevels(c[[input$Groupby_upper_plot]])))/18)
        }
      }
      ggplot(data = c, aes(x=!!rlang::sym("date_time"), y=!!rlang::sym(input$variable_prim), colour=!!rlang::sym(".id")))+
        geom_line()+
        geom_point(data = dat3, fill = "black",size = 3, stroke = 1,alpha= 0.6, show.legend = F)+
        theme_bw()+
        guides(colour = guide_legend(title = NULL, direction = "vertical", byrow = T, nrow = rows))+
        theme(legend.position = "top")
    }
    plot_plot_data = reactive({
      req(!is.null(d$a))
      if(isFALSE(input$one_by_one_switch)){
        c = d$a
        c = c %>% filter(.id %in% input$one_by_one_group_select)
      } else {
        c = d$a
      }
      return(c)
    })
    # repeated invalidations with identical keys don’t rebuild the plot
    upper_plot_obj <- reactive({
      plot_upper_plot()
<<<<<<< HEAD
    }) %>%
      bindEvent(
        input$navbar,
        input$one_by_one_group_select,
        ignoreInit = T
      )
=======
    })
    # %>%
    #   bindEvent(
    #     input$navbar,
    #     input$variable_prim,
    #     input$upper_plot_interval_input,
    #     input$group_switch_upper_plot,
    #     input$Groupby_upper_plot,
    #     input$method_upper_plot,
    #     input$se_switch_upper_plot,
    #     input$one_by_one_switch,
    #     input$one_by_one_group_select,
    #     input$legend_value,
    #     zooming$x, zooming$y,
    #     ignoreInit = T
    #   )
>>>>>>> bd2ffb53c949f6fda25ebf7b7016917edb059126
    plot_upper_plot = function(){
        rows <- if (!is.null(input$legend_value)) input$legend_value else legend_rows()
        data_upper_plot <- plot_plot_data() %>% {
          if (input$upper_plot_interval_input == "original") {
            .
          } else if (input$upper_plot_interval_input == "min") {
            dplyr::group_by(., .id) %>%
              dplyr::filter(lubridate::second(date_time) == 0)
          } else if (input$upper_plot_interval_input == "hour") {
            dplyr::group_by(., .id) %>%
              dplyr::filter(lubridate::minute(date_time) == 0)
          } else if (input$upper_plot_interval_input == "day") {
            dplyr::group_by(., .id) %>%
              dplyr::filter(lubridate::hour(date_time) == 0 & lubridate::minute(date_time) == 0)
          } else {
            .
          }
        }
        if(input$method_upper_plot == "mean") {
          {if(input$Groupby_upper_plot == "none"){ggplot(data = data_upper_plot %>%
                                                           group_by(date_time) %>%
                                                           summarise(data_mean = mean(!!rlang::sym(input$variable_prim), na.rm = T), data_se = sd(!!rlang::sym(input$variable_prim), na.rm = T)/sqrt(sum(!is.na(!!rlang::sym(input$variable_prim))))) %>%
                                                           as.data.frame(),
                                                         aes(x=!!rlang::sym("date_time"), y=!!rlang::sym("data_mean")))+
              geom_line(colour = "#3366FF")+
              {if(isTRUE(input$se_switch_upper_plot)){
                geom_ribbon(aes(ymin = data_mean-data_se, ymax = data_mean+data_se), colour = NA, fill = "#3366FF", alpha = .2)}}
          }else{ggplot(data = data_upper_plot %>%
                         group_by(!!rlang::sym(input$Groupby_upper_plot),date_time) %>%
                         summarise(data_mean = mean(!!rlang::sym(input$variable_prim), na.rm = T), data_se = sd(!!rlang::sym(input$variable_prim), na.rm = T)/sqrt(sum(!is.na(!!rlang::sym(input$variable_prim))))) %>%
                         as.data.frame(),
                       aes(x=!!rlang::sym("date_time"), y=!!rlang::sym("data_mean"),colour = !!rlang::sym(input$Groupby_upper_plot), fill = !!rlang::sym(input$Groupby_upper_plot)))+
              geom_line()+
              {if(isTRUE(input$se_switch_upper_plot)){
                geom_ribbon(aes(ymin = data_mean-data_se, ymax = data_mean+data_se), colour = NA, alpha = .2)}}
          }}+
            coord_cartesian(xlim = zooming$x,  ylim = zooming$y, expand = T)+
            theme_bw()+
            guides(shape = guide_legend(title = NULL, direction = "vertical", byrow = T, nrow = rows))+
            theme(legend.position = "top")+
            ylab(label = input$variable_prim)
        } else {
          ggplot(data = data_upper_plot, aes(x=!!rlang::sym("date_time"), y=!!rlang::sym(input$variable_prim)))+
            {if(isFALSE(input$group_switch_upper_plot)){geom_line(aes(colour=!!rlang::sym(".id")))
            }else {
              {if(input$Groupby_upper_plot == "none"){geom_smooth(method = input$method_upper_plot, se=input$se_switch_upper_plot)}
                else{geom_smooth(aes(colour = !!rlang::sym(input$Groupby_upper_plot)),method = input$method_upper_plot, se=input$se_switch_upper_plot)}}}}+
            coord_cartesian(xlim = zooming$x,  ylim = zooming$y, expand = T)+
            theme_bw()+
            guides(colour = guide_legend(title = NULL, direction = "vertical", byrow = T, nrow = rows))+
            theme(legend.position = "top")}
      }
    plot_lower_plot = function() {
      data_lower_plot <- plot_plot_data() %>% {
        if (input$lower_plot_interval_input == "original") {
          .
        } else if (input$lower_plot_interval_input == "min") {
          dplyr::group_by(., .id) %>%
            dplyr::filter(lubridate::second(date_time) == 0)
        } else if (input$lower_plot_interval_input == "hour") {
          dplyr::group_by(., .id) %>%
            dplyr::filter(lubridate::minute(date_time) == 0)
        } else if (input$lower_plot_interval_input == "day") {
          dplyr::group_by(., .id) %>%
            dplyr::filter(lubridate::hour(date_time) == 0 & lubridate::minute(date_time) == 0)
        } else {
          .
        }
      }
      if(input$method_lower_plot == "mean") {
        {if(input$Groupby_lower_plot == "none"){ggplot(data = data_lower_plot %>%
                       group_by(date_time) %>%
                       summarise(data_mean = mean(!!rlang::sym(input$variable_prim_lower), na.rm = T), data_se = sd(!!rlang::sym(input$variable_prim_lower), na.rm = T)/sqrt(sum(!is.na(!!rlang::sym(input$variable_prim_lower))))) %>%
                       as.data.frame(),
                     aes(x=!!rlang::sym("date_time"), y=!!rlang::sym("data_mean")))+
            geom_line(colour = "#3366FF")+
            {if(isTRUE(input$se_switch_lower_plot)){
              geom_ribbon(aes(ymin = data_mean-data_se, ymax = data_mean+data_se), colour = NA, fill = "#3366FF", alpha = .2)}}
            }else{ggplot(data = data_lower_plot %>%
                        group_by(!!rlang::sym(input$Groupby_lower_plot),date_time) %>%
                        summarise(data_mean = mean(!!rlang::sym(input$variable_prim_lower), na.rm = T), data_se = sd(!!rlang::sym(input$variable_prim_lower), na.rm = T)/sqrt(sum(!is.na(!!rlang::sym(input$variable_prim_lower))))) %>%
                        as.data.frame(),
                      aes(x=!!rlang::sym("date_time"), y=!!rlang::sym("data_mean"),colour = !!rlang::sym(input$Groupby_lower_plot), fill = !!rlang::sym(input$Groupby_lower_plot)))+
                geom_line()+
                {if(isTRUE(input$se_switch_lower_plot)){
                  geom_ribbon(aes(ymin = data_mean-data_se, ymax = data_mean+data_se), colour = NA, alpha = .2)}}
                }}+
          coord_cartesian(xlim = zooming$x,  ylim = zooming$y, expand = T)+
          theme_bw()+
          # guides(shape = guide_legend(title = NULL, direction = "vertical", byrow = T, nrow = rows))+
          theme(legend.position = "none")+
          ylab(label = input$variable_prim_lower)
      } else {
        ggplot(data = data_lower_plot, aes(x=!!rlang::sym("date_time"), y=!!rlang::sym(input$variable_prim_lower)))+
          {if(isFALSE(input$group_switch_lower_plot)){geom_line(aes(colour=!!rlang::sym(".id")))
          }else {
            {if(input$Groupby_lower_plot == "none"){geom_smooth(method = input$method_lower_plot, se=input$se_switch_lower_plot)}
              else{geom_smooth(aes(colour = !!rlang::sym(input$Groupby_lower_plot)),method = input$method_lower_plot, se=input$se_switch_lower_plot)}}}}+
          #geom_point(data = dat3, fill = "black",size = 3, stroke = 1,alpha= 0.6, show.legend = F)+
          coord_cartesian(xlim = zooming$x,  ylim = zooming$y, expand = T)+
          theme_bw()+
          # guides(colour = guide_legend(title = NULL, direction = "vertical", byrow = T, nrow = rows))+
          theme(legend.position = "none")}
    }
    plot_sec_axis_1 = function() {
      recal = reactiveValues(avar = (max(d$a[[input$variable_sec]],na.rm = T)-min(d$a[[input$variable_sec]],na.rm = T))/(max(d$a[[input$variable_prim]],na.rm = T)-min(d$a[[input$variable_prim]],na.rm = T)))
      recal2 = reactiveValues(bvar = min(d$a[[input$variable_sec]],na.rm = T)-(min(d$a[[input$variable_prim]],na.rm = T)*recal$avar))
      # e = reactive({
      #   cbind(d$a,  sec = (d$a[[input$variable_sec]]-recal2$bvar)/recal$avar,
      #         .id_sec = ifelse(is.na(d$a[[input$variable_sec]]), paste(d$a[['.id']]),paste(substring(input$variable_sec,1,3), d$a[['.id']], sep = '_')))
      # })
      dat2 = d$a %>% mutate(sec = (d$a[[input$variable_sec]]-recal2$bvar)/recal$avar, .id_sec = ifelse(is.na(d$a[[input$variable_sec]]), paste(d$a[['.id']]),paste(substring(input$variable_sec,1,3), d$a[['.id']], sep = '_'))) %>% as.data.frame()
      #plot_sec_axis
      sel = input$table_rows_selected
      # dat2 = e()
      # dat3 = dat2[sel,]
      rows = ceiling(length(levels(droplevels(dat2$.id)))/6)
      ggplot(data = dat2, aes(x=!!rlang::sym("date_time"), y=!!rlang::sym(input$variable_prim), colour=!!rlang::sym(".id")))+
        geom_line()+
        # geom_point(data = dat3,fill = "black", size = 3, alpha= 0.6, show.legend = F)+
        geom_line(aes(x=!!rlang::sym("date_time"), y=!!rlang::sym("sec"), colour=!!rlang::sym(".id_sec")))+
        scale_y_continuous(name = input$variable_prim,sec.axis = sec_axis(~.*recal$avar+recal2$bvar, name = input$variable_sec))+
        theme_bw()+
        guides(colour = guide_legend(title = NULL, direction = "vertical", byrow = T, nrow = rows))+
        theme(legend.position = "top")
    }
    plot_sec_axis_2 = function() {
      if(isFALSE(input$one_by_one_switch)){
        dat2 = d$a
        dat2 = dat2 %>% filter(.id %in% input$one_by_one_group_select) %>% droplevels()
      } else {
        dat2 = d$a
      }
      if(input$upper_plot_interval_input == "min" | input$upper_plot_interval_input == "original" ){
      } else {
        if(input$upper_plot_interval_input == "hour"){
          dat2 = as.data.frame(dat2 %>% dplyr::group_by(.id) %>% dplyr::filter(minute(date_time) == 00))
        } else {
          if(input$upper_plot_interval_input == "day"){
            dat2 = as.data.frame(dat2 %>% dplyr::group_by(.id) %>% dplyr::filter(hour(date_time) == 00 & minute(date_time) == 00))
          }
        }
      }
      if(isTRUE(input$regre_switch) & !is.na(input$variable_sec)){
        dat2 = as.data.frame(dat2 %>% dplyr::group_by(.id) %>% dplyr::filter(lubridate::hour(date_time) == 00 & lubridate::minute(date_time) == 00))
        groups = c("date_time", input$Groupby_upper_plot)
        dat2 = dat2 %>% dplyr::group_by(across(groups)) %>% dplyr::summarise(meanx = mean(!!rlang::sym(input$variable_prim), na.rm = T), meany = mean(!!rlang::sym(input$variable_sec), na.rm = T)) %>% na.omit() %>%
          plyr::rename(c("meanx" = input$variable_prim, "meany" = input$variable_sec)) %>% as.data.frame()
      }else{
      if(isTRUE(input$freeze_switch & input$variable_prim == "Radius")){
        dat2 = dat2 %>% dplyr::rename(Raw = freeze_show2, Fine = freeze_show)
        recal = reactiveValues(avar = (max(dat2[[input$variable_prim]],na.rm = T)-min(dat2[[input$variable_prim]],na.rm = T))/(max(dat2[[input$variable_prim]],na.rm = T)-min(dat2[[input$variable_prim]],na.rm = T)))
        recal2 = reactiveValues(bvar = min(dat2[[input$variable_prim]],na.rm = T)-(min(dat2[[input$variable_prim]],na.rm = T)*recal$avar))
        # e = reactive({
        #   cbind(c,  sec = (c[[input$method3]]-recal2$bvar)/recal$avar,
        #         .id_sec = ifelse(is.na(c[[input$method3]]), paste(c[['.id']]),paste(substring(input$method3,1,3), c[['.id']], sep = '_')))
      # })
        dat2 = dat2 %>% mutate(sec = (dat2[[input$method3]]-recal2$bvar)/recal$avar, .id_sec = ifelse(is.na(dat2[[input$variable_sec]]), paste(dat2[['.id']]),paste(substring(input$method3,1,3), dat2[['.id']], sep = '_'))) %>% as.data.frame()
      } else {
        recal = reactiveValues(avar = (max(dat2[[input$variable_sec]],na.rm = T)-min(dat2[[input$variable_sec]],na.rm = T))/(max(dat2[[input$variable_prim]],na.rm = T)-min(dat2[[input$variable_prim]],na.rm = T)))
        recal2 = reactiveValues(bvar = min(dat2[[input$variable_sec]],na.rm = T)-(min(dat2[[input$variable_prim]],na.rm = T)*recal$avar))
        # e = reactive({
        #   cbind(c,  sec = (c[[input$variable_sec]]-recal2$bvar)/recal$avar,
        #         .id_sec = ifelse(is.na(c[[input$variable_sec]]), paste(c[['.id']]),paste(substring(input$variable_sec,1,3), c[['.id']], sep = '_')))
        # })
        dat2 = dat2 %>% mutate(sec = (dat2[[input$variable_sec]]-recal2$bvar)/recal$avar, .id_sec = ifelse(is.na(dat2[[input$variable_sec]]), paste(dat2[['.id']]),paste(substring(input$variable_sec,1,3), dat2[['.id']], sep = '_'))) %>% as.data.frame()

      }}
      #plot_sec_axis
      sel = input$table_rows_selected
      # dat2 = e()
      # dat3 = dat2[sel,]
      if(isFALSE(input$regre_switch)){
        rows = ceiling(length(levels(droplevels(dat2$.id)))/6)
      }
      if(isTRUE(input$regre_switch) & !is.na(input$variable_sec)){
        ggplot(data = dat2, aes(x=!!rlang::sym(input$variable_prim), y=!!rlang::sym(input$variable_sec)))+
          geom_point()+
          geom_smooth(aes(colour = !!rlang::sym(input$Groupby_upper_plot)), method = input$method_upper_plot, se=input$se_switch_upper_plot)+
          theme_classic()
      } else {
      ggplot(data = dat2, aes(x=!!rlang::sym("date_time"), y=!!rlang::sym(input$variable_prim), colour=!!rlang::sym(".id")))+
        {if(isFALSE(input$group_switch_upper_plot)){geom_line()
        }else {
          geom_smooth(aes(colour = !!rlang::sym(input$Groupby_upper_plot)),method = input$method_upper_plot, se=input$se_switch_upper_plot)}}+
        # geom_point(data = dat3,fill = "black", size = 3, alpha= 0.6, show.legend = F)+
        {if(isFALSE(input$group_switch_upper_plot)){geom_line(aes(x=!!rlang::sym("date_time"), y=!!rlang::sym("sec"), colour=!!rlang::sym(".id_sec")))
        }else {
          geom_smooth(aes(y=!!rlang::sym("sec"),colour = !!rlang::sym(input$Groupby_upper_plot)),method = input$method_upper_plot, se=input$se_switch_upper_plot, linetype = 2)}}+
        scale_y_continuous(name = input$variable_prim,sec.axis = sec_axis(~.*recal$avar+recal2$bvar, name = ifelse(isTRUE(input$freeze_switch),paste0(input$method3, " freeze_show"), input$variable_sec)))}+
        coord_cartesian(xlim = zooming$x, ylim = zooming$y,expand = T)+
        theme_bw()+
        guides(colour = guide_legend(title = NULL, direction = "vertical", byrow = T, nrow = rows))+
        theme(legend.position = "top")
    }
    plot_sec_axis_3 = function() {
      recal = reactiveValues(avar = (max(d$a[[input$variable_sec]],na.rm = T)-min(d$a[[input$variable_sec]],na.rm = T))/(max(d$a[[input$variable_prim]],na.rm = T)-min(d$a[[input$variable_prim]],na.rm = T)))
      recal2 = reactiveValues(bvar = min(d$a[[input$variable_sec]],na.rm = T)-(min(d$a[[input$variable_prim]],na.rm = T)*recal$avar))
      e = reactive({
        cbind(d$a,  sec = (d$a[[input$variable_sec]]-recal2$bvar)/recal$avar,
              .id_sec = ifelse(is.na(d$a[[input$variable_sec]]), paste(d$a[['.id']]),paste(substring(input$variable_sec,1,3), d$a[['.id']], sep = '_')))
      })
      #plot_sec_axis
      sel = input$table_rows_selected
      dat2 = e()
      dat3 = dat2[sel,]
      rows = ceiling(length(levels(droplevels(dat2$.id)))/6)
      ggplot(data = dat2, aes(x=!!rlang::sym("date_time"), y=!!rlang::sym(input$variable_prim), colour=!!rlang::sym(".id")))+
        {if(isFALSE(input$group_switch_lower_plot)){geom_line()
        }else {
          geom_smooth(aes(colour = !!rlang::sym(input$Groupby_lower_plot)),method = input$method_lower_plot, se=input$se_switch_lower_plot)}}+
        geom_point(data = dat3,fill = "black", size = 3, alpha= 0.6, show.legend = F)+
        {if(isFALSE(input$group_switch_lower_plot)){geom_line(aes(x=!!rlang::sym("date_time"), y=!!rlang::sym("sec"), colour=!!rlang::sym(".id_sec")))
        }else {
          geom_smooth(aes(y=!!rlang::sym("sec"),colour = !!rlang::sym(input$Groupby_lower_plot)),method = input$method_lower_plot, se=input$se_switch_lower_plot)}}+
        scale_y_continuous(name = input$variable_prim,sec.axis = sec_axis(~.*recal$avar+recal2$bvar, name = input$variable_sec))+
        coord_cartesian(xlim = zooming$x, ylim = zooming$y,expand = T)+
        theme_bw()+
        guides(colour = guide_legend(title = NULL, direction = "vertical", byrow = T, nrow = rows))+
        theme(legend.position = "top")
    }
    #plot GS ----
    plot_GS = reactiveValues(active = FALSE)
    observeEvent(input$bar36,{
        if(!is.null(input$variable_prim)){
          shiny::withProgress(
            message = paste0("Processing..."),
            detail = "Uploading data",
            value = 0,
            {
            if(any(colnames(d$a) %in% c("GS_start"))){
                data_fit = d$a %>%
                  select(where(is.factor), .id, input$variable_prim, date_time) %>%
                  dplyr::mutate(date_time = floor_date(date_time, "day"), year = as.factor(year(date_time))) %>%
                  dplyr::rename(Variable = input$variable_prim) %>%
                  dplyr::group_by(.id, year) %>%
                  dplyr::filter(!all(is.na(Variable))) %>% droplevels() %>%
                  group_by(.id, year, date_time) %>%
                  dplyr::summarise_if(is.numeric, mean, na.rm = T) %>%
                  ungroup()
                data_fit = data_fit %>% left_join(d$a %>% select(.id, date_time, GS_start), by = c(".id", "date_time")) %>%
                  group_by(.id, year) %>%
                  dplyr::filter(!all(is.na(Variable))) %>% droplevels() %>%
                  mutate(Variable = case_when(all(is.na(GS_start)) ~ Variable - first(na.omit(Variable)),
                                              !all(is.na(GS_start)) ~ Variable - Variable[date_time == first(na.omit(GS_start))])) %>%
                  mutate(Variable_rate = (Variable - dplyr::lag(Variable,14))/14) %>%
                  mutate(GRO = if_else(date_time<first(na.omit(GS_start)), 0, Variable, missing = Variable)) %>%
                  mutate(GRO = if_else(is.na(GRO), NA , cummax(if_else(is.na(GRO), -Inf, GRO)))) %>%
                  mutate(GRO_rate = (GRO - dplyr::lag(GRO,14))/14) %>%
                  mutate(GRO_rate = if_else(GRO_rate < 2, 0, GRO_rate)) %>%
                  mutate(TWD = if_else(is.na(Variable) | GRO <= 0, NA, Variable-GRO)) %>%
                  {if(!any(colnames(d$a) %in% c("GS_end"))){mutate(.data = ., GS_end = NA)}else{.}}
                model_fit = data_fit %>% select(.id, date_time, year, GRO_rate) %>% group_by(.id, year) %>% dplyr::mutate(rows = 1:n()) %>%
                  tidyr::drop_na(GRO_rate) %>% droplevels() %>% dplyr::mutate(model_rate = as.numeric(tryCatch(mgcv::predict.gam(mgcv::gam(formula = GRO_rate ~ s(rows, bs = "cs", k = 20), method = "REML")),error = function(e) return(0))))
                data_fit = data_fit %>% left_join(model_fit %>% select(-rows, -GRO_rate), by = c(".id", "date_time", "year")) %>% select(-GS_start) %>% as.data.frame()
              } else {
                data_fit = d$a %>%
                  select(where(is.factor), .id, input$variable_prim, date_time) %>%
                  dplyr::mutate(date_time = lubridate::floor_date(date_time, "day"), year = as.factor(year(date_time))) %>%
                  dplyr::rename(Variable = input$variable_prim) %>%
                  dplyr::group_by(.id, year) %>%
                  dplyr::filter(!all(is.na(Variable))) %>% droplevels() %>%
                  group_by(.id, year, date_time) %>%
                  dplyr::summarise_if(is.numeric, mean, na.rm = T) %>%
                  ungroup() %>%
                  group_by(.id, year) %>%
                  dplyr::filter(!all(is.na(Variable))) %>% droplevels() %>%
                  mutate(Variable = Variable - first(na.omit(Variable))) %>%
                  mutate(Variable_rate = (Variable - dplyr::lag(Variable,14))/14) %>%
                  mutate(GRO = if_else(is.na(Variable), NA , cummax(if_else(is.na(Variable), -Inf, Variable)))) %>%
                  mutate(GRO_rate = (GRO - dplyr::lag(GRO,14))/14) %>%
                  mutate(GRO_rate = if_else(GRO_rate < 2, 0, GRO_rate)) %>%
                  mutate(TWD = if_else(is.na(Variable) | GRO <= 0, NA, Variable-GRO)) %>%
                  {if(!any(colnames(d$a) %in% c("GS_start"))){mutate(.data = ., GS_start = NA)}else{.}} %>%
                  {if(!any(colnames(d$a) %in% c("GS_end"))){mutate(.data = ., GS_end = NA)}else{.}}
                model_fit = data_fit %>% select(.id, date_time, year, GRO_rate) %>% group_by(.id, year) %>% dplyr::mutate(rows = 1:n()) %>%
                  tidyr::drop_na(GRO_rate) %>% droplevels() %>% dplyr::mutate(model_rate = as.numeric(tryCatch(mgcv::predict.gam(mgcv::gam(formula = GRO_rate ~ s(rows, bs = "cs", k = 20), method = "REML")),error = function(e) return(0))))
                data_fit = data_fit %>% left_join(model_fit %>% select(-rows, -GRO_rate), by = c(".id", "date_time", "year")) %>% as.data.frame()
              }
            shiny::incProgress(8/10, detail = "Merging data")
            d$c = d$a %>%
              dplyr::group_by(.id) %>%
              dplyr::filter(hour(date_time) == 00 & minute(date_time) == 00) %>%
              ungroup() %>%
              dplyr::mutate(year = as.factor(year(date_time))) %>%
              dplyr::select(-any_of(setdiff(colnames(data_fit), c(".id", "date_time", "year")))) %>%
              dplyr::left_join(data_fit, by = c(".id", "date_time", "year")) %>%
              group_by(.id, year) %>%
              as.data.frame()
            rm(data_fit, model_fit)
            shiny::incProgress(2/10, detail = "Done")
          plot_GS$active = TRUE
          shinyWidgets::updateMaterialSwitch(session, "lower_plot_switch", value = T)
        })
          }else{
          NULL
        }
    })
    plot_prim_up_GS = function() {
      if(isFALSE(input$one_by_one_switch)){
        c_upper = d$c
        c_upper = c_upper %>% filter(.id %in% input$one_by_one_group_select)
        } else {
          c_upper = d$c
      }
      ggplot(data = c_upper, aes(x=!!rlang::sym("date_time"), y=!!rlang::sym(input$variable_prim), colour=!!rlang::sym(".id")))+
        geom_line()+
        geom_vline(aes(xintercept = as.POSIXct(!!rlang::sym("GS_start"), tz = "UTC")), colour = "green")+
        geom_vline(aes(xintercept = as.POSIXct(!!rlang::sym("GS_end"), tz = "UTC")), colour = "red")+
        coord_cartesian(xlim = zooming$x,  ylim = NULL, expand = T)+
        theme_bw()+
        {if(isTRUE(plot_GS$active) & isFALSE(input$GS_switch_plot)){theme(
          panel.background = element_rect(fill = "grey95"),
          panel.grid = element_blank())
        }}
    }
    plot_prim_down_GS = function() {
      if(isFALSE(input$one_by_one_switch)){
        c_lower = d$c
        c_lower = c_lower %>% filter(.id %in% input$one_by_one_group_select) %>% dplyr::mutate(GS_end = as.POSIXct(ifelse(is.na(GS_end), NA, GS_end+lubridate::days(14)),origin = '1970-01-01', tz = "UTC")) %>% as.data.frame()
        } else {
          c_lower = d$c
          c_lower = c_lower %>% dplyr::mutate(GS_end = as.POSIXct(ifelse(is.na(GS_end), NA, GS_end+lubridate::days(14)),origin = '1970-01-01', tz = "UTC")) %>% as.data.frame()
      }
      ggplot(data = c_lower, aes(x=!!rlang::sym("date_time"), y=!!rlang::sym("GRO_rate"), colour=!!rlang::sym(".id")))+
        geom_line()+
        geom_line(aes(x=!!rlang::sym("date_time"), y=!!rlang::sym("Variable_rate"), group = !!rlang::sym(".id")), colour = "grey", alpha = .4)+
        geom_line(aes(x=!!rlang::sym("date_time"), y=!!rlang::sym("model_rate"), group = !!rlang::sym(".id")), colour = "black")+
        geom_vline(aes(xintercept = as.POSIXct(!!rlang::sym("GS_start"), tz = "UTC"), group = !!rlang::sym(".id")), colour = "green")+
        geom_vline(aes(xintercept = as.POSIXct(!!rlang::sym("GS_end"), tz = "UTC"), group = !!rlang::sym(".id")), colour = "red")+
        geom_hline(aes(yintercept = 0))+
        coord_cartesian(xlim = zooming$x,  ylim = NULL, expand = T)+
        theme_bw()+
        theme(legend.position = "none")+
        {if(isTRUE(plot_GS$active) & isTRUE(input$GS_switch_plot)){theme(
          panel.background = element_rect(fill = "grey95"),
          panel.grid = element_blank())
        }}
    }
    ampl <- function(x){
      min_max(x,method = "max")-min_max(x, method = "min")
    }
    cleaner_mean = function(x,na.rm = T, inf_fix = NA){
      if(length(x)>0 & !all(is.na(x))) mean(x, na.rm = na.rm) else inf_fix
    }
    upper <- function(x, probs  = input$quantile_upper_threshold, na.rm = T){
      as.numeric(quantile(x, probs = probs, na.rm = na.rm))
    }
    lower <- function(x, probs  = input$quantile_lower_threshold, na.rm = T){
      as.numeric(quantile(x, probs = probs, na.rm = na.rm))
    }
    cleaner_groups = function(x){
      cumsum(c(TRUE, diff(x) != 1  & diff(x) != 0))
    }
    # cleaner mode----
    cleaner_mode_rect = reactiveValues(data = NULL)
    cleaner_mode_rect_fnct = function(x){
      if(paste0("outliers_", input$variable_prim) %in% colnames(x)){
        cleaner_mode_rect_fnct_tmp = x %>% select(.id, day, paste0("outliers_", input$variable_prim)) %>% filter(.id %in% levels(df$.id)) %>%
          dplyr::arrange(.id, day) %>% group_by(.id) %>% mutate_if(is.logical, list(~ na_if(.,T))) %>% filter_if(is.logical, ~is.na(.)) %>%
          mutate(na_order = as.factor(cleaner_groups(day))) %>% group_by(na_order, .add = T) %>% dplyr::summarise(day_min = first(day), day_max = last(day)) %>% select(-na_order) %>% as.data.frame()
      if(nrow(cleaner_mode_rect_fnct_tmp)<1){
        return(NULL)
      }else{
        return(cleaner_mode_rect_fnct_tmp)
      }
        } else {
        return(NULL)
      }
    }
    cleaner_mode_rect_fnct_levelup = function(x){
      if( paste0("levelup_", input$variable_prim) %in% colnames(x)){
        cleaner_mode_rect_fnct_levelup_tmp = x %>% select(.id, day, paste0("levelup_", input$variable_prim)) %>% filter(.id %in% levels(df$.id)) %>%
          dplyr::arrange(.id, day) %>% group_by(.id) %>% mutate_if(is.logical, list(~ na_if(.,T))) %>% filter_if(is.logical, ~is.na(.)) %>%
          mutate(na_order = as.factor(cleaner_groups(day))) %>% group_by(na_order, .add = T) %>% dplyr::summarise(day_min = first(day), day_max = last(day)) %>% select(-na_order) %>% as.data.frame()
        if(nrow(cleaner_mode_rect_fnct_levelup_tmp)<1){
          return(NULL)
        }else{
          return(cleaner_mode_rect_fnct_levelup_tmp)
        }
        }else{
        return(NULL)
      }
      }
    # cleaner_mode_calculator = function(){
    #   if(isTRUE(input$cleaner_mode_switch)){
    #     if(input$compare_within == "within days and across ids"){
    #       data_correct_grouping = "day"
    #     } else {if(input$compare_within == "within ids and across days"){
    #       data_correct_grouping = ".id"
    #     } else {
    #       data_correct_grouping = NULL
    #     }
    #     }
    #     if(isTRUE(input$compare_to_selected)){
    #       if(isFALSE(input$one_by_one_switch)){
    #         lower_day =  head(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"),1)["date_time"]
    #         upper_day  = tail(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"),1)["date_time"]
    #       }else{
    #         lower_day =  head(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"),1)["date_time"]
    #         upper_day  = tail(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"),1)["date_time"]
    #       }
    #     }
    #     # level-up part
    #     if(isTRUE(input$cleaner_levelup_switch)){
    #       #week searching and data filter
    #       if(isTRUE(input$accept_nas2_switch)){
    #         cleaner_mode_data_levelup = isolate(d$a) %>% mutate(nas = !!rlang::sym(input$variable_prim)) %>% tidyr::fill(!!rlang::sym(input$variable_prim), .direction = "downup")
    #       }else{
    #         cleaner_mode_data_levelup = isolate(d$a) %>% mutate(nas = 1)
    #       }
    #       #searching differences
    #         cleaner_mode_data_levelup = cleaner_mode_data_levelup %>% dplyr::group_by(.id)%>%
    #         dplyr::mutate(Index_level2 = !!rlang::sym(input$variable_prim)-lag(!!rlang::sym(input$variable_prim))) %>%
    #         dplyr::mutate(Index_level2 = ifelse(abs(Index_level2) > input$cleaner_levelup_threshold | is.na(nas), TRUE, FALSE)) %>%
    #           filter(Index_level2 == TRUE) %>%
    #           # dplyr::rename_with(~paste0("levelup_",input$variable_prim), "outliers") %>%
    #           plyr::rename(., c("Index_level2" = paste0("levelup_",input$variable_prim))) %>%
    #           mutate(day = lubridate::floor_date(date_time, "day")) %>%
    #         select(.id, day, paste0("levelup_",input$variable_prim))%>%
    #         distinct(.keep_all = T) %>%
    #         as.data.frame()
    #     } else {
    #       cleaner_mode_data_levelup = NULL
    #     }
    #     # outliers part
    #     # all columns version
    #     # cleaner_mode_columns = isolate(d$a) %>% select_if(is.numeric) %>% colnames()
    #     # single variable version
    #     cleaner_mode_columns = isolate(d$a) %>% select(input$variable_prim) %>% colnames()
    #     if(length(isolate(d$a) %>% select_if(is.numeric)) == 1) {
    #       cleaner_mode_data_day = isolate(d$a) %>% mutate(day = floor_date(date_time, "day")) %>% group_by(.id , day) %>% summarise_if(is.numeric,list(ampl = ampl, mean = cleaner_mean)) %>% dplyr::rename_with(function(x){paste0(input$variable_prim,"_",x)},where(is.numeric)) %>% as.data.frame()
    #     } else {
    #       cleaner_mode_data_day = isolate(d$a) %>% mutate(day = floor_date(date_time, "day")) %>% group_by(.id , day) %>% summarise_if(is.numeric,list(ampl = ampl, mean = cleaner_mean)) %>% as.data.frame()
    #     }
    #     cleaner_mode_data_outliers = cleaner_mode_data_day %>% select(.id, day) %>% ungroup()
    #     if(input$quantile_correct_input == "Quantiles"){
    #       for(i in cleaner_mode_columns){
    #         # quantiles
    #         if(isTRUE(input$compare_to_selected)){data_day_orig = cleaner_mode_data_day %>% droplevels() %>% select_at(dplyr::vars(tidyselect::contains(i),".id", "day")) %>% rename_with(~sub(paste0(i,"_"), "", .x),everything()) %>% as.data.frame()}
    #         data_correct = cleaner_mode_data_day %>% droplevels() %>% select_at(dplyr::vars(tidyselect::contains(i),".id", "day")) %>%
    #           {if(isTRUE(input$compare_to_selected)) dplyr::mutate(.data = ., across(starts_with(i), ~ replace(., get("day") > upper_day | get("day") < lower_day, NA))) else .} %>%
    #           group_by(across(data_correct_grouping)) %>% mutate_if(is.numeric, list(upper = upper, lower = lower)) %>%
    #           rename_with(~sub(paste0(i,"_"), "", .x),everything()) %>% {if(isTRUE(input$compare_to_selected)) dplyr::select(.data =.,-ampl, -mean) %>% dplyr::left_join(data_day_orig, by  = c(".id", "day")) else .} %>% group_by(.id, day) %>%
    #           {if(isTRUE(input$ampl_switch)) mutate(.data = ., outliers = ifelse(ampl>=ampl_lower & ampl<=ampl_upper, FALSE, TRUE)) else mutate(.data = ., outliers =  FALSE)} %>%
    #           {if(isTRUE(input$mean_switch)) mutate(.data = ., outliers = ifelse(mean>=mean_lower & mean<=mean_upper & outliers == FALSE, FALSE, TRUE)) else .} %>%
    #           mutate(year = as.factor(year(day))) %>% group_by(.id, year) %>% mutate(T_ratio = (sum(outliers == T | is.na(outliers))/n())) %>% mutate(outliers = ifelse(is.na(outliers), FALSE, ifelse(T_ratio > input$remove_all_threshold*0.01 & !is.na(outliers), TRUE, outliers))) %>%
    #           {if(isTRUE(input$rollingwindow_switch)) group_by(.data = .,.id) %>% mutate(roll_start = ifelse(zoo::rollsum(outliers,input$roll_window, align = "left", fill = NA) > 0, TRUE, FALSE), roll_end = rev(ifelse(zoo::rollsum(rev(outliers),input$roll_window, align = "left", fill = NA) > 0, TRUE, FALSE))) %>%
    #               mutate(outliers = if_else(roll_start == FALSE | roll_end == FALSE, FALSE, TRUE, missing = FALSE)) else .} %>%
    #           ungroup() %>% select(outliers, day, .id) %>% dplyr::rename_with(~paste0("outliers_",i), "outliers")
    #         cleaner_mode_data_outliers = cleaner_mode_data_outliers %>% left_join(data_correct, by = c(".id", "day")) %>% as.data.frame()}
    #       suppressWarnings(rm(data_correct, cleaner_mode_data_day, cleaner_mode_columns, i,data_correct_grouping, data_day_orig))
    #     }else{
    #       for(i in cleaner_mode_columns){
    #         # z score
    #         if(isTRUE(input$compare_to_selected)){data_day_orig = cleaner_mode_data_day %>% droplevels() %>% select_at(dplyr::vars(tidyselect::contains(i),".id", "day")) %>% rename_with(~sub(paste0(i,"_"), "", .x),everything()) %>% as.data.frame()}
    #         data_correct = cleaner_mode_data_day %>% droplevels() %>% select_at(dplyr::vars(tidyselect::contains(i),".id", "day")) %>%
    #           {if(isTRUE(input$compare_to_selected)) dplyr::mutate(.data = ., across(starts_with(i), ~ replace(., get("day") > upper_day | get("day") < lower_day, NA))) else .} %>%
    #           group_by(across(data_correct_grouping)) %>% mutate_if(is.numeric, list(mean = mean, sd = sd), na.rm = T) %>%
    #           rename_with(~sub(paste0(i,"_"), "", .x),everything()) %>% {if(isTRUE(input$compare_to_selected)) select(.data =.,-ampl, -mean) %>% left_join(data_day_orig, by  = c(".id", "day")) else .} %>% group_by(.id, day) %>% mutate (score_ampl = abs(ampl-ampl_mean)/abs(ampl_sd), score_mean = abs(mean-mean_mean)/abs(mean_sd)) %>%
    #           {if(isTRUE(input$ampl_switch)) mutate(.data = ., outliers = ifelse(score_ampl<=input$z_score_threshold, FALSE, TRUE)) else mutate(.data = ., outliers =  FALSE)} %>%
    #           {if(isTRUE(input$mean_switch)) mutate(.data = ., outliers = ifelse(score_mean<=input$z_score_threshold & outliers == FALSE, FALSE, TRUE)) else .} %>%
    #           mutate(year = as.factor(year(day))) %>% group_by(.id, year) %>% mutate(T_ratio = (sum(outliers == T| is.na(outliers))/n())) %>% mutate(outliers = ifelse(is.na(outliers), FALSE, ifelse(T_ratio > input$remove_all_threshold*0.01 & !is.na(outliers), TRUE, outliers))) %>%
    #           {if(isTRUE(input$rollingwindow_switch)) group_by(.data = .,.id) %>% mutate(roll_start = ifelse(zoo::rollsum(outliers,input$roll_window, align = "left", fill = NA) > 0, TRUE, FALSE), roll_end = rev(ifelse(zoo::rollsum(rev(outliers),input$roll_window, align = "left", fill = NA) > 0, TRUE, FALSE))) %>%
    #               mutate(outliers = if_else(roll_start == FALSE | roll_end == FALSE, FALSE, TRUE, missing = FALSE)) else .} %>%
    #           ungroup() %>% select(outliers, .id, day) %>% dplyr::rename_with(~paste0("outliers_",i), "outliers")
    #         cleaner_mode_data_outliers = cleaner_mode_data_outliers %>% left_join(data_correct, by = c(".id", "day")) %>% as.data.frame()}
    #       suppressWarnings(rm(data_correct, cleaner_mode_data_day, cleaner_mode_columns, i, data_correct_grouping, data_day_orig))
    #     }
    #     if(!is.null(cleaner_mode_data_levelup)){
    #       cleaner_mode_data_outliers = cleaner_mode_data_outliers %>% full_join(cleaner_mode_data_levelup, by = c(".id", "day")) %>%
    #         mutate(across(starts_with(paste0("outliers_", input$variable_prim)), ~ replace(., get(paste0("levelup_", input$variable_prim)) == TRUE, FALSE))) %>%
    #         mutate_if(is.logical, ~tidyr::replace_na(., FALSE)) %>% as.data.frame()
    #       assign("cleaning_meta", cleaner_mode_data_outliers, envir = envir)
    #       cleaner_mode_rect$cleaning_meta = cleaner_mode_data_outliers
    #       cleaner_mode_rect$data = cleaner_mode_rect_fnct(cleaner_mode_data_outliers)
    #       cleaner_mode_rect$data_levelup = cleaner_mode_rect_fnct_levelup(cleaner_mode_data_outliers)
    #       suppressWarnings(rm(cleaner_mode_data_outliers,cleaner_mode_data_levelup))
    #     }else{
    #       assign("cleaning_meta", cleaner_mode_data_outliers, envir = envir)
    #       cleaner_mode_rect$cleaning_meta = cleaner_mode_data_outliers
    #       cleaner_mode_rect$data = cleaner_mode_rect_fnct(cleaner_mode_data_outliers)
    #       suppressWarnings(rm(cleaner_mode_data_outliers))
    #     }
    #
    #   }else{
    #     NULL
    #   }
    # }
    observeEvent(input$cleaner_upload_btn,{
      cleaner_meta_choices = df_choices()
      cleaner_meta_choices = cleaner_meta_choices[startsWith(cleaner_meta_choices, "cleaning_meta")]
      if(length(cleaner_meta_choices) < 1){
        showModal(modalDialog(
          title = "No compatible data frame found!",
          HTML("Any data-frame named 'cleaning_meta' in environment."),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }else{
        showModal(jqui_draggable(modalDialog(
          selectInput("input_cleaner_meta_choices",
                      "Cleaning files:",
                      choices = cleaner_meta_choices,
                      multiple = F,
                      selectize = F),
          renderDataTable({
            datatable(head(get(input$input_cleaner_meta_choices)),options=list(scrollX=TRUE))
          }),
          easyClose = T,
          footer = tagList(actionButton("cleaner_upload_btn_OK", "Upload", class = "btn-success"),
                           modalButton("Cancel")),
          size = "l"
        )))
      }
    })
    observeEvent(input$cleaner_upload_btn_OK,{
      req(!is.null(input$input_cleaner_meta_choices))
      cleaner_mode_rect$cleaning_meta = get(input$input_cleaner_meta_choices)
      cleaner_mode_rect$data = cleaner_mode_rect_fnct(get(input$input_cleaner_meta_choices))
      cleaner_mode_rect$data_levelup = cleaner_mode_rect_fnct_levelup(get(input$input_cleaner_meta_choices))
      removeModal()
      })
    observeEvent(input$cleaner_recalculate_btn,{
      tryCatch({
        shiny::withProgress(
          message = paste0("Processing..."),
          detail = "Analysing data",
          value = 0,
          {
            shiny::incProgress(5/10, detail = "Detecting outliers")
            cleaner_mode_calculator()
            shiny::incProgress(5/10, detail = "Done")
          })
      },
      error = function(err){
        showNotification(paste0("No data selected?"), type = "error")
      }
      )
    },ignoreInit = T, ignoreNULL = T)
    observeEvent(input$cleaner_refresh_btn,{
      shiny::withProgress(message = paste0("Processing..."),
                          detail = "Refresing data",
                          value = 5,
                          {if("cleaning_meta" %in% ls(envir = envir)){
                            cleaner_mode_rect$cleaning_meta = NULL
                            assign("cleaning_meta", cleaner_mode_rect$cleaning_meta, envir = envir)
                            cleaner_mode_rect$data = NULL
                            cleaner_mode_rect$data_levelup = NULL
                          }else{
                            NULL
                          }
                            shiny::incProgress(5/10, detail =  "Done")
                            })

    })
    observeEvent(input$cleaner_add_btn,{
      shiny::withProgress(message = paste0("Processing..."),
                          detail = "Adding data",
                          value = 5,
                            {if(!is.null(cleaner_mode_rect$cleaning_meta)){
                            cleaning_meta_orig = cleaner_mode_rect$cleaning_meta
                            cleaner_mode_calculator()
                            if(any(colnames(cleaning_meta_orig %>% select_if(is.logical)) %in% colnames(cleaner_mode_rect$cleaning_meta %>% select_if(is.logical)))){
                              cleaner_mode_rect$cleaning_meta = cleaning_meta_orig %>% full_join(cleaner_mode_rect$cleaning_meta, by = c(".id", "day")) %>% replace(is.na(.), FALSE) %>%  dplyr::mutate(across(ends_with(".x"), ~ replace(., get(paste0("outliers_", input$variable_prim, ".y")) == TRUE, TRUE))) %>%
                                dplyr::select(!ends_with(".y")) %>% dplyr::rename_all(~sub('.x', '', .x, fixed=T))
                              rm(cleaning_meta_orig)
                              assign("cleaning_meta", cleaner_mode_rect$cleaning_meta, envir = envir)
                              cleaner_mode_rect$data = cleaner_mode_rect_fnct(cleaner_mode_rect$cleaning_meta)
                            }else{
                              cleaner_mode_rect$cleaning_meta = cleaning_meta_orig %>% full_join(cleaner_mode_rect$cleaning_meta, by=c(".id", "day")) %>% replace(is.na(.), FALSE)
                              rm(cleaning_meta_orig)
                              assign("cleaning_meta", cleaner_mode_rect$cleaning_meta, envir = envir)
                              cleaner_mode_rect$data = cleaner_mode_rect_fnct(cleaner_mode_rect$cleaning_meta)
                            }
                          }else{
                            cleaner_mode_calculator()
                          }
                          shiny::incProgress(5/10, detail =  "Done")
                          })
    })


    plot_prim_cleaner_mode = function() {
      rect_ids = if(isFALSE(input$one_by_one_switch)){input$one_by_one_group_select}else{if(is.null(cleaner_mode_rect$data )){NULL}else{cleaner_mode_rect$data %>% dplyr::select(.id) %>% distinct(.id) %>% dplyr::pull(.id)}}
      rect_ids_levelup = if(isFALSE(input$one_by_one_switch)){input$one_by_one_group_select}else{if(is.null(cleaner_mode_rect$data_levelup)){NULL}else{cleaner_mode_rect$data_levelup %>% dplyr::select(.id) %>% distinct(.id) %>% dplyr::pull(.id)}}
      rect_data = if(is.null(cleaner_mode_rect$data )){data.frame()}else{cleaner_mode_rect$data %>% filter(.id %in% rect_ids) %>% droplevels()}
      rect_data_levelup = if(is.null(cleaner_mode_rect$data_levelup )){data.frame()}else{cleaner_mode_rect$data_levelup %>% filter(.id %in% rect_ids_levelup) %>% droplevels()}
      plot_prim_cleaner_mode_data = plot_plot_data() %>% {
        if (input$upper_plot_interval_input == "original") {
          .
        } else if (input$upper_plot_interval_input == "min") {
          dplyr::group_by(., .id) %>%
            dplyr::filter(lubridate::second(date_time) == 0)
        } else if (input$upper_plot_interval_input == "hour") {
          dplyr::group_by(., .id) %>%
            dplyr::filter(lubridate::minute(date_time) == 0)
        } else if (input$upper_plot_interval_input == "day") {
          dplyr::group_by(., .id) %>%
            dplyr::filter(lubridate::hour(date_time) == 0 & lubridate::minute(date_time) == 0)
        } else {
          .
        }
      }
      ggplot(data = plot_prim_cleaner_mode_data, aes(!!rlang::sym("date_time"), !!rlang::sym(input$variable_prim), colour = !!rlang::sym(".id"))) +
        geom_line()+
      # change7----
      # {if(isFALSE(input$fine_mode)){{if(length(rect_ids)>0 & !is.null(rect_data) & nrow(rect_data)>0){geom_rect(data = rect_data, inherit.aes = F, aes(xmin = day_min, xmax = day_max + hours(23) + minutes(59), ymin = -Inf, ymax = Inf, fill = .id), colour = NA ,alpha = 0.3, show.legend = F)}}}
      #   else{{if(length(rect_ids)>0 & !is.null(rect_data) & nrow(rect_data)>0){geom_rect(data = rect_data, inherit.aes = F, aes(xmin = day_min, xmax = day_max, ymin = -Inf, ymax = Inf, fill = .id), colour = NA ,alpha = 0.3, show.legend = F)}}}}+
        {if(length(rect_ids)>0 & !is.null(rect_data) & nrow(rect_data)>0){geom_rect(data = rect_data, inherit.aes = F, aes(xmin = day_min, xmax = day_max + hours(23) + minutes(59), ymin = -Inf, ymax = Inf, fill = .id), colour = NA ,alpha = 0.3, show.legend = F)}}+
      # {if(isFALSE(input$fine_mode)){{if(length(rect_ids_levelup)>0 & !is.null(rect_data_levelup) & nrow(rect_data_levelup)>0){geom_rect(data = rect_data_levelup, inherit.aes = F, aes(xmin = day_min, xmax = day_max + hours(23) + minutes(59), ymin = -Inf, ymax = Inf, fill = .id, colour = .id),alpha = 0.05, show.legend = F)}}}
      #   else{{if(length(rect_ids_levelup)>0 & !is.null(rect_data_levelup) & nrow(rect_data_levelup)>0){geom_rect(data = rect_data_levelup, inherit.aes = F, aes(xmin = day_min, xmax = day_max, ymin = -Inf, ymax = Inf, fill = .id, colour = .id),alpha = 0.05, show.legend = F)}}}}+
        {if(length(rect_ids_levelup)>0 & !is.null(rect_data_levelup) & nrow(rect_data_levelup)>0){geom_rect(data = rect_data_levelup, inherit.aes = F, aes(xmin = day_min, xmax = day_max + hours(23) + minutes(59), ymin = -Inf, ymax = Inf, fill = .id, colour = .id),alpha = 0.05, show.legend = F)}}+
        coord_cartesian(xlim = zooming$x,  ylim = zooming$y, expand = T)+
              theme_bw()
    }
    # Cleaner menu----
    output$dynamicInput_cleaner <- renderUI({
      if(isTRUE(input$cleaner_mode_switch)){
        tags$div(
          actionButton("Cleaner_Button_delete", "Delete"),
          actionButton("Cleaner_Button_keep", "Keep"),
          actionButton("Cleaner_Button_levelup", "Level-up"),
          actionButton("Cleaner_Button_levelup_detect", "Detect jumps"),
          actionButton("Cleaner_Button_apply", "Apply", style = "margin-left: 10px"),
          actionButton("Cleaner_Button_save", "Save"),
          actionButton(inputId = "cleaner_refresh_btn", label = "Refresh", style = "margin-left: 10px"),
          actionButton(inputId = "cleaner_upload_btn", label = "Upload"),
          # change7----
          # shinyWidgets::materialSwitch("fine_mode", "Fine mode", status = "primary",value = F),
          tags$hr()
        )
      } else {
        return(NULL)
      }
    })
observeEvent(input$Cleaner_Button_delete,{
  if(is.null(input$RadiusPlot_brush)){
    NULL
  }else{
  if(isFALSE(input$one_by_one_switch)){
    df10 = brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time")
  }else{
    df10 = brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time")
  }
    # chnage7----
    # if(isFALSE(input$fine_mode)){
      df10 = df10 %>% droplevels() %>% mutate(day = floor_date(date_time, "day")) %>% select(.id, day, input$variable_prim) %>% rename_with(~sub(paste0(input$variable_prim), paste0("outliers_", input$variable_prim), .x),everything()) %>%
        dplyr::mutate(across(starts_with("outliers"), ~TRUE)) %>% distinct_all(.keep_all = T)
    # }else{
    #   df10 = df10 %>% droplevels() %>% mutate(day = date_time) %>% select(.id, day, input$variable_prim) %>% rename_with(~sub(paste0(input$variable_prim), paste0("outliers_", input$variable_prim), .x),everything()) %>%
    # dplyr::mutate(across(starts_with("outliers"), ~TRUE)) %>% distinct_all(.keep_all = T)
    # }


  if("cleaning_meta" %in% ls(envir = envir) | !is.null(cleaner_mode_rect$cleaning_meta)){
      if(is.null(cleaner_mode_rect$cleaning_meta)){
        cleaner_mode_rect$cleaning_meta = df10 %>% as.data.frame()
        assign("cleaning_meta", df10, envir = envir)
        cleaner_mode_rect$data = cleaner_mode_rect_fnct(cleaner_mode_rect$cleaning_meta)
      }else{
        cleaner_mode_rect$cleaning_meta = isolate(cleaner_mode_rect$cleaning_meta) %>% full_join(df10, by = c(".id", "day")) %>% dplyr::mutate(across(ends_with(".x"), ~ replace(., get(paste0("outliers_", input$variable_prim, ".y")) == TRUE, TRUE))) %>%
          dplyr::select(!ends_with(".y")) %>% dplyr::rename_all(~sub('.x', '', .x, fixed=T)) %>% mutate_if(is.logical, ~replace_na(., FALSE))
        assign("cleaning_meta", cleaner_mode_rect$cleaning_meta, envir = envir)
        cleaner_mode_rect$data =  cleaner_mode_rect_fnct(cleaner_mode_rect$cleaning_meta)
      }
  }else{
    cleaner_mode_rect$cleaning_meta = df10 %>% as.data.frame()
    assign("cleaning_meta", df10, envir = envir)
    cleaner_mode_rect$data = cleaner_mode_rect_fnct(cleaner_mode_rect$cleaning_meta)
  }
  }
})
observeEvent(input$Cleaner_Button_keep,{
  if(is.null(input$RadiusPlot_brush)){
    NULL
  }else{
  if(isFALSE(input$one_by_one_switch)){
    df10 = brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time")
  }else{
    df10 = brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time")
  }
    # change7----
    # if(isFALSE(input$fine_mode)){
      df10 = df10 %>% droplevels() %>% mutate(day = floor_date(date_time, "day")) %>% select(.id, day, input$variable_prim) %>%
        dplyr::mutate(outliers = FALSE, levelup = FALSE) %>% plyr::rename(., c("outliers" = paste0("outliers_", input$variable_prim), "levelup" = paste0("levelup_", input$variable_prim))) %>% select(-input$variable_prim) %>% distinct_all(.keep_all = T)
    # }else{
    #   df10 = df10 %>% droplevels() %>% mutate(day = date_time) %>% select(.id, day, input$variable_prim) %>%
    #     dplyr::mutate(outliers = FALSE, levelup = FALSE) %>% plyr::rename(., c("outliers" = paste0("outliers_", input$variable_prim), "levelup" = paste0("levelup_", input$variable_prim))) %>% select(-input$variable_prim) %>% distinct_all(.keep_all = T)
    # }

  if("cleaning_meta" %in% ls(envir = envir) | !is.null(cleaner_mode_rect$cleaning_meta)){
      if(is.null(cleaner_mode_rect$cleaning_meta)){
        NULL
      }else{
        cleaner_mode_rect$cleaning_meta = isolate(cleaner_mode_rect$cleaning_meta) %>% full_join(df10, by = c(".id", "day")) %>% dplyr::mutate(across(starts_with(paste0("outliers_", input$variable_prim, ".x")), ~ replace(., get(paste0("outliers_", input$variable_prim, ".y")) == FALSE, FALSE))) %>%
          dplyr::mutate(across(starts_with(paste0("levelup_", input$variable_prim, ".x")), ~ replace(., get(paste0("levelup_", input$variable_prim, ".y")) == FALSE, FALSE))) %>% dplyr::select(!ends_with(".y")) %>% dplyr::rename_all(~sub('.x', '', .x, fixed=T)) %>% mutate_if(is.logical, ~replace_na(., FALSE))
        assign("cleaning_meta", cleaner_mode_rect$cleaning_meta, envir = envir)
        cleaner_mode_rect$data =  cleaner_mode_rect_fnct(cleaner_mode_rect$cleaning_meta)
        cleaner_mode_rect$data_levelup =  cleaner_mode_rect_fnct_levelup(cleaner_mode_rect$cleaning_meta)
      }
  }else{
    NULL
  }
  }
})
observeEvent(input$Cleaner_Button_apply,{
  shiny::withProgress(
    message = paste0("Cleaning..."),
    value = 0,
    {
      # outliers part
      shiny::incProgress(1/10, detail = "deleting")
      if(!is.null(cleaner_mode_rect$cleaning_meta)){
        for(i in colnames(cleaner_mode_rect$cleaning_meta %>% select_if(is.logical) %>% select(starts_with("outliers_")))) {
          meta_tmp = cleaner_mode_rect$cleaning_meta[c(".id","day", i)]
          # change7----
          #   df = d$b %>% {if(isFALSE(input$fine_mode)) mutate(.data = .,day = floor_date(date_time, "day")) else mutate(.data = .,day = date_time)} %>% left_join(meta_tmp, by = c(".id", "day")) %>%
          #     dplyr::mutate(across(starts_with(sub("outliers_", "", i, fixed=T)), ~ replace(., get(paste0(i)) == TRUE, NA))) %>%
          #     dplyr::select(!starts_with("outliers_"))
          df = d$b %>% mutate(day = floor_date(date_time, "day")) %>% left_join(meta_tmp, by = c(".id", "day")) %>%
            dplyr::mutate(across(starts_with(sub("outliers_", "", i, fixed=T)), ~ replace(., get(paste0(i)) == TRUE, NA))) %>%
            dplyr::select(!starts_with("outliers_"))
        }
        # levelup part
        shiny::incProgress(4/10, detail = "level-up")
        for(i in colnames(cleaner_mode_rect$cleaning_meta %>% select_if(is.logical) %>% select(starts_with("levelup_")))) {
          meta_tmp = cleaner_mode_rect$cleaning_meta[c(".id","day", i)]
          meta_tmp = meta_tmp %>% dplyr::arrange(.id, day) %>% group_by(.id) %>% mutate_if(is.logical, list(~ na_if(.,T))) %>% filter_if(is.logical, ~is.na(.)) %>%
            mutate(levelup_order = cleaner_groups(day)) %>% select(-any_of(i))
          # change7----
          # meta_tmp_before = meta_tmp %>% group_by(.id, levelup_order) %>% filter(row_number() == 1) %>%
          #  {if(isFALSE(input$fine_mode)) mutate(.data = ., day = day-lubridate::days(1)) else .}
          meta_tmp_before = meta_tmp %>% group_by(.id, levelup_order) %>% filter(row_number() == 1) %>%
            mutate(day = day-lubridate::days(1))

          meta_tmp = meta_tmp %>% bind_rows(meta_tmp_before) %>% arrange(.id, day) %>% group_by(.id, levelup_order)
          rm(meta_tmp_before)
          cleaner_data_tmp = df %>% select(.id, date_time, any_of(sub("levelup_", "",i))) %>% filter(.id %in% levels(meta_tmp$.id)) %>% droplevels() %>% group_by(.id )%>% arrange(date_time, .by_group = T) %>% mutate(nas = !!rlang::sym(sub("levelup_", "",i))) %>% as.data.frame()
          # change7----
          # meta_tmp =  cleaner_data_tmp %>% {if(isFALSE(input$fine_mode)) mutate(.data = ., day = lubridate::date(date_time)) else mutate(.data = ., day = date_time)} %>% left_join(meta_tmp, by = c(".id", "day")) %>% filter(!is.na(levelup_order)) %>% group_by(.id, levelup_order, day) %>% filter(row_number() == n()) %>% select(-date_time) %>%
          #   dplyr::rename(levelup_data = sub("levelup_", "",i)) %>%
          #   group_by(.id, levelup_order) %>% mutate(levelup_data_cumsum = ifelse(row_number() == 1, first(levelup_data)-last(levelup_data), 0), levelup_data = first(na.omit(levelup_data))) %>% group_by(.id) %>% mutate(levelup_data_cumsum = cumsum(levelup_data_cumsum)) %>% as.data.frame()
          meta_tmp =  cleaner_data_tmp %>% mutate(day = lubridate::date(date_time)) %>% left_join(meta_tmp, by = c(".id", "day")) %>% filter(!is.na(levelup_order)) %>% group_by(.id, levelup_order, day) %>% filter(row_number() == n()) %>% select(-date_time) %>%
            dplyr::rename(levelup_data = sub("levelup_", "",i)) %>%
            group_by(.id, levelup_order) %>% mutate(levelup_data_cumsum = ifelse(row_number() == 1, first(levelup_data)-last(levelup_data), 0), levelup_data = first(na.omit(levelup_data))) %>% group_by(.id) %>% mutate(levelup_data_cumsum = cumsum(levelup_data_cumsum)) %>% as.data.frame()

          meta_tmp_cumsum = meta_tmp %>% select(.id, day, levelup_order, levelup_data_cumsum) %>% group_by(.id, levelup_order) %>% filter(row_number()==n()) %>% mutate(day = day+lubridate::days(1)) %>% as.data.frame()
          meta_tmp = meta_tmp %>% select(-levelup_data_cumsum, -levelup_order) %>% as.data.frame()
          cleaner_data_tmp = cleaner_data_tmp %>% group_by(.id) %>% arrange(date_time, .by_group = T) %>% mutate(day = lubridate::floor_date(date_time, "day")) %>% left_join(meta_tmp, by = c(".id", "day")) %>% left_join(meta_tmp_cumsum, by = c(".id", "day"))  %>% tidyr::fill(levelup_data_cumsum, .direction = "down") %>% mutate(levelup_data_cumsum = tidyr::replace_na(levelup_data_cumsum,0)) %>%
          mutate(levelup_leveled = ifelse(is.na(levelup_data), !!rlang::sym(sub("levelup_", "",i)) +levelup_data_cumsum, levelup_data+levelup_data_cumsum)) %>%
          dplyr::mutate(levelup_leveled = replace(levelup_leveled, is.na(!!rlang::sym(sub("levelup_", "",i))), NA)) %>%
          select(.id, date_time, levelup_leveled) %>% plyr::rename(.,c("levelup_leveled" = sub("levelup_", "",i))) %>% as.data.frame()
          df = df %>% left_join(cleaner_data_tmp, by = c(".id", "date_time"))  %>%
            dplyr::mutate(across(paste0(sub("levelup_", "",i),".x"), ~ifelse(!is.na(get(paste0(sub("levelup_", "",i), ".y"))), get(paste0(sub("levelup_", "",i), ".y")), get(paste0(sub("levelup_", "",i), ".x")))))%>% dplyr::select(!ends_with(".y")) %>% dplyr::select(-any_of("day")) %>% dplyr::rename_all(~sub('.x', '', .x, fixed=T)) %>% as.data.frame()
        }
        shiny::incProgress(3/10, detail = "assigning")
        suppressWarnings(rm(meta_tmp, cleaner_data_tmp, meta_tmp_cumsum, i))
        assign("df", df, envir = envir)
        d$b = df
      } else {
        NULL
      }
      shiny::incProgress(2/10, detail = "done")
    })
})
Cleaner_Button_save_names = reactiveValues(name = 0)
observeEvent(input$Cleaner_Button_save,{
    if("cleaning_meta" %in% ls(envir = envir)){
    index = 0
    while(paste0("cleaning_meta_", index, sep = "") %in% ls(envir = envir)|
          Cleaner_Button_save_names$name >= index) {
      index = index+1
    }
    cleaning_meta_name = paste0("cleaning_meta_",index, sep = "")
    Cleaner_Button_save_names$name = index
  }else{
    cleaning_meta_name = paste0("cleaning_meta")

  }
  assign(cleaning_meta_name, cleaner_mode_rect$cleaning_meta, envir = envir)
  showNotification(paste0("Saved as: ", cleaning_meta_name))
})
# new changes5----
observeEvent(input$Cleaner_Button_levelup, {
  if (is.null(input$RadiusPlot_brush)) {
    NULL
  } else {
    if (isFALSE(input$one_by_one_switch)) {
      subset_df <- isolate(d$a)[isolate(d$a)[[".id"]] %in% input$one_by_one_group_select, ]
      bp <- brushedPoints(subset_df, input$RadiusPlot_brush, xvar = "date_time")
      first_brush <- bp %>%
        dplyr::filter(date_time == min_max(date_time)) %>%
        dplyr::distinct(date_time) %>%
        as.data.frame()
      last_brush <- bp %>%
        dplyr::filter(date_time == min_max(date_time, "max")) %>%
        dplyr::distinct(date_time) %>%
        as.data.frame()
      if (nrow(first_brush) == 0 || nrow(last_brush) == 0) {
        NULL
      } else {
        brushed_ids <- unique(bp$.id)
        df10 <- subset_df %>%
          dplyr::filter(.id %in% brushed_ids,
                        dplyr::between(date_time, first_brush$date_time, last_brush$date_time))
      }
    } else {
      subset_df <- isolate(d$a)
      bp <- brushedPoints(subset_df, input$RadiusPlot_brush, xvar = "date_time")
      first_brush <- bp %>%
        dplyr::filter(date_time == min_max(date_time)) %>%
        dplyr::distinct(date_time) %>%
        as.data.frame()
      last_brush <- bp %>%
        dplyr::filter(date_time == min_max(date_time, "max")) %>%
        dplyr::distinct(date_time) %>%
        as.data.frame()
      if (nrow(first_brush) == 0 || nrow(last_brush) == 0) {
        NULL
      } else {
        brushed_ids <- unique(bp$.id)
        df10 <- subset_df %>%
          dplyr::filter(.id %in% brushed_ids,
                        dplyr::between(date_time, first_brush$date_time, last_brush$date_time))
      }
    }
    if (!exists("df10")) {
      NULL
    } else {
      # change7----
      # if (isFALSE(input$fine_mode)){
      df10 = df10 %>%
        droplevels() %>%
        mutate(day = floor_date(date_time, "day")) %>%
        select(.id, day, input$variable_prim) %>%
        rename_with(~ sub(paste0(input$variable_prim),
                          paste0("levelup_", input$variable_prim), .x),
                    everything()) %>%
        dplyr::mutate(across(starts_with("levelup"), ~ TRUE)) %>%
        distinct_all(.keep_all = TRUE)
      if ("cleaning_meta" %in% ls(envir = envir) && !is.null(cleaner_mode_rect$cleaning_meta)) {
        if (is.null(cleaner_mode_rect$cleaning_meta)) {
          cleaner_mode_rect$cleaning_meta = df10 %>% as.data.frame()
          assign("cleaning_meta", df10, envir = envir)
          cleaner_mode_rect$data_levelup = cleaner_mode_rect_fnct_levelup(cleaner_mode_rect$cleaning_meta)
          rm(df10)
        } else {
          cleaner_mode_rect$cleaning_meta = isolate(cleaner_mode_rect$cleaning_meta) %>%
            full_join(df10, by = c(".id", "day")) %>%
            dplyr::mutate(across(ends_with(".x"),
                                 ~ replace(., get(paste0("levelup_", input$variable_prim, ".y")) == TRUE, TRUE))) %>%
            dplyr::select(!ends_with(".y")) %>%
            dplyr::rename_all(~ sub('.x', '', .x, fixed = TRUE)) %>%
            mutate_if(is.logical, ~ replace_na(., FALSE))

          assign("cleaning_meta", cleaner_mode_rect$cleaning_meta, envir = envir)
          cleaner_mode_rect$data_levelup = cleaner_mode_rect_fnct_levelup(cleaner_mode_rect$cleaning_meta)
          rm(df10)
        }
      } else {
        cleaner_mode_rect$cleaning_meta = df10 %>% as.data.frame()
        assign("cleaning_meta", df10, envir = envir)
        cleaner_mode_rect$data_levelup = cleaner_mode_rect_fnct_levelup(cleaner_mode_rect$cleaning_meta)
        rm(df10)
      }
    }
  }
})

# observeEvent(input$Cleaner_Button_levelup,{
#   if(is.null(input$RadiusPlot_brush)){
#     NULL
#   }else{
#     if(isFALSE(input$one_by_one_switch)){
#       first_brush = brushedPoints(isolate(d$a)[isolate(d$a)[[".id"]] %in% input$one_by_one_group_select,], input$RadiusPlot_brush, xvar = "date_time") %>% filter(date_time == min_max(date_time)) %>% distinct(date_time) %>% as.data.frame()
#       last_brush = brushedPoints(isolate(d$a)[isolate(d$a)[[".id"]] %in% input$one_by_one_group_select,], input$RadiusPlot_brush, xvar = "date_time") %>% filter(date_time == min_max(date_time, "max")) %>% distinct(date_time) %>% as.data.frame()
#       if(nrow(first_brush) == 0 | nrow(last_brush) == 0){
#         NULL
#       }else{
#         df10 = isolate(d$a)[isolate(d$a)[[".id"]] %in% input$one_by_one_group_select,] %>% filter(between(date_time, first_brush$date_time, last_brush$date_time))
#         }
#       }else{
#       first_brush = brushedPoints(isolate(d$a), input$RadiusPlot_brush, xvar = "date_time") %>% filter(date_time == min_max(date_time)) %>% distinct(date_time) %>% as.data.frame()
#       last_brush = brushedPoints(isolate(d$a), input$RadiusPlot_brush, xvar = "date_time") %>% filter(date_time == min_max(date_time, "max")) %>% distinct(date_time) %>% as.data.frame()
#       if(nrow(first_brush) == 0 | nrow(last_brush) == 0){
#         NULL
#       }else{
#         df10 = isolate(d$a) %>% filter(between(date_time, first_brush$date_time, last_brush$date_time))
#       }
#       }
#     if(!exists("df10")){
#       NULL
#     } else {
#       # change7----
#       # if(isFALSE(input$fine_mode)){
#       df10 = df10 %>% droplevels() %>% mutate(day = floor_date(date_time, "day")) %>% select(.id, day, input$variable_prim) %>% rename_with(~sub(paste0(input$variable_prim), paste0("levelup_", input$variable_prim), .x),everything()) %>%
#         dplyr::mutate(across(starts_with("levelup"), ~TRUE)) %>% distinct_all(.keep_all = T)
#       # }else{
#       #   df10 = df10 %>% droplevels() %>% mutate(day = date_time) %>% select(.id, day, input$variable_prim) %>% rename_with(~sub(paste0(input$variable_prim), paste0("levelup_", input$variable_prim), .x),everything()) %>%
#       #     dplyr::mutate(across(starts_with("levelup"), ~TRUE)) %>% distinct_all(.keep_all = T)
#       # }
#
#       if("cleaning_meta" %in% ls(envir = envir) | !is.null(cleaner_mode_rect$cleaning_meta)){
#         if(is.null(cleaner_mode_rect$cleaning_meta)){
#           cleaner_mode_rect$cleaning_meta = df10 %>% as.data.frame()
#           assign("cleaning_meta", df10, envir = envir)
#           cleaner_mode_rect$data_levelup = cleaner_mode_rect_fnct_levelup(cleaner_mode_rect$cleaning_meta)
#           rm(df10)
#         }else{
#           cleaner_mode_rect$cleaning_meta = isolate(cleaner_mode_rect$cleaning_meta) %>% full_join(df10, by = c(".id", "day")) %>% dplyr::mutate(across(ends_with(".x"), ~ replace(., get(paste0("levelup_", input$variable_prim, ".y")) == TRUE, TRUE))) %>%
#             dplyr::select(!ends_with(".y")) %>% dplyr::rename_all(~sub('.x', '', .x, fixed=T)) %>% mutate_if(is.logical, ~replace_na(., FALSE))
#           assign("cleaning_meta", cleaner_mode_rect$cleaning_meta, envir = envir)
#           cleaner_mode_rect$data_levelup =  cleaner_mode_rect_fnct_levelup(cleaner_mode_rect$cleaning_meta)
#           rm(df10)
#         }
#       }else{
#         cleaner_mode_rect$cleaning_meta = df10 %>% as.data.frame()
#         assign("cleaning_meta", df10, envir = envir)
#         cleaner_mode_rect$data_levelup = cleaner_mode_rect_fnct_levelup(cleaner_mode_rect$cleaning_meta)
#         rm(df10)
#       }
#     }
#   }
# })
observeEvent(input$Cleaner_Button_levelup_detect,{
  showModal(modalDialog(
    title = "Detect jumps.",
    "Detecting jumps over defined threshold.",
    numericInput("cleaner_levelup_threshold", "Jumps over:", value = 300, min = 100, max = 9000, step = 100),
    shinyWidgets::materialSwitch("cleaner_levelup_detect_all", "Process all selected data?", status = "primary",value = F),
    shinyWidgets::materialSwitch("accept_nas2_switch", "Across empty periods?", status = "primary",value = F),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("Cleaner_Button_levelup_detect_ok", "OK")
    )
  ))
})
observeEvent(input$Cleaner_Button_levelup_detect_ok,{
  if(isTRUE(input$cleaner_levelup_detect_all)){
    cleaner_mode_data_levelup = isolate(d$a)
  }else{
    cleaner_mode_data_levelup = isolate(d$a)[isolate(d$a)[[".id"]] %in% input$one_by_one_group_select,]
  }
  if(isTRUE(input$accept_nas2_switch)){
    cleaner_mode_data_levelup = cleaner_mode_data_levelup %>% mutate(nas = !!rlang::sym(input$variable_prim)) %>% tidyr::fill(!!rlang::sym(input$variable_prim), .direction = "downup")
  }else{
    cleaner_mode_data_levelup = cleaner_mode_data_levelup %>% mutate(nas = 1)
  }
  if(!exists("cleaner_mode_data_levelup")){
      NULL
    } else {
      cleaner_mode_data_levelup = cleaner_mode_data_levelup %>% dplyr::group_by(.id)%>%
        dplyr::mutate(Index_level2 = !!rlang::sym(input$variable_prim)-lag(!!rlang::sym(input$variable_prim))) %>%
        dplyr::mutate(Index_level2 = ifelse(abs(Index_level2) > input$cleaner_levelup_threshold | is.na(nas), TRUE, FALSE)) %>%
        filter(Index_level2 == TRUE) %>%
        # dplyr::rename_with(~paste0("levelup_",input$variable_prim), "outliers") %>%
        plyr::rename(., c("Index_level2" = paste0("levelup_",input$variable_prim))) %>%
        mutate(day = lubridate::floor_date(date_time, "day")) %>%
        select(.id, day, paste0("levelup_",input$variable_prim))%>%
        distinct(.keep_all = T) %>%
        as.data.frame()
      if("cleaning_meta" %in% ls(envir = envir) | !is.null(cleaner_mode_rect$cleaning_meta)){
        if(is.null(cleaner_mode_rect$cleaning_meta)){
          cleaner_mode_rect$cleaning_meta = cleaner_mode_data_levelup %>% as.data.frame()
          assign("cleaning_meta", cleaner_mode_data_levelup, envir = envir)
          cleaner_mode_rect$data_levelup = cleaner_mode_rect_fnct_levelup(cleaner_mode_rect$cleaning_meta)
          rm(cleaner_mode_data_levelup)
        }else{
          cleaner_mode_rect$cleaning_meta = isolate(cleaner_mode_rect$cleaning_meta) %>% full_join(cleaner_mode_data_levelup, by = c(".id", "day")) %>% dplyr::mutate(across(ends_with(".x"), ~ replace(., get(paste0("levelup_", input$variable_prim, ".y")) == TRUE, TRUE))) %>%
            dplyr::select(!ends_with(".y")) %>% dplyr::rename_all(~sub('.x', '', .x, fixed=T)) %>% mutate_if(is.logical, ~replace_na(., FALSE))
          assign("cleaning_meta", cleaner_mode_rect$cleaning_meta, envir = envir)
          cleaner_mode_rect$data_levelup =  cleaner_mode_rect_fnct_levelup(cleaner_mode_rect$cleaning_meta)
          rm(cleaner_mode_data_levelup)
        }
      }else{
        cleaner_mode_rect$cleaning_meta = cleaner_mode_data_levelup %>% as.data.frame()
        assign("cleaning_meta", cleaner_mode_data_levelup, envir = envir)
        cleaner_mode_rect$data_levelup = cleaner_mode_rect_fnct_levelup(cleaner_mode_rect$cleaning_meta)
        rm(cleaner_mode_data_levelup)
      }
    }
  removeModal()
})
observe({
      if(req(input$navbar) == "Data")
        plot_GS$active = FALSE
      d$c = NULL
    })
    #plot output function
    plot_appear_1 = function () {
      if (is.null(input$.id)|is.null(input$variable_prim)){
        empty_plot_1()
      } else {
        if (is.null(input$variable_sec)|isFALSE(input$sec_ax)){
          plot_data_plot()
        } else {
          plot_sec_axis_1()
        }
      }
    }
    plot_appear_2 = function () {
      if (is.null(input$.id)|is.null(input$variable_prim)){
        empty_plot_2()
      } else {
        if(isTRUE(input$freeze_switch) & input$variable_prim == "Radius"){
          plot_sec_axis_2()
        } else {
          if(plot_GS$active & input$lower_plot_switch){
            plot_prim_up_GS()+
              theme(legend.position = "none")
          } else {
            if(isTRUE(input$cleaner_mode_switch)){
              plot_prim_cleaner_mode()+
                theme(legend.position = "none")
            }else{
            if (is.null(input$variable_sec)|isFALSE(input$sec_ax)|isTRUE(input$bar4)){
              plot_upper_plot()+
                theme(legend.position = "none")
            } else {
              plot_sec_axis_2()
            }
          }
        }
      }
    }
    }
    plot_appear_3 = function () {
      if (is.null(input$.id)|is.null(input$variable_prim_lower)){
        empty_plot_2()
      } else {
        if(plot_GS$active){
          plot_prim_down_GS()+
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank())
        } else {
          if (is.null(input$variable_sec)|isFALSE(input$sec_ax)|isTRUE(input$bar4)){
            plot_lower_plot()+
              theme(legend.position = "none")
          } else {
            plot_sec_axis_3()
          }
        }
      }
    }
    legend_appear_2 <- function() {
      p <- upper_plot_obj()
      legend <- cowplot::get_legend(p + theme(legend.position = "top"))
      grid::grid.newpage()
      grid::grid.draw(legend)
      legend
    }
    # Plot_output ----
    #input data plot
    output$Data_tab_plot <- renderPlot({
      input$goButton
      isolate(plot_appear_1())
    })
    # changes6----
    #plot_page
    # RadiusPlot2_activator = reactiveValues(active = FALSE)
    # observeEvent(input$Upperplot,{
    #   if(input$Upperplot%%2 == 0){
    #     RadiusPlot2_activator$active = TRUE
    #   }else(
    #     RadiusPlot2_activator$active = FALSE
    #   )
    # })
    # output$Plot_tab_upper_plot <- renderPlot({
    #   req(input$navbar) == "Plot"
    #   RadiusPlot2_activator$active
    #   isolate(plot_appear_2())
    #   #plot_zooming()
    # })
    output$Plot_tab_upper_plot <- renderPlot({
      req(input$navbar == "Plot")
      p <- upper_plot_obj()
      p + theme(legend.position = "none")
    })
    output$Plot_tab_lower_plot <- renderPlot({
      plot_appear_3()
      #plot_zooming()

    })
    #legend outputs ----
    output$legend_sizable <- renderUI({
      conditionalPanel(condition = "input.legend_switch == true",plotOutput('Legend2', height = 40 * legend_rows()))
    })
    output$Legend2 <- renderPlot({
      req(input$navbar == "Plot", isTRUE(input$legend_switch))
      p <- upper_plot_obj()
      # Use get_legend (or your original get_plot_component line)
      legend <- cowplot::get_legend(p + theme(legend.position = "top"))
      grid::grid.newpage()
      grid::grid.draw(legend)
    })
    outputOptions(output, "Legend2", suspendWhenHidden = TRUE)
    output$export_plot_btn <- downloadHandler(
      filename = function (){"Shinyplot.png"},
      content = function(file) {
        ggplot2::ggsave(file, plot_export_funct(), device = "png" ,width = 45, height = height_funct(), units = "cm", dpi = 300, bg = "white")
      })
    #Data_manipulation ----
    data_export_funct = function(){
      if(is.null(input$RadiusPlot_brush) && isTRUE(input$one_by_one_switch)){
        isolate(d$a)
      } else {
        if(is.null(input$RadiusPlot_brush) && isFALSE(input$one_by_one_switch)){
          isolate(d$a)[isolate(d$a)[[".id"]] %in% input$one_by_one_group_select,]
        } else {
          if(isFALSE(input$one_by_one_switch)){
            brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time")
          }else{
            brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time")
          }
        }
      }
    }
    output$bar2 <- downloadHandler(
      filename = function (){paste("Shinydata_", Sys.Date(), ".csv", sep="")},
      content = function(file2) {
        shiny::withProgress(
          message = paste0("Downloading Dataset"),
          value = 0,
          {
            shiny::incProgress(5/10)
            write.table(data_export_funct(),file2, sep =';',dec = '.',row.names = F)
            shiny::incProgress(5/10)
          }
        )
      })
    # Deselect brushed data----
    brushed_ids = function(){
      if(isFALSE(input$one_by_one_switch)){
        ids = brushedPoints(isolate(d$a)[isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time")
      }else{
        ids = brushedPoints(isolate(d$a), input$RadiusPlot_brush, xvar = "date_time")
      }
      ids = droplevels(ids)
      return(levels(ids$.id))
    }
    observeEvent(input$bar44,{
      showModal(jqui_draggable(modalDialog(
        selectInput("deselect_brush",
                    "Deselect brushed data?",
                    choices = c("Deselect", "Remove from data frame"),
                    multiple = F,
                    selectize = F,
                    selected = c("Deselect")),
                    renderText(paste0("IDs:", brushed_ids())),
        easyClose = T,
        footer = tagList(
          actionButton("ok_bar18", "OK"),
          modalButton("Cancel")
        ),
        size = "m"
      )))
    })
    observeEvent(input$ok_bar18,{
      req(!is.null(brushed_ids()))
      one_selected = input$.id[! input$.id %in% brushed_ids()][1]
      id_selected = input$.id[!input$.id %in% brushed_ids()]
      id_choices = d$a %>% dplyr::filter(!.id %in% brushed_ids()) %>% droplevels() %>% dplyr::distinct(.id) %>% pull(.id)
      if(input$deselect_brush == "Deselect"){
        updateSelectInput(session, "one_by_one_group_select",
                          choices = input$.id[! input$.id %in% brushed_ids()],
                          selected = one_selected)
        updateCheckboxGroupInput(session, ".id",
                                 selected = id_selected)
      }else{
        df = df %>% dplyr::filter(!.id %in% brushed_ids()) %>% droplevels() %>% as.data.frame()
        assign("df", df, envir = envir)
        d$b = df
        updateCheckboxGroupInput(session, ".id",
                                 choices = id_choices,
                                 selected = id_selected)
      }
      removeModal()
      })
    #subset - plot tab----
    observeEvent(input$bar11, {
      showModal(modalDialog(
        title = "Subset data?",
        HTML("Would you like to continue with shown data only?<br>
             This function is not comaptible with One_by_one selection"),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok_bar6", "OK")
        )
      ))
    })
    observeEvent(input$ok_bar6, {
      shiny::withProgress(
        message = paste0("Processing..."),
        value = 0,
        {
          shiny::incProgress(3/10, detail = "data backup")
      save(list= 'df', file = paste(Sys.Date(),'.RData', sep=''))
      if(is.null(zooming$x)){
        df = isolate(d$a)
        df = droplevels(df)
        shiny::incProgress(3/10, detail = "assigning data")
        assign('df', df, envir = envir)
      } else {
        df = subset(isolate(d$a),date_time >= zooming$x[1] & date_time <= zooming$x[2])
        df = droplevels(df)
        assign('df', df, envir = envir)
      }
      shiny::incProgress(4/10, detail = "done")
      d$b <- df
      removeModal()
        })
    })
    #subset - data tab----
    observeEvent(input$bar17, {
      showModal(modalDialog(
        title = "Subset data?",
        HTML("Would you like to continue with selected ID's (.id) and time period only?"),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok_bar8", "OK")
        )
      ))
    })
    observeEvent(input$ok_bar8, {
      shiny::withProgress(
        message = paste0("Processing..."),
        value = 0,
        {
        shiny::incProgress(3/10, detail = "data backup")
      save(list= 'df', file = paste(Sys.Date(),'.RData', sep=''))
      df = isolate(d$a)
      df = droplevels(df)
      shiny::incProgress(3/10, detail = "assigning data")
      assign('df', df, envir = envir)
      d$b <- df
      removeModal()
      shiny::incProgress(4/10, detail = "done")
      updateCheckboxInput(session, "bar", value = F)
        })
    })
    observeEvent(input$bar45, {
      showModal(modalDialog(
        title = "Drop variable?",
        HTML("Would you like to delete variable from data?"),
        selectInput("input45", "Drop variable:", choices = names(df)[sapply(df, is.numeric)], selected = NULL, selectize = F),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok_bar45", "OK")
        )
      ))
    })
    observeEvent(input$ok_bar45, {
      shiny::withProgress(
        message = paste0("Processing..."),
        value = 0,
        {
          shiny::incProgress(3/10, detail = "removing variable")
          df = df %>% dplyr::select(-all_of(input$input45))
          shiny::incProgress(3/10, detail = "assigning data")
          assign('df', df, envir = envir)
          d$b <- df
          removeModal()
          shiny::incProgress(4/10, detail = "done")
        })
    })
    #output$dynamicInput4 <- renderUI({
    # if(isTRUE(input$bar4)){
    #   actionButton("bar5", "Delete")
    # } else {
    #   return(NULL)
    # }
    #})
    observeEvent(input$bar5, {
      showModal(modalDialog(
        title = "Delete data!",
        "Are you sure to delete selected data?",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok_bar5", "OK")
        )
      ))
    })
    observeEvent(input$ok_bar5, {
      shiny::withProgress(
        message = paste0("Processing..."),
        value = 0,
        {
          shiny::incProgress(2/10)
          #save(list= 'df', file = paste(Sys.Date(),'.RData', sep=''))
          shiny::incProgress(2/10)
          if(isFALSE(input$one_by_one_switch)){
            r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
          }else{
            r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
          }
          df[r,input$variable_prim] <- NA
          shiny::incProgress(2/10)
          # df = df %>% dplyr::arrange(.id, date_time) %>% group_by(.id) %>% filter_if(is.numeric, ~lead_trail_nas_filter(.)) %>% as.data.frame()
          assign('df', df, envir = envir)
          shiny::incProgress(2/10)
          d$b <- df
          removeModal()
          shiny::incProgress(2/10)
        })
    })
    offst_upper  = function() {
      if(!is.null(input$RadiusPlot_brush)){
        brush_min = input$RadiusPlot_brush$xmin
        # as.POSIXct(brush$xmin,origin = "1970-01-01 00:00:00 UTC")
        dat4_upper = isolate(d$a) %>% group_by(.id) %>% arrange(date_time, .by_group = TRUE) %>%
          dplyr::rename(newVar = input$variable_prim) %>%
          mutate(newVar = newVar - first(newVar[date_time >= brush_min[1] & !is.na(newVar)], default = NA_real_)) %>%
          as.data.frame()
        names(dat4_upper)[names(dat4_upper) == "newVar"] <- paste(input$variable_prim)
        d$a <- dat4_upper
        rm(dat4_upper)
        return(d$a)
      }else{
        if(is.null(zooming$x)){
          dat4_upper = isolate(d$a) %>% group_by(.id) %>% arrange(date_time, .by_group = TRUE) %>% mutate(newVar = !!rlang::sym(input$variable_prim)-first(na.omit(!!rlang::sym(input$variable_prim)))) %>% select(-!!rlang::sym(input$variable_prim)) %>% as.data.frame()
          names(dat4_upper)[names(dat4_upper) == "newVar"] <- paste(input$variable_prim)
          d$a <- dat4_upper
          rm(dat4_upper)
          return(d$a)
        } else {
          dat4_upper = isolate(d$a) %>% group_by(.id) %>% arrange(date_time, .by_group = TRUE) %>%
            dplyr::rename(newVar = input$variable_prim) %>%
            mutate(newVar = newVar - first(newVar[date_time >= zooming$x[1] & !is.na(newVar)], default = NA_real_)) %>%
            as.data.frame()
          names(dat4_upper)[names(dat4_upper) == "newVar"] <- paste(input$variable_prim)
          d$a <- dat4_upper
          rm(dat4_upper)
          return(d$a)
        }
      }
    }
    offst_lower  = function() {
      if(!is.null(input$RadiusPlot_brush)){
        brush_min = input$RadiusPlot_brush$xmin
        dat4_lower = isolate(d$a) %>% group_by(.id) %>% arrange(date_time, .by_group = TRUE) %>%
          dplyr::rename(newVar = input$variable_prim_lower) %>%
          mutate(newVar = newVar - first(newVar[date_time >= brush_min[1] & !is.na(newVar)], default = NA_real_)) %>%
          as.data.frame()
        names(dat4_lower)[names(dat4_lower) == "newVar"] <- paste(input$variable_prim_lower)
        d$a <- dat4_lower
        rm(dat4_lower)
        return(d$a)
      }else{
        if(is.null(zooming$x)){
          dat4_lower = isolate(d$a) %>% group_by(.id) %>% arrange(date_time, .by_group = TRUE) %>% mutate(newVar = !!rlang::sym(input$variable_prim_lower)-first(na.omit(!!rlang::sym(input$variable_prim_lower)))) %>% select(-!!rlang::sym(input$variable_prim_lower)) %>% as.data.frame()
          names(dat4_lower)[names(dat4_lower) == "newVar"] <- paste(input$variable_prim_lower)
          d$a <- dat4_lower
          rm(dat4_lower)
          return(d$a)
        } else {
          dat4_lower = isolate(d$a) %>% group_by(.id) %>% arrange(date_time, .by_group = TRUE) %>%
            dplyr::rename(newVar = input$variable_prim_lower) %>%
            mutate(newVar = newVar - first(newVar[date_time >= zooming$x[1] & !is.na(newVar)], default = NA_real_)) %>%
            as.data.frame()
          names(dat4_lower)[names(dat4_lower) == "newVar"] <- paste(input$variable_prim_lower)
          d$a <- dat4_lower
          rm(dat4_lower)
          return(d$a)
        }
      }
    }
    observeEvent(input$subtract_upper_plot, {
      if(length(levels(d$a[['.id']]))>1 && isTRUE(input$one_by_one_switch)){
        showModal(modalDialog(
          'Note that any change in Data tab will reset offset.',
          easyClose = T,
          footer = NULL
        ))
        offst_upper()
      } else {
        showModal(modalDialog(
          'Plot more than one sensor.',
          easyClose = T,
          footer = NULL
        ))
      }
    })
    observeEvent(input$subtract_lower_plot, {
      if(length(levels(d$a[['.id']]))>1 && isTRUE(input$one_by_one_switch)){
        showModal(modalDialog(
          'Note that any change in Data tab will reset offset.',
          easyClose = T,
          footer = NULL
        ))
        offst_lower()
      } else {
        showModal(modalDialog(
          'Plot more than one sensor.',
          easyClose = T,
          footer = NULL
        ))
      }
    })
    observeEvent(input$bar39,{
      d$a = d$b[which(d$b[['date_time']] >= input$period[1] & d$b[['date_time']] <= input$period[2] & d$b[['.id']] %in% input$.id),]

    })

    stat_function = function(){
      if(is.null(zooming$x) && is.null(input$RadiusPlot_brush) && isTRUE(input$one_by_one_switch)){
        dat5 = isolate(d$a)
      } else {
        if(is.null(zooming$x) && is.null(input$RadiusPlot_brush) && isFALSE(input$one_by_one_switch)){
          dat5 = isolate(d$a)[isolate(d$a)[[".id"]] %in% input$one_by_one_group_select,]
        } else {
          if(isFALSE(input$one_by_one_switch)){
            res <- brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time")
            dat5 = isolate(res)
          } else {
            res <- brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time")
            dat5 = isolate(res)
          }
        }
      }
      dat5 <- Filter(function(x){!all(is.na(x))}, dat5)
      statistics = dat5 %>%
        group_by(across(input$group_stat)) %>%
        # summarise_if(is.numeric, list(obs = ~sum(!is.na(.)), avg = ~mean(.), stdev = ~sd(.), min = ~min(.), max = ~max(.)), na.rm = T)
        summarise_if(is.numeric, list(obs = ~sum(!is.na(.)), avg =mean, stdev = sd, min = min, max = max), na.rm = T)
      return(statistics)
    }
    stat_choices = function(){
      ch <- colnames(Filter(is.factor, select_if(d$a,function(x){!all(is.na(x))})))
      return(ch)
    }
    autolevelup = function (){
      shiny::withProgress(
        message = paste0("Processing ..."),
        value = 0,
        {
          shiny::incProgress(1/10, detail = "selecting data")
          if(isFALSE(input$one_by_one_switch) & isFALSE(input$bar37)){
            df2 = isolate(d$a)[isolate(d$a)[[".id"]] %in% input$one_by_one_group_select,]
          }else{
            df2 = isolate(d$a)
          }
          #week searching and data filter
          shiny::incProgress(1/10, detail = "rough filter")
          if(isTRUE(input$accept_nas)){
            df2 = df2 %>% mutate(nas = Radius) %>% tidyr::fill(Radius, .direction = "downup")
          }else{
            df2 = df2 %>% mutate(nas = 1)
          }
          shiny::incProgress(1/10, detail = "searching")
          #searching differences
          df2 = df2 %>% dplyr::group_by(.id)%>%
            dplyr::mutate(Index_level2 = Radius-lag(Radius)) %>%
            dplyr::mutate(Index_level2 = ifelse(abs(Index_level2) < input$bar27, NA, Index_level2))%>%
            mutate(Index_level2 =  cumsum(replace_na(Index_level2,0))) %>%
            mutate(Index_level2 = na_if(Index_level2,0)) %>%
            mutate(Radius = ifelse(is.na(Index_level2), Radius, Radius - Index_level2)) %>%
            mutate(Radius = ifelse(is.na(nas), NA, Radius))  %>% select(.id, date_time, Radius)%>%
            as.data.frame()
          #data.frame manipulation
          shiny::incProgress(1/10, detail = "merging data")
          #assign to global environment
          df = left_join(df, df2, by = c('.id', 'date_time')) %>% dplyr::mutate(Radius.x = ifelse(is.na(Radius.y), Radius.x, Radius.y)) %>%
            dplyr::rename_all(~sub('.x', '', .x, fixed=T)) %>% dplyr::select(paste(colnames(df)))
          rm(df2)
          df = as.data.frame(df %>% dplyr::group_by(.id) %>% dplyr::arrange(date_time, .by_group = T))
          shiny::incProgress(2/10, detail = "assigning data")
          assign('df', df, envir=envir)
          shiny::incProgress(2/10, detail = "done")
        })}
    observeEvent(input$bar8, {
      if(input$variable_prim != 'Radius' & any(is.null(input$variable_sec), input$variable_sec != 'Radius')){
        showModal(modalDialog(
          title = "Radius data auto-level-up!",
          "Select Radius as variable for primary or secondary y-axis",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }else{
        showModal(modalDialog(
          title = "Radius data auto level-up!",
          "Rendered data of Radius should be automatically leveled-up.",
          numericInput("bar27", "Level over:", value = 1000, min = 100, max = 9000, step = 100),
          # selectInput("bar28", "Search within:", choices = c("week", "day"), selected = "week"),
          shinyWidgets::materialSwitch("bar37", "Process all selected data?", status = "primary",value = F),
          shinyWidgets::materialSwitch("accept_nas", "Across empty periods?", status = "primary",value = F),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok_bar12", "OK")
          )
        ))
      }
    })
    observeEvent(input$ok_bar12, {
      autolevelup()
      d$b <- df
      removeModal()
    })
    manualmoveup_part1 = function (){
      if(isFALSE(input$one_by_one_switch)){
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
      }else{
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
      }
      df3 = isolate(df[r,])
      df3 = droplevels(df3)
      df3 = df3 %>% arrange(date_time) %>% dplyr::rename(man_lev_var = input$variable_prim)
      first =  head(df3,1)
      last  = tail(df3,1)
      diff = last(df3$man_lev_var)-first(df3$man_lev_var)
      return(diff)
    }
    manualmoveup_part2 = function (){
      if(isFALSE(input$one_by_one_switch)){
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
      }else{
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
      }
      df3 = isolate(df[r,])
      if(is.null(input$RadiusPlot_brush) | length(levels(droplevels(df3$.id)))!= 1){
        showModal(modalDialog(
          title = "Radius data manual move-up!",
          HTML('Brush data first!<br>
               Brush data from only one device!'),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }else{
        df3 = df3 %>% arrange(date_time) %>% dplyr::rename(man_lev_var = input$variable_prim)
        #data.frame manipulation
        df3 = as.data.frame(df3 %>% mutate(man_lev_var = man_lev_var-isolate(-1*input$moveupInput_div)))
        #assign to global environment
        df = left_join(df, df3, by = c('.id', 'date_time')) %>% mutate(man_lev_var = ifelse(!is.na(man_lev_var), man_lev_var,!!rlang::sym(input$variable_prim))) %>%
          dplyr::select(-c(!!rlang::sym(input$variable_prim),ends_with('.y'))) %>% rename_all(~sub('.x', '', .x, fixed=T))
        df = plyr::rename(df, c("man_lev_var" = input$variable_prim))
        df = as.data.frame(df %>% group_by(.id) %>% arrange(date_time, .by_group = T))
        assign('df', df, envir=envir)
      }
    }
    diff = reactiveValues(val = 0)
    observeEvent(input$bar16, {
      if(is.null(input$variable_prim) | is.null(input$.id)) {
        NULL
      } else {
        if(isFALSE(input$one_by_one_switch)){
          r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
        }else{
          r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
        }
        df3 = isolate(df[r,])
        df3 = droplevels(df3)
        if(is.null(input$RadiusPlot_brush) | length(levels(df3$.id))!= 1){
          showModal(modalDialog(
            title = "Data manual move-up!",
            HTML('Brush data first!<br>
               Brush data from only one device!'),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        }else{
          diff$val = manualmoveup_part1()
          session$resetBrush("RadiusPlot_brush")
          shinyjs::show("moveupInput")
          updateNumericInput(session, inputId = "moveupInput_div", label = "Selected data will be moved by:", value = round(isolate(diff$val * -1),3), step = 1)
          shinyjs::show("moveupInput_div")
          # output$moveupInput_div <- renderText({HTML("Selected data will be moved by: ",
          #                                            paste0(as.character(round(isolate(diff$val * -1),3))))
          # })
        }
      }
    })
    observeEvent(input$moveupInput, {
      if(is.null(input$variable_prim) | is.null(input$.id)) {
        NULL
      } else {
        manualmoveup_part2()
        session$resetBrush("RadiusPlot_brush")
        d$b <- df
      }
      shinyjs::hide("moveupInput")
      shinyjs::hide("moveupInput_div")
    })
    manuallevelup = function (){
      if(isFALSE(input$one_by_one_switch)){
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
      }else{
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
      }
      df3 = isolate(df[r,])
      df3$.id = droplevels(df3$.id)
      if(is.null(input$RadiusPlot_brush) | length(levels(df3$.id))!= 1){
        showModal(modalDialog(
          title = "Radius data manual level-up!",
          HTML('Brush data first!<br>
               Brush data from only one device!'),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }else{
        df3 = df3 %>% arrange(date_time) %>% dplyr::rename(man_lev_var = input$variable_prim)
        first =  head(df3,1)
        last  = tail(df3,1)
        diff = last(df3$man_lev_var)-first(df3$man_lev_var)
        #data.frame manipulation
        df3 = as.data.frame(df %>% filter(.id == levels(df3$.id)) %>% dplyr::rename(man_lev_var = input$variable_prim) %>%
                              mutate(man_lev_var = ifelse(between(date_time, first$date_time, last$date_time), first$man_lev_var, ifelse(date_time > last$date_time, man_lev_var-diff, man_lev_var))))
        #assign to global environment
        df = left_join(df, df3, by = c('.id', 'date_time')) %>% mutate(man_lev_var = ifelse(is.na(man_lev_var), !!rlang::sym(input$variable_prim), man_lev_var)) %>%
          dplyr::select(-c(!!rlang::sym(input$variable_prim),ends_with('.y'))) %>% rename_all(~sub('.x', '', .x, fixed=T))
        df = plyr::rename(df, c("man_lev_var" = input$variable_prim))
        df = as.data.frame(df %>% group_by(.id) %>% arrange(date_time, .by_group = T))
        assign('df', df, envir=envir)
      }
    }
    observeEvent(input$bar9, {
      if(is.null(input$variable_prim) | is.null(input$.id)) {
        NULL
      } else {
        manuallevelup()
        d$b <- df
      }
    })
    manualInterpolation = function (){
      if(isFALSE(input$one_by_one_switch)){
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
      }else{
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
      }
      df3 = isolate(df[r,])
      df3 = droplevels(df3)
      if(is.null(input$RadiusPlot_brush) | length(levels(df3$.id))!= 1){
        showModal(modalDialog(
          title = "Radius data manual interpolation!",
          HTML('Brush data first!<br>
               Brush data from only one device!'),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }else{
        df3 = df3 %>% arrange(date_time) %>% dplyr::rename(man_int_var = input$variable_prim)
        first = head(df3,1)
        last  = tail(df3,1)
        #put there data between first and last
        df3 = as.data.frame(df %>% filter(.id %in% levels(df3$.id) & between(date_time, first$date_time, last$date_time)))
        #data.frame manipulation
        df3 = as.data.frame(df3 %>% mutate(man_int_var = NA_real_) %>% select(-input$variable_prim))
        df3$man_int_var[1] = first$man_int_var
        df3$man_int_var[length(df3$man_int_var)] = last$man_int_var
        df3 = as.data.frame(df3 %>% mutate(man_int_var = na.approx(man_int_var)))
        #assign to global environment
        df = left_join(df, df3, by = c('.id', 'date_time')) %>% mutate(man_int_var = ifelse(!is.na(man_int_var), man_int_var,!!rlang::sym(input$variable_prim))) %>%
          dplyr::select(-c(!!rlang::sym(input$variable_prim),ends_with('.y'))) %>% rename_all(~sub('.x', '', .x, fixed=T))
        df = plyr::rename(df, c("man_int_var" = input$variable_prim))
        df = as.data.frame(df %>% group_by(.id) %>% arrange(date_time, .by_group = T))
        assign('df', df, envir=envir)
      }
    }
    observeEvent(input$bar15, {
      if(is.null(input$variable_prim) | is.null(input$.id)) {
        NULL
      } else {
        manualInterpolation()
        d$b <- df
      }
    })

    observeEvent(input$bar7, {
      showModal(jqui_draggable(modalDialog(
        selectInput("group_stat",
                    "Group by",
                    choices = stat_choices(),
                    multiple = T,
                    selectize = F,
                    selected = stat_choices()[1]),
        renderDataTable({
          stat_function()
        },options=list(scrollX=TRUE)),
        easyClose = T,
        footer = modalButton("OK"),
        size = "l",
        downloadButton("download_stat", "Save as .csv")
      )))
    })
    output$download_stat <- downloadHandler(
      filename = function (){paste("ShinyStat_", Sys.Date(), ".csv", sep="")},
      content = function(file3) {
        write.table(stat_function(),file3, sep =';',dec = '.',row.names = F)
      })
    subtractoffset = function (){
      if(isFALSE(input$one_by_one_switch)){
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
      }else{
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
      }
      df3 = isolate(df[r,])
      df3 = droplevels(df3)
      if(is.null(input$RadiusPlot_brush) | length(levels(df3$.id))<=1){
        showModal(modalDialog(
          title = "Subtract offset on primary axis!",
          HTML('Brush data first!<br>
               Brush data from more than one device!'),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }else{
        shiny::withProgress(
          message = paste0("Processing..."),
          value = 0,
          {
            shiny::incProgress(2/10)
            df4 = data.frame()
            df3 = df3 %>% arrange(date_time)
            if(input$bar22 == "mean") {
              offset1 = as.data.frame(df3 %>% group_by(.id) %>% summarise(x = mean(na.omit(!!rlang::sym(input$variable_prim)))) %>% ungroup() %>% summarise(avg = mean(na.omit(x))))
              offset1 = as.data.frame(df3 %>% group_by(.id) %>% summarise(x = mean(na.omit(!!rlang::sym(input$variable_prim)))-offset1$avg))
            } else {
              if(input$bar22 == "median"){
                offset1 = as.data.frame(df3 %>% group_by(.id) %>% summarise(x = mean(na.omit(!!rlang::sym(input$variable_prim)))) %>% ungroup() %>% summarise(med = median(na.omit(x))))
                offset1 = as.data.frame(df3 %>% group_by(.id) %>% summarise(x = mean(na.omit(!!rlang::sym(input$variable_prim)))-offset1$med))
              }else{
                offset1 = data.frame(val = as.numeric(input$offset_val))
                offset1 = as.data.frame(df3 %>% group_by(.id) %>% summarise(x = mean(na.omit(!!rlang::sym(input$variable_prim)))-offset1$val))
              }
            }
            #data.frame manipulation
            shiny::incProgress(2/10)
            for(i in 1:length(levels(df3$.id))){
              c = as.data.frame(df %>% filter(.id == levels(df3$.id)[i]) %>% mutate(newVar = !!rlang::sym(input$variable_prim)-offset1$x[i]))
              df4 = rbind(df4, c)
            }
            shiny::incProgress(3/10)
            df4 = df4 %>% select(-!!rlang::sym(input$variable_prim))
            names(df4)[names(df4) == "newVar"] <- paste(input$variable_prim)
            #assign to global environment
            df = df %>% filter(!.id %in% levels(df3$.id))
            df = rbind(df, df4)
            df = as.data.frame(df %>% group_by(.id) %>% arrange(date_time, .by_group = T))
            assign('df', df, envir=envir)
            shiny::incProgress(3/10)
          })
      }
    }
    offset_mult_val = function (){
      if(isFALSE(input$one_by_one_switch)){
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
      }else{
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
      }
      df3 = isolate(df[r,])
      df3 = droplevels(df3)
      df3 = df3 %>% arrange(.id, date_time)
      offset1 = as.data.frame(df3 %>% group_by(.id) %>% select(group_cols(), input$bar23) %>%
                                summarise_all(list(~mean(., na.rm = T))) %>% ungroup() %>% select(-.id) %>%
                                summarise_all(list(~mean(., na.rm = T))))
      offset2 = as.data.frame(df3 %>% group_by(.id) %>% select(group_cols(), input$bar23) %>%
                                summarise_all(list(~mean(., na.rm = T))) %>% ungroup() %>% select(-.id) %>%
                                summarise_all(list(~median(., na.rm = T))))
      offset1 = melt(offset1, value.name = "mean",id.vars=NULL)
      offset2 = melt(offset2, value.name = "median",id.vars=NULL)
      offset = format(left_join(offset1, offset2, by = "variable"),digits = 2, nsmall = 2)
      return(offset)
    }
    subMultOffset_part1 = function (){
      if(isFALSE(input$one_by_one_switch)){
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
      }else{
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
      }
      df3 = isolate(df[r,])
      df3 = droplevels(df3)
      if(is.null(input$RadiusPlot_brush) | length(levels(df3$.id))<=1){
        showModal(modalDialog(
          title = "Subtract offset on primary axis!",
          HTML('Brush data first!<br>
               Brush data from more than one device!'),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }else{
        #select method
        showModal(modalDialog(
          title = "Select method!",
          br(),
          renderDataTable(
            offset_mult_val(), server = FALSE,
            options = list(autoWidth = TRUE, ordering = F, searching = F, paging = F),
            rownames= FALSE
          ),
          br(),
          selectInput("bar24", "", choices = c("mean", "median")),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok_bar11", "OK")
          )
        ))
      }
    }
    subMultOffset_part2 = function (){
      if(isFALSE(input$one_by_one_switch)){
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
      }else{
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
      }
      df3 = isolate(df[r,])
      df3 = droplevels(df3)
      if(is.null(input$RadiusPlot_brush) | length(levels(df3$.id))<=1){
        showModal(modalDialog(
          title = "Subtract offset on primary axis!",
          HTML('Brush data first!<br>
               Brush data from more than one device!'),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }else{
        shiny::withProgress(
          message = paste0("Processing..."),
          value = 0,
          {
            shiny::incProgress(2/10)
            df3 = df3 %>% arrange(.id, date_time)
            if(input$bar24 == "mean") {
              offset1 = as.data.frame(df3 %>% group_by(.id) %>% select(group_cols(), input$bar23) %>%
                                        summarise_all(list(~mean(.))) %>% ungroup() %>% select(-.id) %>%
                                        summarise_all(list(~mean(.))))
              offset2 = as.data.frame(df3 %>% group_by(.id) %>% select(group_cols(), input$bar23) %>%
                                        summarise_all(list(~mean(.))))
              offset3 = cbind(offset2[1],round(offset2[-1]-offset1[rep(seq_len(nrow(offset1)),nrow(offset2)), ],3))
            } else {
              offset1 = as.data.frame(df3 %>% group_by(.id) %>% select(group_cols(), input$bar23) %>%
                                        summarise_all(list(~mean(.))) %>% ungroup() %>% select(-.id) %>%
                                        summarise_all(list(~median(.))))
              offset2 = as.data.frame(df3 %>% group_by(.id) %>% select(group_cols(), input$bar23) %>%
                                        summarise_all(list(~mean(.))))
              offset3 = cbind(offset2[1],round(offset2[-1]-offset1[rep(seq_len(nrow(offset1)),nrow(offset2)), ],3))
            }
            #data.frame manipulation
            shiny::incProgress(3/10)
            df3 = as.data.frame(df %>% filter(.id %in% levels(df3$.id)))
            df3 = droplevels(df3)
            df3 = cbind(select(df3,-input$bar23), select(df3,input$bar23)-offset3[match(df3$.id, offset3$.id),-1])
            shiny::incProgress(3/10)
            #assign to global environment
            df = df %>% filter(!.id %in% levels(df3$.id))
            df = rbind(df, df3)
            df = as.data.frame(df %>% group_by(.id) %>% arrange(date_time, .by_group = T))
            assign('df', df, envir=envir)
            shiny::incProgress(2/10)
          })
      }
    }
    observeEvent(input$bar10, {
      if(is.null(input$variable_prim) | is.null(input$.id)) {
        NULL
      } else {
        showModal(modalDialog(
          title = "Select method!",
          shinyWidgets::materialSwitch("multiple_switch","Subtract offset across multiple variables", status = "info", value = FALSE),
          br(),
          conditionalPanel(condition = "input.multiple_switch == false",
                           selectInput("bar22", "", choices = c("mean", "median", "value"))
          ),
          conditionalPanel(condition = "input.bar22 == 'value'",
                           numericInput("offset_val", "", value = 0, step = 1)
          ),
          conditionalPanel(condition = "input.multiple_switch == true",
                           selectInput("bar23", "Select variables", choices = f(), selected = NULL, multiple = T)
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok_bar10", "OK")
          )
        ))
      }
    })
    observeEvent(input$ok_bar10, {
      if(is.null(input$variable_prim) | is.null(input$.id)) {
        NULL
      } else {
        if(isTRUE(input$multiple_switch)){
          removeModal()
          subMultOffset_part1()
        } else {
          removeModal()
          subtractoffset()
          d$b <- df
        }
      }
    })
    observeEvent(input$ok_bar11, {
      if(is.null(input$variable_prim) | is.null(input$.id)) {
        NULL
        removeModal()
      } else {
        subMultOffset_part2()
        d$b <- df
        removeModal()
      }
    })
    # subtractTMSoffset = function (){
    #   if(isFALSE(input$one_by_one_switch)){
    #     r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
    #   }else{
    #     r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
    #   }
    #   df3 = isolate(df[r,])
    #   df3 = droplevels(df3)
    #   if(is.null(input$RadiusPlot_brush) | length(levels(df3$.id))<=1){
    #     showModal(modalDialog(
    #       title = "Subtract offset on primary axis!",
    #       HTML('Brush data first!<br>
    #            Brush data from more than one device!'),
    #       easyClose = TRUE,
    #       footer = modalButton("OK")
    #     ))
    #   }else{
    #     shiny::withProgress(
    #       message = paste0("Processing..."),
    #       value = 0,
    #       {
    #         shiny::incProgress(2/10)
    #         df4 = data.frame()
    #         df3 = df3 %>% arrange(date_time)
    #         offset_avg = as.data.frame(df3 %>% group_by(.id) %>% summarise(x = mean(na.omit(!!rlang::sym(input$variable_prim)))) %>% ungroup() %>% summarise(avg = mean(na.omit(x))))
    #         maximum_avg = as.data.frame(df %>% filter(.id %in% levels(df3$.id)) %>% droplevels() %>% group_by(.id) %>% summarise_at(input$variable_prim, list(max = max)) %>% ungroup() %>% summarise(avg_max = mean(na.omit(max))))
    #         maximum = as.data.frame(df %>% filter(.id %in% levels(df3$.id)) %>% droplevels() %>% group_by(.id) %>% summarise_at(input$variable_prim, list(max = max)))
    #         shiny::incProgress(3/10)
    #         if(isTRUE(input$bar13)){
    #           for(i in 1:length(levels(df3$.id))){
    #             offset1 = as.data.frame(df3 %>% filter(.id == levels(df3$.id)[i]) %>% group_by(.id) %>% summarise(a = (mean(na.omit(!!rlang::sym(input$variable_prim)))-maximum$max[i])/(offset_avg$avg-maximum_avg$avg_max), b = mean(na.omit(!!rlang::sym(input$variable_prim)))-offset_avg$avg*a))
    #             c = as.data.frame(df %>% filter(.id == levels(df3$.id)[i]) %>% mutate(newVar = (!!rlang::sym(input$variable_prim)-offset1$b)/offset1$a))
    #             df4 = rbind(df4, c)
    #           }
    #         }else{
    #           offset1 = as.data.frame(df3 %>% group_by(.id) %>% summarise(a = (mean(na.omit(!!rlang::sym(input$variable_prim)))-56.99156)/(offset_avg$avg-56.99156), b = mean(na.omit(!!rlang::sym(input$variable_prim)))-offset_avg$avg*a))
    #           for(i in 1:length(levels(df3$.id))){
    #             c = as.data.frame(df %>% filter(.id == levels(df3$.id)[i]) %>% mutate(newVar = (!!rlang::sym(input$variable_prim)-offset1$b[i])/offset1$a[i]))
    #             df4 = rbind(df4, c)
    #           }
    #         }
    #         #data.frame manipulation
    #         shiny::incProgress(3/10)
    #         df4 = droplevels(df4)
    #         df4 = df4 %>% select(-!!rlang::sym(input$variable_prim))
    #         names(df4)[names(df4) == "newVar"] <- paste(input$variable_prim)
    #         #assign to global environment
    #         df = df %>% filter(!.id %in% levels(df3$.id))
    #         df = rbind(df, df4)
    #         df = as.data.frame(df %>% group_by(.id) %>% arrange(date_time, .by_group = T))
    #         assign('df', df, envir=envir)
    #         shiny::incProgress(2/10)
    #       })
    #   }
    # }
    # observeEvent(input$bar12, {
    #   showModal(modalDialog(
    #     title = "Offset of TMS moisture only!!",
    #     HTML("Using this tool for other devices will lead to wrong data!!.<br>
    #          Do you have brushed values corresponding to zero (air exposed TMS)?"),
    #     checkboxInput("bar13","Do you have maximal values (submerged TMS)?", value = F),
    #     easyClose = TRUE,
    #     footer = tagList(
    #       modalButton("Cancel"),
    #       actionButton("ok_bar7", "OK")
    #     )
    #   ))
    # })
    # observeEvent(input$ok_bar7, {
    #   removeModal()
    #   if(is.null(input$variable_prim) | is.null(input$.id)) {
    #     NULL
    #   } else {
    #     subtractTMSoffset()
    #     d$b <- df
    #   }
    # })
    append1 = function (){
      df3 = as.data.frame(isolate(d$a))
      df3 = droplevels(df3)
      if(length(input$.id) != 2){
        showModal(modalDialog(
          title = "Append only two devices!",
          HTML('Select data from only two devices in data tab!'),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }else{
        shiny::withProgress(
          message = paste0("Processing..."),
          value = 0,
          {
            shiny::incProgress(2/10)
            #df4 = data.frame()
            df3 = df3 %>% arrange(.id,date_time)
            last = as.data.frame(df3 %>% filter(!is.na(!!rlang::sym(input$variable_prim))) %>% group_by(.id) %>% filter(date_time == last(date_time)))
            last = last %>% arrange(date_time)
            #first = df3 %>% filter(.id == last$.id[2], date_time == first(date_time)) %>% droplevels()
            #diff = first$Radius-last$Radius[1]
            shiny::incProgress(3/10)
            df3 = df3 %>% filter(!(.id == last$.id[2] & date_time <= last$date_time[1])) %>%
              mutate(.id = last$.id[2]) %>% droplevels() %>% distinct(date_time, .keep_all = T)
            #df3 = df3 %>% mutate(Radius =  case_when(.id == last$.id[2] ~ Radius-diff,.id == last$.id[1] ~ Radius)) %>% mutate(.id = last$.id[2]) %>% distinct(date_time, .keep_all = T)
            #assign to global environment
            df = df %>% filter(!.id %in% levels(last$.id)) %>% rbind(df3) %>% droplevels() %>% arrange(.id,date_time)
            shiny::incProgress(3/10)
            assign('df', df, envir=envir)
            shiny::incProgress(2/10)
          })
      }
    }
    observeEvent(input$bar14, {
      append1()
      d$b <- df
    })
    change_interval = function(){
      shiny::withProgress(
        message = paste0("Processing..."),
        value = 0,
        {shiny::incProgress(2/10)
          if(input$bar21 == "first"){
            meta_interval = isolate(df_meta())
            df = df %>% dplyr::group_by(.id) %>% dplyr::arrange(date_time, .by_group = T) %>% dplyr::mutate(date_time = floor_date(date_time, input$bar20)) %>%
              dplyr::group_by(.id, date_time) %>% dplyr::filter(row_number()==1) %>% dplyr::ungroup() %>%
              dplyr::mutate_if(is.numeric,~replace(., is.nan(.)|is.infinite(.), NA)) %>%
              dplyr::left_join(meta_interval, by = ".id")
          } else {
            if(input$bar21 == "last"){
              meta_interval = isolate(df_meta())
              df = df %>% dplyr::group_by(.id) %>% dplyr::mutate(date_time = floor_date(date_time, input$bar20)) %>%
                dplyr::group_by(.id, date_time) %>% dplyr::filter(row_number()==n()) %>% dplyr::ungroup() %>%
                dplyr::mutate_if(is.numeric,~replace(., is.nan(.)|is.infinite(.), NA)) %>%
                dplyr::left_join(meta_interval, by = ".id")
            } else {
              meta_interval = isolate(df_meta())
              df = df %>% dplyr::group_by(.id) %>% dplyr::mutate(date_time = floor_date(date_time, input$bar20)) %>%
                dplyr::group_by(.id, date_time) %>% dplyr::summarise_if(is.numeric, input$bar21, na.rm = T) %>% dplyr::ungroup() %>%
                dplyr::mutate_if(is.numeric,~replace(., is.nan(.)|is.infinite(.), NA)) %>%
                dplyr::left_join(meta_interval, by = ".id")
            }
          }
          shiny::incProgress(4/10)
          df = as.data.frame(droplevels(df))
          shiny::incProgress(2/10)
          assign('df', df, envir=envir)
          shiny::incProgress(2/10)
        })
    }
    observeEvent(input$bar19, {
      showModal(modalDialog(
        title = "Change data interval?",
        HTML("The df structure is going to be changed!<br>
             This will affect all data!"),
        br(),
        br(),
        selectInput("bar20", "Interval", choices = c("15 mins", "hour", "2 hours", "day", "week")),
        selectInput("bar21", "Summarize", choices = c("mean", "min", "max", "first", "last")),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok_bar9", "OK")
        )
      ))
    })
    observeEvent(input$ok_bar9, {
      change_interval()
      d$b <- df
      removeModal()
    })
    fct_choices = function(){
      ch <- colnames(Filter(is.factor, select_if(d$b,function(x){!all(is.na(x))})))
      return(ch)
    }
    observeEvent(input$bar43, {
      showModal(jqui_draggable(modalDialog(
        selectInput("fct_list",
                    "Factors:",
                    choices = fct_choices(),
                    multiple = F,
                    selectize = T,
                    selected = ".id"),
        easyClose = T,
        footer = tagList(actionButton("ok_bar17", "Append", class = "btn-success"),
                         modalButton("Cancel")),
      size = "m"
      ))
      )
    })
    observeEvent(input$ok_bar17, {
      if(input$fct_list != ".id") {
        shiny::withProgress(
          message = paste0("Processing..."),
          detail = ("Data manipulation"),
          value = 0,
          {
        df = df %>% dplyr::rename(.id_orig = .id, .id = input$fct_list) %>% group_by(.id) %>% dplyr::mutate(first_occurence = first(date_time)) %>% group_by(first_occurence) %>% dplyr::arrange(.id, date_time, .by_group = T) %>% ungroup() %>% dplyr::select(-first_occurence) %>% dplyr::distinct(.id, date_time, .keep_all = T) %>% as.data.frame()
        shiny::incProgress(5/10, detail = ("Assigning"))
        assign("df", df, envir = envir)
        d$b <- df
        shiny::incProgress(5/10, detail = ("Done"))
          })

      } else {
        NULL
      }
      removeModal()
    })
    observeEvent(input$freeze_switch,{
      if(isTRUE(input$freeze_switch) & length(input$.id)>=1 & input$variable_prim == "Radius"){
        freeze_show()
      }else{
        NULL
      }
    },ignoreInit = T, ignoreNULL = T)

    freeze_show = function(){
      shiny::withProgress(
        message = paste0("Processing..."),
        detail = ("Definitions"),
        value = 0,
        {df8 = as.data.frame(d$a)
        df8 = df8 %>% droplevels() %>% dplyr::mutate(date = floor_date(date_time, unit = "day")) %>%  dplyr::group_by(.id,date) %>%
          dplyr::mutate(T1_freeze = case_when(
            min(T1) < 5 ~ TRUE,TRUE ~ FALSE),
            T1_freeze2 = case_when(
              min(lag(T1,n = 96)) < -0 ~ TRUE,
              min(T1) < -5 ~ TRUE,
              mean(T1) < 5 & min(T1) < 0 ~ TRUE,
              TRUE ~ FALSE)) %>% dplyr::ungroup()
        shiny::incProgress(2/10, detail = ("Anomalies"))
        df8 = df8 %>% dplyr::group_by(.id) %>% dplyr::mutate(nrow = row_number())
        # analysing distance and size, change the minpoints to see the effect
        minpoints = 20
        df8_cluster = data.frame(matrix(ncol = 3, nrow = 0))
        colnames(df8_cluster) <- c(".id", "date_time", "cluster")
        for(i in levels(df8$.id)){
          df8_sub = droplevels(subset(df8, .id == i, select = c(".id","Radius", "nrow", "date_time")))
          df8_sub = na.omit(df8_sub)
          dist = kNNdist(as.matrix(df8_sub[,c("Radius", "nrow"),]), k = minpoints - 1)
          size = quantile(dist, 0.975)
          # Cluster with the chosen parameters
          res = dbscan(as.matrix(df8_sub[,c("Radius", "nrow"),]), eps = size, minPts = minpoints, borderPoints = F)
          df8_sub$cluster = res$cluster
          df8_sub = df8_sub %>% dplyr::select(.id, date_time, cluster)
          df8_cluster = rbind(df8_cluster, df8_sub)
        }
        shiny::incProgress(2/10,  detail = "Freeze days")
        # add clusters to df and clean_up
        df8 = dplyr::left_join(df8, df8_cluster, by = c(".id", "date_time"))
        rm(df8_sub, df8_cluster, minpoints, size, dist,i)
        # analysis of freezing data
        df8 = df8 %>% dplyr::group_by(.id, date) %>% dplyr::mutate(cluster_zero = case_when(
          min(cluster) == 0 & any(T1_freeze == TRUE) |  any(T1_freeze2 == TRUE) ~ TRUE,
          TRUE ~ FALSE), cluster_zero2 = case_when(any(T1_freeze == TRUE) ~ TRUE, TRUE ~ FALSE) )%>% dplyr::ungroup()
        shiny::incProgress(2/10,  detail = "Clean data")
        # freezing data extraction and interpolation
        df8 = df8 %>% dplyr::group_by(.id) %>% dplyr::mutate(freeze_show = ifelse(is.na(Radius), Inf, ifelse(cluster_zero == FALSE | is.na(cluster_zero),Radius,NA)),
                                                             freeze_show2 = ifelse(is.na(Radius), Inf, ifelse(cluster_zero2 == FALSE | is.na(cluster_zero2),Radius,NA))) %>% dplyr::ungroup()
        # data_db2 = data_db2 %>% mutate(freeze_show = na.spline(freeze_show) + 0*na.approx(freeze_show, na.rm = FALSE), freeze_show2 = na.spline(freeze_show2) + 0*na.approx(freeze_show2, na.rm = FALSE))
        df8 = as.data.frame(df8 %>% group_by(.id)  %>% dplyr::mutate(freeze_show = na.approx(freeze_show, na.rm = F), freeze_show2 = na.approx(freeze_show2, na.rm = F))%>%
                              dplyr::mutate(freeze_show = ifelse(freeze_show<=Radius & cluster !=0 | T1_freeze == FALSE , Radius, NA), freeze_show2 = ifelse(freeze_show2<=Radius & cluster != 0 | T1_freeze == FALSE, Radius, NA)) %>%
                              dplyr::mutate(freeze_show = na.approx(freeze_show, na.rm = F), freeze_show2 = na.approx(freeze_show2, na.rm = F)) %>%
                              dplyr::mutate(freeze_show = na_if(freeze_show, Inf), freeze_show2 = na_if(freeze_show2, Inf)) %>%
                              dplyr::mutate(lead_trail1 = na.locf(freeze_show, na.rm = F, fromLast = F), lead_trail2 = na.locf(freeze_show2, na.rm = F, fromLast = F)) %>%
                              dplyr::mutate(lead_trail1 = na.locf(lead_trail1, na.rm = F, fromLast = T), lead_trail2 = na.locf(lead_trail2, na.rm = F, fromLast = T)) %>%
                              dplyr::mutate(freeze_show = ifelse(is.na(Radius), NA, ifelse(lead_trail1<=Radius, Radius,lead_trail1 )), freeze_show2 = ifelse(is.na(Radius), NA, ifelse(lead_trail2<=Radius, Radius,lead_trail2 ))) %>%
                              dplyr::ungroup() %>% dplyr::select(-date,-T1_freeze, -T1_freeze2, -nrow, -cluster, -cluster_zero, -cluster_zero2, -lead_trail1, -lead_trail2))
        # df = as.data.frame(df %>% group_by(.id) %>% complete(date_time = seq.POSIXt(min(date_time), max(date_time), by="15 min",tz = 'UTC')) %>% arrange(date_time, .by_group = T))
        shiny::incProgress(2/10,  detail = "Decomposing")
        # decompose
        # d$a <- as.data.frame(df8)
        # rm(df8)
        # shiny::incProgress(2/10,  detail = "Done")
        # return(d$a)
        d$a<- as.data.frame(df8)
        rm(df8)
        shiny::incProgress(2/10,  detail = "Done")
        return(d$a)
        })
    }

    observeEvent(input$anomalies_switch,{
      if(isTRUE(input$anomalies_switch) & length(input$.id)>=1 & input$variable_prim == "Radius"){
        anomalies_show()
      }else{
        NULL
      }
    },ignoreInit = T, ignoreNULL = T)
    observeEvent(input$bar38,{
      if(isTRUE(input$anomalies_switch) & length(input$.id)>=1 & input$variable_prim == "Radius"){
        anomalies_show()
        }else{
          NULL
        }
    })

    anomalies_show = function(){
      shiny::withProgress(
        message = paste0("Processing..."),
        detail = ("Definitions"),
        value = 0,
        {df7 =as.data.frame(isolate(d$a))
        # df7 = df7 %>% droplevels() %>% dplyr::mutate(date = floor_date(date_time, unit = "day")) %>%  dplyr::group_by(.id,date) %>%
        #   dplyr::mutate(T1_freeze = case_when(
        #     min(T1) < 5 ~ TRUE,TRUE ~ FALSE),
        #     T1_freeze2 = case_when(
        #       min(lag(T1,n = 96)) < -0 ~ TRUE,
        #       min(T1) < -5 ~ TRUE,
        #       mean(T1) < 5 & min(T1) < 0 ~ TRUE,
        #       TRUE ~ FALSE)) %>% dplyr::ungroup()
        shiny::incProgress(2/10, detail = ("Anomalies"))
        df7 = df7 %>% droplevels() %>% dplyr::group_by(.id) %>% dplyr::mutate(nrow = row_number())
        # analysing distance and size, change the minpoints to see the effect
        minpoints = input$minpoints_val
        df7_cluster = data.frame(matrix(ncol = 3, nrow = 0))
        colnames(df7_cluster) <- c(".id", "date_time", "cluster")
        for(i in levels(df7$.id)){
          df7_sub = droplevels(subset(df7, .id == i, select = c(".id","Radius", "nrow", "date_time")))
          df7_sub = na.omit(df7_sub)
          dist = kNNdist(as.matrix(df7_sub[,c("Radius", "nrow"),]), k = minpoints - 1)
          size = quantile(dist, 0.975)
          # Cluster with the chosen parameters
          res = dbscan(as.matrix(df7_sub[,c("Radius", "nrow"),]), eps = size, minPts = minpoints, borderPoints = F)
          df7_sub$cluster = res$cluster
          df7_sub = df7_sub %>% dplyr::select(.id, date_time, cluster)
          df7_cluster = rbind(df7_cluster, df7_sub)
        }
        shiny::incProgress(2/10,  detail = "Freeze days")
        # add clusters to df and clean_up
        df7 = dplyr::left_join(df7, df7_cluster, by = c(".id", "date_time"))
        rm(df7_sub, df7_cluster, minpoints, size, dist,i)
        # analysis of freezing data
        # df7 = df7 %>% dplyr::group_by(.id, date) %>% dplyr::mutate(cluster_zero = case_when(
        #   min(cluster) == 0 & any(T1_freeze == TRUE) |  any(T1_freeze2 == TRUE) ~ TRUE,
        #   TRUE ~ FALSE), cluster_zero2 = case_when(any(T1_freeze == TRUE) ~ TRUE, TRUE ~ FALSE) )%>% dplyr::ungroup()
        # analysis of freezing data
        df7 = df7 %>% dplyr::group_by(.id) %>% dplyr::mutate(cluster_zero = ifelse(
          cluster == 0, Radius, NA))%>% dplyr::ungroup() %>% dplyr::select(-nrow, -cluster)
        shiny::incProgress(2/10,  detail = "Clean data")
        # freezing data extraction and interpolation
        # df7 = df7 %>% dplyr::group_by(.id) %>% dplyr::mutate(freeze_show = ifelse(is.na(Radius), Inf, ifelse(cluster_zero == FALSE | is.na(cluster_zero),Radius,NA)),
        #                                                      freeze_show2 = ifelse(is.na(Radius), Inf, ifelse(cluster_zero2 == FALSE | is.na(cluster_zero2),Radius,NA))) %>% dplyr::ungroup()
        # # data_db2 = data_db2 %>% mutate(freeze_show = na.spline(freeze_show) + 0*na.approx(freeze_show, na.rm = FALSE), freeze_show2 = na.spline(freeze_show2) + 0*na.approx(freeze_show2, na.rm = FALSE))
        # df7 = as.data.frame(df7 %>% group_by(.id)  %>% dplyr::mutate(freeze_show = na.approx(freeze_show, na.rm = F), freeze_show2 = na.approx(freeze_show2, na.rm = F))%>%
        #                       dplyr::mutate(freeze_show = ifelse(freeze_show<=Radius & cluster !=0 | T1_freeze == FALSE , Radius, NA), freeze_show2 = ifelse(freeze_show2<=Radius & cluster != 0 | T1_freeze == FALSE, Radius, NA)) %>%
        #                       dplyr::mutate(freeze_show = na.approx(freeze_show, na.rm = F), freeze_show2 = na.approx(freeze_show2, na.rm = F)) %>%
        #                       dplyr::mutate(freeze_show = na_if(freeze_show, Inf), freeze_show2 = na_if(freeze_show2, Inf)) %>%
        #                       dplyr::mutate(lead_trail1 = na.locf(freeze_show, na.rm = F, fromLast = F), lead_trail2 = na.locf(freeze_show2, na.rm = F, fromLast = F)) %>%
        #                       dplyr::mutate(lead_trail1 = na.locf(lead_trail1, na.rm = F, fromLast = T), lead_trail2 = na.locf(lead_trail2, na.rm = F, fromLast = T)) %>%
        #                       dplyr::mutate(freeze_show = ifelse(is.na(Radius), NA, ifelse(lead_trail1<=Radius, Radius,lead_trail1 )), freeze_show2 = ifelse(is.na(Radius), NA, ifelse(lead_trail2<=Radius, Radius,lead_trail2 ))) %>%
        #                       dplyr::ungroup() %>% dplyr::select(-date,-T1_freeze, -T1_freeze2, -nrow, -cluster, -cluster_zero, -cluster_zero2, -lead_trail1, -lead_trail2))
        # df = as.data.frame(df %>% group_by(.id) %>% complete(date_time = seq.POSIXt(min(date_time), max(date_time), by="15 min",tz = 'UTC')) %>% arrange(date_time, .by_group = T))
        shiny::incProgress(2/10,  detail = "Decomposing")
        # decompose
        d$a <- as.data.frame(df7)
        rm(df7)
        shiny::incProgress(2/10,  detail = "Done")
        return(d$a)
        })
    }

    observeEvent(input$bar30, {
      showModal(modalDialog(
        title = "Decompose Radius of selected devices to GRO, TWD and FREEZE?",
        HTML("Is the dendrometer growth variable selected in the upper plot?<br>
               Clean the dendrometer data before this step!"),
        br(),
        br(),
        # selectInput("bar31", "Freeze method:", choices = c("Raw", "Fine")),
        # numericInput("bar32", "Density clustering Minpoints:", min = 3, max = 60, step = 1, value = 20),
        # numericInput("bar33", "Minimum temperature method1:", min = -10, max = 10, step = 1, value = 5),
        # numericInput("bar35", "Minimum temperature forced filtered method_lower_plot:", min = -10, max = 10, step = 1, value = -5),
        # numericInput("bar34", "Mean daily temperature with below zero temperature:",  min = -10, max = 10, step = 1, value = 5),
        numericInput("freeze_temp_input", "Freeze temperature",  min = -10, max = 10, step = 0.1, value = 0),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok_bar13", "OK")
        )
      ))
    })

    observeEvent(input$ok_bar13, {
      # if(isTRUE(input$freeze_switch)){
      #   shinyWidgets::updateMaterialSwitch(session, "freeze_switch", value = F)
      # }
      req(!input$variable_prim == "T1")
      GRO_TWD_FREEZE()
      d$b <- df
      removeModal()
    })

    GRO_TWD_FREEZE = function(){
      shiny::withProgress(
        message = paste0("Processing..."),
        detail = "Definitions",
        value = 0,
        {
          if("T1" %in% colnames(d$a)) {
            shiny::incProgress(2/10, detail = "Filtering freeze days")
            df6 = isolate(d$a) %>% dplyr::mutate(day_freeze = lubridate:: floor_date(date_time, "day")) %>%
              dplyr::rename(Variable_freeze_orig = input$variable_prim) %>%
              dplyr::mutate(GRO_orig = Variable_freeze_orig,
                            FREEZE = Variable_freeze_orig) %>%
              group_by(.id, day_freeze) %>% mutate(GRO_orig = case_when(any(T1 < 0) ~ NA, TRUE ~ GRO_orig)
                                                   # ,
                                                    # = case_when(any(T1 < as.numeric(input$freeze_temp_input)) ~ FREEZE, TRUE ~ 0)
                                                   ) %>%
              group_by(.id) %>% mutate(GRO_orig = zoo::na.spline(GRO_orig, na.rm = F)) %>%
              ungroup() %>%
              mutate(GRO_orig = if_else(is.na(Variable_freeze_orig), NA, GRO_orig)) %>%
              select(-day_freeze) %>% as.data.frame()
          } else {
            shiny::incProgress(2/10, detail = "GRO TWD")
            df6 = d$a
          }
          df6 = df6 %>% mutate(year = lubridate::year(date_time)) %>% dplyr::group_by(.id) %>%
            dplyr::mutate(GRO = if_else(is.na(GRO_orig), NA , cummax(if_else(is.na(GRO_orig), -Inf, GRO_orig))),
                          TWD = if_else(is.na(GRO), NA, GRO_orig-GRO),
                          FREEZE = if_else(is.na(GRO_orig), NA, FREEZE-GRO_orig)) %>%
            mutate(FREEZE = if_else(FREEZE > 0, 0, FREEZE))%>%
            dplyr::group_by(year, .add = T) %>%
            dplyr::mutate(GRO = GRO - first(na.omit(GRO)),
                          Variable_freeze_orig = Variable_freeze_orig - first(na.omit(Variable_freeze_orig))) %>%
            dplyr::ungroup() %>%
            dplyr::select(-GRO_orig, -year) %>%
            plyr::rename(., c("Variable_freeze_orig" = input$variable_prim)) %>% as.data.frame()
          df = df %>% dplyr::anti_join(df6, by = c(".id", "date_time")) %>% dplyr::bind_rows(df6) %>% droplevels() %>% dplyr::arrange(.id, date_time) %>% as.data.frame()
          rm(df6)
          assign('df', df, envir=envir)
          shiny::incProgress(2/10,  detail = "Done")
        # df6 =as.data.frame(isolate(d$a))
        # df6 = df6 %>% dplyr::group_by(.id) %>% dplyr::filter(!all(is.na(Radius))) %>% droplevels() %>% dplyr::mutate(date = floor_date(date_time, unit = "day")) %>%  dplyr::group_by(.id,date) %>%
        #   dplyr::mutate(T1_freeze = case_when(
        #     min(T1) < input$bar33 ~ TRUE,TRUE ~ FALSE),
        #     T1_freeze2 = case_when(
        #       min(lag(T1,n = 96)) < -0 ~ TRUE,
        #       min(T1) < input$bar35 ~ TRUE,
        #       mean(T1) < input$bar34 & min(T1) < 0 ~ TRUE,
        #       TRUE ~ FALSE)) %>% dplyr::ungroup()
        # shiny::incProgress(2/10, detail = "Anomalies")
        # df6 = df6 %>% dplyr::group_by(.id) %>% dplyr::mutate(nrow = row_number())
        # # analysing distance and size, change the minpoints to see the effect
        # minpoints = input$bar32
        # df6_cluster = data.frame(matrix(ncol = 3, nrow = 0))
        # colnames(df6_cluster) <- c(".id", "date_time", "cluster")
        # for(i in levels(df6$.id)){
        #   df6_sub = droplevels(subset(df6, .id == i, select = c(".id","Radius", "nrow", "date_time")))
        #   df6_sub = na.omit(df6_sub)
        #   dist = kNNdist(as.matrix(df6_sub[,c("Radius", "nrow"),]), k = minpoints - 1)
        #   size = quantile(dist, 0.975)
        #   # Cluster with the chosen parameters
        #   res = dbscan(as.matrix(df6_sub[,c("Radius", "nrow"),]), eps = size, minPts = minpoints, borderPoints = F)
        #   df6_sub$cluster = res$cluster
        #   df6_sub = df6_sub %>% dplyr::select(.id, date_time, cluster)
        #   df6_cluster = rbind(df6_cluster, df6_sub)
        # }
        # shiny::incProgress(2/10,  detail = "Freeze days")
        # # add clusters to df and clean_up
        # df6 = dplyr::left_join(df6, df6_cluster, by = c(".id", "date_time"))
        # rm(df6_sub, df6_cluster, minpoints, size, dist,i)
        # # analysis of freezing data
        # df6 = df6 %>% dplyr::group_by(.id, date) %>% dplyr::mutate(cluster_zero = case_when(
        #   min(cluster) == 0 & any(T1_freeze == TRUE) |  any(T1_freeze2 == TRUE) ~ TRUE,
        #   TRUE ~ FALSE), cluster_zero2 = case_when(any(T1_freeze == TRUE) ~ TRUE, TRUE ~ FALSE) )%>% dplyr::ungroup()
        # shiny::incProgress(2/10,  detail = "Clean data")
        # # freezing data extraction and interpolation
        # df6 = df6 %>% dplyr::group_by(.id) %>% dplyr::mutate(freeze_show = ifelse(is.na(Radius), Inf, ifelse(cluster_zero == FALSE | is.na(cluster_zero),Radius,NA)),
        #                                                      freeze_show2 = ifelse(is.na(Radius), Inf, ifelse(cluster_zero2 == FALSE | is.na(cluster_zero2),Radius,NA))) %>% dplyr::ungroup()
        # # data_db2 = data_db2 %>% mutate(freeze_show = na.spline(freeze_show) + 0*na.approx(freeze_show, na.rm = FALSE), freeze_show2 = na.spline(freeze_show2) + 0*na.approx(freeze_show2, na.rm = FALSE))
        # df6 = as.data.frame(df6 %>% group_by(.id)  %>% dplyr::mutate(freeze_show = na.approx(freeze_show, na.rm = F), freeze_show2 = na.approx(freeze_show2, na.rm = F))%>%
        #                       dplyr::mutate(freeze_show = ifelse(freeze_show<=Radius & cluster !=0 | T1_freeze == FALSE , Radius, NA), freeze_show2 = ifelse(freeze_show2<=Radius & cluster != 0 | T1_freeze == FALSE, Radius, NA)) %>%
        #                       dplyr::mutate(freeze_show = na.approx(freeze_show, na.rm = F), freeze_show2 = na.approx(freeze_show2, na.rm = F)) %>%
        #                       dplyr::mutate(freeze_show = na_if(freeze_show, Inf), freeze_show2 = na_if(freeze_show2, Inf)) %>%
        #                       dplyr::mutate(lead_trail1 = na.locf(freeze_show, na.rm = F, fromLast = F), lead_trail2 = na.locf(freeze_show2, na.rm = F, fromLast = F)) %>%
        #                       dplyr::mutate(lead_trail1 = na.locf(lead_trail1, na.rm = F, fromLast = T), lead_trail2 = na.locf(lead_trail2, na.rm = F, fromLast = T)) %>%
        #                       dplyr::mutate(freeze_show = ifelse(is.na(Radius), NA, ifelse(lead_trail1<=Radius, Radius,lead_trail1 )), freeze_show2 = ifelse(is.na(Radius), NA, ifelse(lead_trail2<=Radius, Radius,lead_trail2 ))) %>%
        #                       dplyr::ungroup() %>% dplyr::select(-date,-T1_freeze, -T1_freeze2, -nrow, -cluster, -cluster_zero, -cluster_zero2, -lead_trail1, -lead_trail2))
        # # df = as.data.frame(df %>% group_by(.id) %>% complete(date_time = seq.POSIXt(min(date_time), max(date_time), by="15 min",tz = 'UTC')) %>% arrange(date_time, .by_group = T))
        # shiny::incProgress(2/10,  detail = "Decomposing")
        # # decompose
        # if (input$bar31 == "Raw") {
        #   df6 = df6 %>% group_by(.id) %>% mutate(Radius_first = Radius-first(na.omit(Radius)), freeze_show2_first = freeze_show2-first(na.omit(freeze_show2)))
        #   df6 = as.data.frame(df6 %>% dplyr::group_by(.id) %>%
        #                         dplyr::mutate(
        #                           FREEZE = ifelse(is.na(Radius_first), NA, Radius_first-freeze_show2_first),
        #                           GRO = ifelse(is.na(Radius_first), NA, cummax(ifelse(is.na(Radius_first), -Inf, freeze_show2_first))),
        #                           TWD = ifelse(is.na(Radius_first), NA, freeze_show2_first-GRO)) %>%
        #                         dplyr::select(-Radius_first, -freeze_show2_first, -freeze_show, -freeze_show2))
        # } else {
        #   df6 = df6 %>% group_by(.id) %>% mutate(Radius_first = Radius-first(na.omit(Radius)), freeze_show_first = freeze_show-first(na.omit(freeze_show)))
        #   df6 = as.data.frame(df6 %>% dplyr::group_by(.id) %>%
        #                         dplyr::mutate(
        #                           FREEZE = ifelse(is.na(Radius_first), NA, Radius_first-freeze_show_first),
        #                           GRO = ifelse(is.na(Radius_first), NA, cummax(ifelse(is.na(Radius_first), -Inf, freeze_show_first))),
        #                           TWD = ifelse(is.na(Radius_first), NA, freeze_show_first-GRO)) %>%
        #                         dplyr::select(-Radius_first, -freeze_show,-freeze_show_first, -freeze_show2))
        # }
        # df = df %>% filter(!.id %in% levels(df6$.id)) %>% dplyr::bind_rows(df6) %>% droplevels() %>% dplyr::arrange(.id,date_time)
        # rm(df6)
        # assign('df', df, envir=envir)
        # shiny::incProgress(2/10,  detail = "Done")
        })
    }

    fill_gap_linear = function(){
      shiny::withProgress(
        message = paste0("Processing..."),
        detail = "Selecting data",
        value = 0,
        {
      if(isTRUE(input$fill_id_select | isTRUE(input$one_by_one_switch))){
        df3 = isolate(d$a)
      }else{
        df3 = isolate(d$a)[isolate(d$a)[[".id"]] %in% input$one_by_one_group_select,]
      }
      shiny::incProgress(2/10,  detail = "Fill Nas")
      df3 = as.data.frame(df3 %>% dplyr::group_by(.id) %>% dplyr::arrange(date_time, .by_group = T) %>% dplyr::rename(fill_lin = input$variable_prim) %>%
                            select(.id, date_time, fill_lin) %>% dplyr::mutate(fill_lin = na.approx(fill_lin, na.rm = F
                                                                                                    # , maxgap = input$maxgap
                                                                                                    )))
      shiny::incProgress(3/10,  detail = "Joining data")
       df = as.data.frame(dplyr::left_join(df, df3, by = c('.id', 'date_time')) %>% mutate(fill_lin = ifelse(is.na(fill_lin), !!rlang::sym(input$variable_prim), fill_lin)) %>%
                           select(-c(!!rlang::sym(input$variable_prim))))
       shiny::incProgress(2/10,  detail = "Assigning")
      df = plyr::rename(df, c("fill_lin" = input$variable_prim))
      rm(df3)
      assign('df', df, envir=envir)
      shiny::incProgress(3/10,  detail = "Done")
        })
    }
    # changefillgap----
    fill_gap_avg = function(){
      shiny::withProgress(
        message = paste0("Processing..."),
        detail = "Selecting data",
        value = 0,
        {
          if(isTRUE(input$fill_id_select | isFALSE(input$one_by_one_switch))){
            df3 = isolate(d$a)
            target_id = levels(df3$.id)
          }else{
            df3 = isolate(d$a)
            target_id = input$one_by_one_group_select
          }
          shiny::incProgress(2/10,  detail = "Analysing Nas")
          if(is.null(input$fill_gap_group)){
            df3 = as.data.frame(df3 %>% group_by(.id) %>% dplyr::arrange(date_time, .by_group = T) %>% dplyr::rename(fill_avg = input$variable_prim) %>%
                                  dplyr::select(.id, date_time, fill_avg) %>% group_by(.id) %>%
                                  mutate(na_order = cumsum(!is.na(fill_avg)), left = na.locf(fill_avg, na.rm = F), right = na.locf(fill_avg, fromLast = T, na.rm = F)) %>%
                                  mutate(na_order = ifelse(is.na(fill_avg), na_order, NA)) %>% group_by(.id, na_order) %>% mutate(na_n = n()+1) %>%
                                  ungroup() %>% dplyr::filter(date_time %in% df3[is.na(df3[input$variable_prim]),"date_time"]) %>%
                                  dplyr::group_by(date_time) %>% dplyr::mutate(fill_avg_mean = mean(fill_avg, na.rm = T)) %>% ungroup() %>%
                                  dplyr::filter(.id %in% target_id) %>% droplevels() %>% group_by(.id) %>% filter(is.na(fill_avg)))

          } else {
            df3 = as.data.frame(df3 %>% group_by(.id) %>% dplyr::arrange(date_time, .by_group = T) %>% dplyr::rename(fill_avg = input$variable_prim) %>%
                                  dplyr::select(.id, date_time, fill_avg, input$fill_gap_group) %>% group_by(.id) %>%
                                  mutate(na_order = cumsum(!is.na(fill_avg)), left = na.locf(fill_avg, na.rm = F), right = na.locf(fill_avg, fromLast = T, na.rm = F)) %>%
                                  mutate(na_order = ifelse(is.na(fill_avg), na_order, NA)) %>% group_by(.id, na_order) %>% mutate(na_n = n()+1) %>%
                                  ungroup() %>% dplyr::filter(date_time %in% df3[is.na(df3[input$variable_prim]),"date_time"]) %>%
                                  dplyr::group_by(.dots = c("date_time", input$fill_gap_group)) %>% dplyr::mutate(fill_avg_mean = mean(fill_avg, na.rm = T)) %>% ungroup() %>%
                                  dplyr::filter(.id %in% target_id) %>% droplevels() %>% group_by(.id) %>% filter(is.na(fill_avg)))
          }
          shiny::incProgress(2/10,  detail = "Filling NAs")
          df3 = as.data.frame(df3 %>% group_by(.id, na_order) %>% mutate(fill_avg_left = first(fill_avg_mean), fill_avg_right = last(fill_avg_mean)) %>%
                                mutate(fill_avg2 = ifelse(is.na(left), fill_avg_mean+(right-fill_avg_right),  fill_avg_mean+(left-fill_avg_left))) %>%
                                mutate(fill_avg2 = fill_avg2+(cumsum((right-last(fill_avg2)))/na_n)) %>% ungroup() %>% select(date_time, .id, fill_avg2))
          shiny::incProgress(2/10,  detail = "Joining data")
          df = as.data.frame(dplyr::left_join(df, df3, by = c('.id', 'date_time')) %>% mutate(fill_avg2 = ifelse(is.na(fill_avg2), !!rlang::sym(input$variable_prim), fill_avg2)) %>%
                               select(-c(!!rlang::sym(input$variable_prim))))
          shiny::incProgress(2/10,  detail = "Assigning")
          df = plyr::rename(df, c("fill_avg2" = input$variable_prim))
          rm(df3)
          assign('df', df, envir=envir)
          shiny::incProgress(2/10,  detail = "Done")
        })
    }
    fill_gap_avg_scale = function(){
      shiny::withProgress(
        message = paste0("Processing..."),
        detail = "Selecting data",
        value = 0,
        {
          if(isTRUE(input$fill_id_select | isTRUE(input$one_by_one_switch))){
            df3 = isolate(d$a) %>% droplevels()
            target_id = levels(df3$.id)
          }else{
            df3 = isolate(d$a)  %>% droplevels()
            target_id = input$one_by_one_group_select
          }
          shiny::incProgress(2/10,  detail = "Analysing Nas")
          if(is.null(input$fill_gap_group)){
            df3 = df3 %>% group_by(.id) %>% dplyr::arrange(date_time, .by_group = T) %>% dplyr::rename(fill_avg = input$variable_prim) %>%
              dplyr::mutate(fill_avg_filled = zoo::na.approx(fill_avg, na.rm = F)) %>%
              dplyr::select(.id, date_time, fill_avg, fill_avg_filled) %>% group_by(.id) %>%
              mutate(na_order = cumsum(!is.na(fill_avg))) %>%
              mutate(na_order = ifelse(is.na(fill_avg), na_order, NA)) %>% group_by(.id, na_order) %>%
              group_by(date_time) %>% dplyr::filter(any(is.na(fill_avg))) %>%
              dplyr::mutate(jumps = length(na.omit(fill_avg)))  %>% dplyr::mutate(fill_avg_mean = mean(fill_avg, na.rm = T)) %>% mutate(fill_avg_mean = ifelse(is.nan(fill_avg_mean), NA, fill_avg_mean)) %>% ungroup() %>%
              dplyr::filter(.id %in% target_id) %>% droplevels() %>% group_by(.id) %>% filter(is.na(fill_avg)) %>% droplevels() %>%
              mutate(fill_avg_mean_filled = fill_avg_mean) %>%
              tidyr::fill(fill_avg_mean_filled, .direction = "downup") %>%
              group_by(.id, na_order) %>%
              dplyr::mutate(jumps = ifelse(jumps-lag(jumps) == 0 | is.na(jumps-lag(jumps)), NA, lag(fill_avg_mean_filled)-fill_avg_mean_filled+lag(fill_avg_mean_filled)-lag(fill_avg_mean_filled,2))) %>%
              dplyr::mutate(jumps = cumsum(tidyr::replace_na(jumps,0))) %>%
              dplyr::mutate(fill_avg_mean = ifelse(is.na(fill_avg_mean), NA, fill_avg_mean_filled+jumps)) %>%
              select(-fill_avg_mean_filled, - jumps) %>%
              as.data.frame()
          } else {
            df3 = df3 %>% group_by(.id) %>% dplyr::arrange(date_time, .by_group = T) %>% dplyr::rename(fill_avg = input$variable_prim) %>%
              dplyr::mutate(fill_avg_filled = zoo::na.approx(fill_avg, na.rm = F)) %>%
              dplyr::select(.id, date_time, fill_avg, fill_avg_filled, input$fill_gap_group) %>% group_by(.id) %>%
              mutate(na_order = cumsum(!is.na(fill_avg))) %>%
              mutate(na_order = ifelse(is.na(fill_avg), na_order, NA)) %>% group_by(.id, na_order) %>%
              group_by(across(c("date_time", input$fill_gap_group))) %>% dplyr::filter(any(is.na(fill_avg))) %>%
              dplyr::mutate(jumps = length(na.omit(fill_avg)))  %>% dplyr::mutate(fill_avg_mean = mean(fill_avg, na.rm = T)) %>% mutate(fill_avg_mean = ifelse(is.nan(fill_avg_mean), NA, fill_avg_mean)) %>% ungroup() %>%
              dplyr::filter(.id %in% target_id) %>% droplevels() %>% group_by(.id) %>% filter(is.na(fill_avg)) %>% droplevels() %>%
              mutate(fill_avg_mean_filled = fill_avg_mean) %>%
              tidyr::fill(fill_avg_mean_filled, .direction = "downup") %>%
              group_by(.id, na_order) %>%
              dplyr::mutate(jumps = ifelse(jumps-lag(jumps) == 0 | is.na(jumps-lag(jumps)), NA, lag(fill_avg_mean_filled)-fill_avg_mean_filled+lag(fill_avg_mean_filled)-lag(fill_avg_mean_filled,2))) %>%
              dplyr::mutate(jumps = cumsum(tidyr::replace_na(jumps,0))) %>%
              dplyr::mutate(fill_avg_mean = ifelse(is.na(fill_avg_mean), NA, fill_avg_mean_filled+jumps)) %>%
              select(-fill_avg_mean_filled, - jumps) %>%
              as.data.frame()
          }
          shiny::incProgress(2/10,  detail = "Filling NAs")
          df3 = df3 %>% group_by(.id, na_order) %>%
            mutate(vec_len = length(na.omit(fill_avg_mean)),delta_first = first(na.omit(fill_avg_filled-fill_avg_mean)), delta_last = last(na.omit(fill_avg_filled-fill_avg_mean))) %>%
            mutate(vec_len = ifelse(is.na(fill_avg_mean), NA, vec_len), delta_first = ifelse(is.na(fill_avg_mean), NA, delta_first), delta_last = ifelse(is.na(fill_avg_mean), NA, delta_last)) %>%
            mutate(delta_filled = (last(na.omit(fill_avg_mean))+delta_last)-(first(na.omit(fill_avg_mean))+delta_first)) %>%
            mutate(delta_filled = ifelse(is.na(fill_avg_mean), NA, delta_filled)) %>%
            mutate(fill_avg_mean = fill_avg_mean - first(na.omit(fill_avg_mean))) %>%
            mutate(fill_avg_mean = fill_avg_mean*(delta_filled/ifelse(last(na.omit(fill_avg_mean))==0,1,last(na.omit(fill_avg_mean))))) %>%
            mutate(fill_avg_mean = fill_avg_mean+first(na.omit(fill_avg_filled-fill_avg_mean))) %>%
            ungroup() %>% select(date_time, .id, fill_avg_mean) %>% as.data.frame()
          shiny::incProgress(2/10,  detail = "Joining data")
          df = as.data.frame(dplyr::left_join(df, df3, by = c('.id', 'date_time')) %>% mutate(fill_avg_mean = ifelse(is.na(fill_avg_mean), !!rlang::sym(input$variable_prim), fill_avg_mean)) %>%
                               select(-c(!!rlang::sym(input$variable_prim))))
          shiny::incProgress(2/10,  detail = "Assigning")
          df = plyr::rename(df, c("fill_avg_mean" = input$variable_prim))
          rm(df3)
          assign('df', df, envir=envir)
          shiny::incProgress(2/10,  detail = "Done")
        })
    }
    observeEvent(input$Reconstruct_action, {
      showModal(modalDialog(
        title = "Remove noise from data?",
        br(),
        br(),
        selectInput("bar40", "Remove jumps:", choices = c("lower", "upper"),  selected = "lower"),
        br(),
        numericInput("bar41", "Day level to filter:", min = 40, max = 200, step = 10, value = 100),
        numericInput("bar42", "Week level to filter:", min = 50, max = 600, step = 10, value = 400),
        shinyWidgets::materialSwitch("Reconstruct_all", "Process all data selected in data tab?",
                                     status = "primary",
                                     value = F),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok_bar40", "OK")
        )
      ))
    })
    observeEvent(input$ok_bar40, {
      reconstruct_data()
      removeModal()
    })
    reconstruct_data = function(){
      shiny::withProgress(
        message = paste0("Processing..."),
        value = 0,
        {
      if(input$variable_prim != "Radius"){
        showModal(modalDialog(
          title = "Radius is not selected variable!",
          HTML("Select Radius as the primary axis variable"),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))} else{
          shiny::incProgress(3/10, detail = "data preparation")
          if(isFALSE(input$Reconstruct_all)){
            if(isFALSE(input$one_by_one_switch)){
              r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
            }else{
              r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
            }
            df9 = isolate(df[r,])
          }else{
            df9 = isolate(d$a)
          }
          df9 = df9 %>% dplyr::select(.id, date_time, Radius) %>% droplevels()
          if(length(df9$.id) < 1){
            showModal(modalDialog(
              title = "No data selected!",
              HTML("Brush data from at least one device"),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          }else{
            shiny::incProgress(3/10, detail = "data analysis")
            df9 = df9 %>% mutate(date = lubridate::date(date_time), year = lubridate::year(date_time), week = lubridate::week(date_time)) %>% group_by(.id, date) %>%
              {if(input$bar40 == "lower") mutate(.data = .,dailymax = min_max(Radius, method = "max")) %>% mutate(Radius_day_clean = ifelse(Radius < dailymax-input$bar41, NA, Radius)) else mutate(.data = ., dailymin = min_max(Radius,method = "min")) %>% mutate(Radius_day_clean = ifelse(Radius > dailymin + input$bar41, NA, Radius))} %>%
              group_by(.id, year, week)  %>%
              {if(input$bar40 == "lower") mutate(.data = .,weekmax = min_max(Radius_day_clean, method = "max")) %>% mutate(Radius = ifelse(Radius_day_clean < weekmax-input$bar42, NA, Radius)) else mutate(.data = ., weekmin = min_max(Radius_day_clean,method = "min")) %>% mutate(Radius = ifelse(Radius_day_clean > weekmin + input$bar42, NA, Radius))} %>%
              dplyr::select(.id, date_time, Radius) %>% as.data.frame()
            shiny::incProgress(2/10, detail = "merging")
            if(isFALSE(input$Reconstruct_all)){
              df[r,input$variable_prim] <- NA
            }else{
              df[df$.id %in% unique(df9$.id),input$variable_prim] <- NA
            }
            df = left_join(df, df9, by = c('.id', 'date_time')) %>% dplyr::mutate(Radius.x = ifelse(is.na(Radius.y), Radius.x, Radius.y)) %>%
              dplyr::rename_all(~sub('.x', '', .x, fixed=T)) %>% dplyr::select(paste(colnames(df)))
            rm(df9)
            shiny::incProgress(1/10, detail = "assigning")
            assign('df', df, envir=envir)
            d$b <-df
          }
          shiny::incProgress(3/10, detail = "done")
        }
        })
    }
    observeEvent(input$fill_gap_action, {
      showModal(jqui_draggable(modalDialog(
        shinyWidgets::materialSwitch("fill_id_select", "Process all data selected in data tab?",
                    status = "primary",
                    value = F),
        selectInput("fill_gap_list",
                    "Select method:",
                    choices = c("linear", "average"),
                    multiple = F,
                    selectize = F),
        br(),
        br(),
        conditionalPanel(condition = "input.fill_gap_list == 'average'",
                         shinyWidgets::materialSwitch("fill_gap_scale", "Scale average by growh rate?",
                                                      status = "primary",
                                                      value = F),
                         selectInput("fill_gap_group",
                                     "Replace NAs within group:",
                                     choices = colnames(select(df_meta(), -.id)),
                                     multiple = T,
                                     selectize = F,
                                     selected = NULL)
        ),
        # numericInput("maxgap", "Max gap:", min = 2, max = 10000, step = 1, value = 10000),
        easyClose = T,
        footer = tagList(actionButton("ok_bar15", "Fill NAs", class = "btn-success"),
                         modalButton("Cancel")),
        size = "l"
      )))
    })
    observeEvent(input$ok_bar15, {
      if(input$fill_gap_list == "linear") {
        fill_gap_linear()
        d$b <- df
        removeModal()
      } else { if(input$fill_gap_list == "average" & length(input$.id)>=2){
        if(isTRUE(input$fill_gap_scale)){
          fill_gap_avg_scale()
        }else{
          fill_gap_avg()
        }
        d$b <- df
        removeModal()
      }else {
        removeModal()
        showModal(jqui_draggable(modalDialog(
          "Not enought data to provide this operation. Select more than one device.",
          easyClose = T,
          footer = tagList(modalButton("OK")),
          size = "l"
        )))
      }}
    })
    # data uploading----
    df_choices = function(){
      ch = as.vector(ls(envir = envir)[lapply(lapply(ls(envir = envir),get), is.data.frame) == TRUE])
      return(ch)
    }
    # remove stairs----
    remove_stairs_reactive = reactiveValues(input_data = NULL)
    observeEvent(input$remove_stair_action, {
      if(isFALSE(input$one_by_one_switch)){
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]) & isolate(d$a)[[".id"]] %in% input$one_by_one_group_select ,], input$RadiusPlot_brush, xvar = "date_time"))
      }else{
        r = row.names(brushedPoints(isolate(d$a)[complete.cases(isolate(d$a)[,input$variable_prim]),], input$RadiusPlot_brush, xvar = "date_time"))
      }
      remove_stairs_reactive$input_data = isolate(df[r,])
      remove_stairs_reactive$input_data = droplevels(remove_stairs_reactive$input_data)
      if(is.null(input$RadiusPlot_brush)|length(unique(remove_stairs_reactive$input_data[".id"]))!= 1){
        showModal(modalDialog(
          title = "Stair remover!",
          HTML('Brush data first!<br>
               Brush data from only one device!'),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }else{
        showModal(jqui_draggable(modalDialog(
          # render plot for stair
          plotOutput("stair_plot"),
          br(),
          br(),
          numericInput("stair_probs", "Probability:", min = 0, max = 1, step = .01, value = .99),
          easyClose = T,
          footer = tagList(actionButton("ok_bar19", "Remove stairs", class = "btn-danger"),
                           modalButton("Cancel")),
          size = "l"
        )))
          output$stair_plot <- renderPlot({
            stair_plot_data = remove_stair(remove_stairs_reactive$input_data, probs = input$stair_probs)
            ggplot(stair_plot_data, aes(date_time, stair_variable))+
              geom_line()+
              geom_point(data = stair_plot_data, aes(date_time, stair_points), colour = "red")+
              geom_line(data = stair_plot_data, aes(date_time, zoo::na.approx(stair_points, na.rm = F)), colour = "blue")+
              ylab(paste(input$variable_prim))+
              theme_bw()
          })
      }
    })
    observeEvent(input$ok_bar19, {
      stair_data_sub = remove_stair(remove_stairs_reactive$input_data, probs = input$stair_probs)
      stair_data_sub = stair_data_sub %>% select(-stair_variable) %>% droplevels()
      stair_data_sub = plyr::rename(stair_data_sub, c("stair_points" = input$variable_prim))
      df = df %>% filter(!(.id %in% levels(stair_data_sub$.id) & date_time %in% stair_data_sub$date_time)) %>%
        dplyr::bind_rows(stair_data_sub) %>% dplyr::arrange(.id, date_time) %>% as.data.frame()
      stair_data_sub = stair_data_sub %>% select(.id, date_time, input$variable_prim)
      if("stair_data_removed" %in% ls(envir = envir)){
        print("yes")
        stair_data_removed = get("stair_data_removed", envir = envir)
        stair_data_removed = stair_data_removed %>% bind_rows(stair_data_sub)
      } else {
        print("no")
        stair_data_removed = stair_data_sub
      }
      assign("stair_data_removed", stair_data_removed, envir = envir)
      rm(stair_data_sub)
      assign('df', df, envir=envir)
      d$b <- df
      # remove_stairs_reactive$input_data = NULL
      removeModal()
    })
    remove_stair = function(stair_data, probs = 0.99){
      stair_data = stair_data %>% as.data.frame() %>% dplyr::rename(stair_variable = input$variable_prim) %>%
        mutate(jump = stair_variable - lag(stair_variable))
      upper_limit = as.numeric(stats::quantile(stair_data$jump, na.rm = T, probs = probs))
      stair_data = stair_data %>% mutate(jump = if_else(jump > upper_limit, jump, NA)) %>%
        mutate(group = cumsum(is.na(jump)) + !is.na(jump)) %>%
        mutate(group = if_else(is.na(jump), NA, group)) %>%
        group_by(group) %>%
        mutate(group = if_else(row_number() == n(), group, NA)) %>% ungroup() %>%
        mutate(group = if_else(row_number() == 1, stair_variable, group)) %>%
        mutate(group = if_else(row_number() == n(), stair_variable, group)) %>%
        mutate(stair_points = if_else(is.na(group), NA, stair_variable)) %>% select(-group, -jump) %>%
        as.data.frame()
      return(stair_data)
    }
    observeEvent(input$df_uploader, {
      showModal(jqui_draggable(modalDialog(
        selectInput("df_list",
                    "Data frames:",
                    choices = df_choices(),
                    multiple = F,
                    selectize = F,
                    selected = df_choices()[1]),
        renderDataTable({
          datatable(head(get(input$df_list)),options=list(scrollX=TRUE))
        }),
        easyClose = T,
        footer = tagList(actionButton("df_upload", "Upload", class = "btn-success"),
                         modalButton("Cancel")),
        size = "l"
      )))
    })
    observeEvent(input$df_upload,{
      if(".id" %in% colnames(get(input$df_list))){
        d$b <-get(input$df_list)
        assign('df', as.data.frame(d$b), envir = envir)
        updateCheckboxInput(session, "bar", value = F)
        activator = TRUE
        removeModal()
      }else{
        showModal(jqui_draggable(modalDialog(
          selectInput(".id_list",
                      "Choose a factor column to sort your devices:",
                      choices = colnames(Filter(is.factor, get(input$df_list)))
          ),
          easyClose = T,
          footer = tagList(actionButton(".id_change", "Select", class = "btn-success"),
                           modalButton("Cancel"))
        )))
      }
    })
    observeEvent(input$.id_change,{
      df_temporary = get(input$df_list)
      df_temporary = as.data.frame(df_temporary %>% dplyr::rename(.id = input$.id_list) %>% dplyr::mutate(.id = as.factor(.id)))
      assign('df', as.data.frame(df_temporary), envir = envir)
      d$b <-df
      rm(df_temporary)
      updateCheckboxInput(session, "bar", value = F)
      removeModal()
    })
    observeEvent(input$tomst_uploader, {
      showModal(modalDialog(
        selectInput("time_reso_input", "Time resolution:", choices = c("auto","1 min", "5 min", "15 min", "1 hour")),
        shiny::fileInput("select_files", "Choose CSV Files", accept = ".csv", multiple = T),
        selectInput(inputId = "select_units", label = "Dendrometer data units?", selected = NULL,
                    selectize =  F, choices = c("auto", "tomst", "micrometers")),
        selectInput(inputId = "select_tms_calib", label = "TMS calibration?", selected = "Loamy_Sand_A",
                    selectize =  F, choices = c("Loamy_Sand_A", "Loamy_Sand_B","Sandy_Loam_A","Sandy_Loam_B","Loam","Sil_Loam", "Peat", "Sand", "none")),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("ok_bar14", "OK")
        )))
    })
    observeEvent(input$ok_bar14,{
      removeModal()
      files_selected <- input$select_files
      shiny::req(files_selected)
      files_list = unlist(regmatches(files_selected$name, gregexpr("data.+csv$", files_selected$name)))
      files_selected = files_selected %>% filter(name %in% files_list) %>% as.data.frame()
      if(length(files_selected$name) == 0){
        showModal(modalDialog(
          HTML("No files in appropriate format. You can try read_tomst function or another importer."),
          easyClose = TRUE,
          footer = modalButton("OK"))
        )
      } else {
        shiny::withProgress(
          message = paste0("Processing..."),
          detail = "Choosing directory",
          value = 0,
          {
            shiny::incProgress(1/10, detail = "Files uploading")
            TMS = list()
            for(i in files_selected$datapath) {
              x <- read_delim(i, delim =";",
                              col_names = F,
                              col_types = cols(.default = "c"))
              if(length(x)<7){
                NULL
              }else{
                x<-as.data.frame(x)[c(2,4,5,6,7)]
                name_tms = files_selected %>% filter(datapath == i) %>% pull(name)
                TMS[[name_tms]] <-x
              }
            }
            if(length(TMS) == 0){
              showModal(modalDialog(
                HTML("No files in appropriate format"),
                easyClose = TRUE,
                footer = modalButton("OK"))
              )
            }else{
              shiny::incProgress(1/10, detail = "Assigning to list")
              TMS2 = list()
              for(i in unique(substr(names(TMS),6,13))) {
                x <- dplyr::bind_rows(TMS[(grep(names(TMS),pattern = i, value = T))])
                TMS2[[i]] <-x
              }
              shiny::incProgress(1/10, detail = "Assigning to df")
              df = plyr::ldply(TMS2, data.frame)
              rm(TMS, TMS2)
              colnames(df) = c(".id", "date_time", "T1", "T2", "T3", "Moisture")
              delim = Sys.localeconv()["decimal_point"]
              if(delim == ","){
                df = df %>% dplyr::mutate(across(c(T1, T2, T3, Moisture), function(x) gsub("\\.", paste0(delim), x))) %>% as.data.frame()
              }else{
                df = df %>% dplyr::mutate(across(c(T1, T2, T3, Moisture), function(x) gsub(",", paste0(delim), x))) %>% as.data.frame()
              }
              df = subset(df, !is.na(date_time))
              df$date_time = parse_date_time(df$date_time , c("YmdHM", "dmYHM", "YmdHMS", "dmYHMS"), tz = 'UTC')
              df = subset(df, date_time > "1990-01-01 00:00:00 UTC")
              shiny::incProgress(2/10, detail = "Unique data")
              df = df %>% group_by(.id) %>% dplyr::mutate(first_occurence = first(date_time)) %>% group_by(first_occurence) %>% dplyr::arrange(.id, date_time, .by_group = T) %>% ungroup() %>% dplyr::select(-first_occurence) %>% dplyr::distinct(.id, date_time, .keep_all = T) %>% as.data.frame()
              df[c(".id", "Moisture")] = lapply(df[c(".id", "Moisture")], as.numeric)
              if(input$select_units == "micrometers" | is.null(input$select_units)){
                df$Radius = ifelse(df$.id > 93000000, NA, df$Moisture)
              }else{
                df$Radius = ifelse(df$.id > 93000000, NA, (df$Moisture-1278)*(8890/(34000-1278)))
              }
              df$Moisture = ifelse(df$.id >= 93000000 | df$.id %in% c(90181123, 91181123), df$Moisture, NA)
              # Soil type calibration lines----
              # Soil type start
              tms_calib_data = list('Loamy_Sand_A' = matrix(data = c(-1.90e-8, 2.66e-4, -1.54e-1), nrow = 1, ncol = 3, dimnames = list(c("Loamy_Sand_A"), c("a", "b", "c"))),
                                    'Loamy_Sand_B' = matrix(data = c(-2.30E-8, 2.82E-4, -1.67E-1), nrow = 1, ncol = 3, dimnames = list(c("Loamy_Sand_B"), c("a", "b", "c"))),
                                    'Sandy_Loam_A' = matrix(data = c(-3.80E-8, 3.39E-4, -2.15E-1), nrow = 1, ncol = 3, dimnames = list(c("Sandy_Loam_A"), c("a", "b", "c"))),
                                    'Sandy_Loam_B' = matrix(data = c(-9.00E-10, 2.62E-4, -1.59E-1), nrow = 1, ncol = 3, dimnames = list(c("Sandy_Loam_B"), c("a", "b", "c"))),
                                    'Loam' = matrix(data = c(-5.10E-8, 3.98E-4, -2.91E-1), nrow = 1, ncol = 3, dimnames = list(c("Loam"), c("a", "b", "c"))),
                                    'Sil_Loam' = matrix(data = c(1.70E-8, 1.18E-4, -1.01E-1), nrow = 1, ncol = 3, dimnames = list(c("Sil_Loam"), c("a", "b", "c"))),
                                    'Peat' = matrix(data = c(1.23E-7, -1.45E-4, 2.03E-1), nrow = 1, ncol = 3, dimnames = list(c("Peat"), c("a", "b", "c"))),
                                    'Sand' = matrix(data = c(-3.00E-9, 1.61E-4, -1.10E-1), nrow = 1, ncol = 3, dimnames = list(c("Sand"), c("a", "b", "c"))))
              if(is.null(input$select_tms_calib) | input$select_tms_calib == "none"){
                # Moisture is not recalculated into volumetric soil moisture
                showNotification(paste0("Raw TMS data were not recalculated."))
              }else{
                df$Moisture = (tms_calib_data[[input$select_tms_calib]][1]*df$Moisture^2+tms_calib_data[[input$select_tms_calib]][1]*df$Moisture+tms_calib_data[[input$select_tms_calib]][1])*100
                showNotification(paste0("TMS volumetric soil moisture according to ", input$select_tms_calib))
                }
              # Soil type end
              df$T1 = as.numeric(df$T1)
              df$T2 = as.numeric(ifelse(df$.id >= 93000000, df$T2, NA))
              df$T3 = as.numeric(ifelse(df$.id >= 93000000, df$T3, NA))
              df$.id = as.factor(df$.id)
              df = droplevels(df)
              shiny::incProgress(2/10, detail = "Arranging")
              if(input$time_reso_input == "auto"){
                resol = df %>% select(.id, date_time) %>% group_by(.id) %>%
                  mutate(time_diff = difftime(date_time, lag(date_time), units = "mins") ) %>%
                  filter(!is.na(time_diff)) %>%
                  dplyr::mutate(tot = n()) %>%
                  group_by(time_diff, .add = T) %>%
                  dplyr::mutate(per = n()) %>%
                  dplyr::distinct(.id, time_diff, .keep_all = T) %>%
                  dplyr::summarise(time_diff_per = (per/tot)*100) %>%
                  dplyr:: filter(time_diff_per == max(time_diff_per, na.rm = T)) %>% select(-time_diff_per) %>% as.data.frame
                df = df %>% group_by(.id) %>%
                  tidyr::complete(date_time = seq.POSIXt(min(date_time), max(date_time), by=as.difftime(resol$time_diff[1], units = "mins") ,tz = 'UTC')) %>%
                  dplyr::arrange(date_time, .by_group = T) %>% as.data.frame()
                rm(resol)
              } else {
                df = df %>% group_by(.id) %>%
                  tidyr::complete(date_time = seq.POSIXt(min(date_time), max(date_time), by=input$time_reso_input ,tz = 'UTC')) %>%
                  dplyr::arrange(date_time, .by_group = T) %>% as.data.frame()
              }
              df = df %>% select(where(~!all(is.na(.x)))) %>% as.data.frame()
              if(any(colnames(df) %in% "Radius")){
                if(input$select_units == "tomst"){
                  if(any(na.omit(df$Radius < -2.173461 & round(df$Radius,4) != -277.9314))){
                    showNotification(paste0("Data contains low values. Check whether data were truly downloaded in ", input$select_units, " units."), type = "warning")
                    message(paste0("Data contains low values. Check whether data were truly downloaded in ", input$select_units, " units."))
                    }else{
                      showNotification(paste0(input$select_units, " units were converted into micrometers and used for Radius calculation."))
                      message(paste0(input$select_units, " units were converted into micrometers and used for Radius calculation."))
                      }
                }else{
                    if(any(na.omit(df$Radius < 0))){
                      showNotification(paste0(input$select_units, " were used for Radius calculation."))
                      showNotification(paste0("Data contains low values. Check Radius data."), type = "warning")
                      message(paste0(input$select_units, " were used for Radius calculation."))
                      message(paste0("Data contains low values. Check Radius data."))
                    }else{
                      showNotification(paste0(input$select_units, " were used for Radius calculation."))
                      message(paste0(input$select_units, " were used for Radius calculation."))
                    }
                }
              }
              df = df %>% dplyr::arrange(.id, date_time) %>% group_by(.id) %>% as.data.frame()
              shiny::incProgress(2/10, detail = "Assigning to env")
              assign('df', df, envir = envir)
              d$b = df
              shiny::incProgress(1/10, detail = "Done!")
            }
          })
      }
    })

    #error warnings----
    dataerrors <- reactive({
      tryCatch({print(plot_appear_2())
      }, message = function(e) {
        return(e$message)
      }, warning = function(e) {
        return(e$message)
      }, error = function(e) {
        return(e$message)
      })
    })
    output$ggplot_warnings <- renderPrint({
      dataerrors()
    })
    auto_save = reactive({
      if(isTRUE(input$Auto_save_switch)){
        DateNext = function(){
          name = paste(Sys.Date(),".RData", sep="")
          name2 = paste(Sys.Date())
          if(!file.exists(name)){return(name)}
          else {
            i=1
            repeat {
              f = paste(paste(name2,i,sep="_"),".RData", sep="")
              if(!file.exists(f)){return(f)}
              i=i+1
            }
          }
        }
        message("workspace backup running, please wait")
        save(list= 'df', file = DateNext())
        message("done")
      } else {
        NULL
      }
    })
    #end server
    session$onSessionEnded(function(){
      stopApp()
      isolate(auto_save())
    })
  }
  shinyApp(ui, server, options=list(launch.browser = T))
  #end shiny app
}
