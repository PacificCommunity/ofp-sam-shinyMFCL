# General Shiny app to compare individual (e.g., stepwise) models
# 2022-04-21  Finlay Scott created for SKJ stepwise development
# 2022-09-01  Arni Magnusson adapted for YFT review
# 2023-04-07  Arni Magnusson and Jemery Day adapted for YFT and BET

# CRAN packages
library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)
library(markdown)
library(DT)
library(RColorBrewer)
library(viridis)
library(gplots)

default_models <- c("Diagnostic")
default_fishery <- "PS ASS"
rec_units <- 1000000
sb_units <- 1000
copyright.year <- 2023
shiny.title <- "BET stepwise 2023"

#---------------------------------------------------------------------------

spc_about <- function(){
  out <- tags$html(
    tags$p(style="opacity: 0.5", class="caption", align="center", HTML("&copy"), paste("Pacific Community,", copyright.year)),
    tags$p(align="justify", "The Pacific Community (SPC) is the principal scientific and technical organisation in the Pacific region, proudly supporting development since 1947. It is an international development organisation owned and governed by its 27 country and territory members. The members are: American Samoa, Australia, Cook Islands, Federated States of Micronesia, Fiji, France, French Polynesia, Guam, Kiribati, Marshall Islands, Nauru, New Caledonia, New Zealand, Niue, Northern Mariana Islands, Palau, Papua New Guinea, Pitcairn Islands, Samoa, Solomon Islands, Tokelau, Tonga, Tuvalu, United Kingdom, United States of America, Vanuatu, and Wallis and Futuna."),
    tags$p(align="justify", "In pursuit of sustainable development to benefit Pacific people, this unique organisation works across more than 25 sectors. SPC is renowned for its knowledge and innovation in such areas as fisheries science, public health surveillance, geoscience, and conservation of plant genetic resources for food and agriculture."),
    tags$p(align="justify", "Much of SPC's focus is on major cross-cutting issues, such as climate change, disaster risk management, food security, gender equality, human rights, non-communicable diseases, and youth employment. Using a multi-sector approach in responding to its members' development priorities, SPC draws on skills and capabilities from around the region and internationally, and supports the empowerment of Pacific communities and sharing of expertise and skills between countries and territories."),
    tags$p(align="justify", "With over 600 staff, SPC has its headquarters in Noumea, regional offices in Suva and Pohnpei, a country office in Honiara and field staff in other Pacific locations. Its working languages are English and French. See: ", a("https://www.spc.int", href="https://www.spc.int"))
  )
  return(out)
}

#---------------------------------------------------------------------------
# Load data
# Get the fishery map - generated in ../R/fisheries_map.R
load("data/fishery_map.RData")

# Load the data - generated using the app_data_preparation.R script
# Data for catch size distribution plots
load("data/lfits_dat.RData")
# Movement data
load("data/move_coef.RData")
# Other stuff
other_data_files <- load("data/other_data.RData")
# Tag stuff
tag_data_files <- load("data/tag_data.RData")
# Likelihood table
ll_tab_data_files <- load("data/ll_tab_data.RData")

# Shave off topdir/* only if the model name contains a forward slash
# Example: TopDir/ThisModel -> ThisModel, and ThatModel -> ThatModel
lfits_dat[, model := gsub(".*/", "", model)]
move_coef[, model := gsub(".*/", "", model)]
biomass_dat[, model := gsub(".*/", "", model)]
cpue_dat[, model := gsub(".*/", "", model)]
m_dat[, model := gsub(".*/", "", model)]
mat_age_dat[, model := gsub(".*/", "", model)]
mat_length_dat[, model := gsub(".*/", "", model)]
rec_dev_dat[, model := gsub(".*/", "", model)]
sel_dat[, model := gsub(".*/", "", model)]
srr_dat[, model := gsub(".*/", "", model)]
srr_fit_dat[, model := gsub(".*/", "", model)]
status_tab_dat[, Model := gsub(".*/", "", Model)]
tag_attrition[, model := gsub(".*/", "", model)]
tag_returns_prop[, model := gsub(".*/", "", model)]
tag_returns_time[, model := gsub(".*/", "", model)]
ll_tab_dat[, Model := gsub(".*/", "", Model)]

# Format the LLhood data
ll_tab_dat[, `Recruitment` := sprintf("%.2f",`Recruitment`)]
ll_tab_dat[, `Effort_devs` := NULL]
ll_tab_dat[, `Catchability_devs` := NULL]
ll_tab_dat[, `Total` := NULL]
ll_tab_dat[, `ObjFun` := round(`ObjFun`)]
ll_tab_dat[, `CPUE` := round(`CPUE`)]
ll_tab_dat[, `Length` := round(`Length`)]
ll_tab_dat[, `Weight` := round(`Weight`)]
ll_tab_dat[, `Age` := round(`Age`)]
ll_tab_dat[, `Tags` := round(`Tags`)]
ll_tab_dat[, `Penalties` := round(`Penalties`)]
ll_tab_dat[, `Gradient` := sprintf("%.5f", `Gradient`)]
# Format the status table
status_tab_dat[, `Final SB/SBF0latest` := .(signif(`Final SB/SBF0latest`, 3))]
status_tab_dat[, `SB/SBF0 (2012)` := .(signif(`SB/SBF0 (2012)`, 3))]
status_tab_dat[, `FMSY` := .(signif(`FMSY`, 3))]

#---------------------------------------------------------------------------
# App options
start_collapsed <- TRUE

# Information for building the selectors
all_models <- unique(biomass_dat$model)
fishgrp_names <- unique(fishery_map$group)

# Number of models and regions
nmodels <- length(all_models)
nregions <- length(unique(fishery_map$region))

#---------------------------------------------------------------------------
# The app
# https://stackoverflow.com/questions/31711307/how-to-change-color-in-shiny-dashboard

# Adding logos and loading bars
#https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header

ui <- dashboardPage(
  header = dashboardHeader(title=shiny.title),
  sidebar = dashboardSidebar(
    br(),
    br(),
    sidebarMenu(id="sidebarmenu",
      menuItem("Introduction", tabName="introduction", icon=icon("wine-bottle")),
      menuItem("Fitting diagnostics", tabName="diagnostics", icon=icon("wine-glass-alt")),
      menuItem("Fits to data", tabName="fittodata", icon=icon("cocktail")),
      menuItem("Model outputs", tabName="modeloutput", icon=icon("beer")),
      menuItem("Stock status", tabName="stockstatus", icon=icon("glass-whiskey")),
      menuItem("About", tabName="about", icon=icon("glass-martini-alt"))
    ),

    # Only show these on the plotting tabs - not Introduction and About tabs
    conditionalPanel(condition="input.sidebarmenu == 'diagnostics' || input.sidebarmenu == 'fittodata' || input.sidebarmenu == 'modeloutput' || input.sidebarmenu == 'stockstatus'",
      # Model selection - select multiple
      checkboxGroupInput(inputId="model_select", label="Select models", choiceNames=all_models, choiceValues=all_models, selected=default_models),
      # Fishery grouping selection - only single
      radioButtons(inputId="fishery_group", label="Fishery / Tag recapture groups", choiceNames=fishgrp_names, choiceValues=fishgrp_names, selected=default_fishery)
    ),
    br(),
    br(),
    tags$footer(
      div(style="text-align:center",
        tags$p("version 0.0.1 The Cosmic Barrilete"),
        tags$p(paste("Copyright", copyright.year, "OFP SPC MSE Team"))
      )
    )
  ), # End of sidebar

  body = dashboardBody(
    tags$head(tags$style(HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}') )),

    # Start of main tab stuff
    tabItems(
      # **** Introduction ****
      tabItem(tabName="introduction", h2("Introduction"),
        fluidRow(column(12, includeMarkdown("introtext/introduction.md"))),
        fluidRow(column(12, includeMarkdown("introtext/models.md")))
      ), # End of introduction tab

      # **** Fitting diagnostics ****
      tabItem(tabName="diagnostics", h2("Fitting diagnostics"),
        fluidRow(box(title="Likelihood components and gradients", collapsed=start_collapsed, solidHeader=TRUE, collapsible=TRUE, status="primary", width=12, DTOutput("llhood_table")))
      ), # End of diagnostics tab

      # **** Fits to data ****
      tabItem(tabName="fittodata", h2("Fits to data sources"),
        fluidRow(
          box(title="Catch size distribution", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            p("Bars are the observations. Lines are the model predictions."),
            plotOutput("plot_catch_size_dist", height="auto"))
        ),
        fluidRow(
          box(title="Tag returns: time series", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            p("Dots are the observations. Lines are the model predictions."),
            plotOutput("plot_tag_returns_time", height="800px"))
        ),
        fluidRow(
          box(title="Tag returns: residuals time series", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            plotOutput("plot_tag_returns_residuals_time", height="800px"))
        ),
        fluidRow(
          box(title="CPUE: time series", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            p("Dots are the observed CPUE. Lines are the predicted CPUE."),
            plotOutput("plot_cpue_time", height="800px"))
        ),
        fluidRow(
          box(title="CPUE: residuals time series", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            plotOutput("plot_cpue_residuals_time", height="800px"))
        ),
        fluidRow(
          box(title="Tag attrition", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            p("Dots are the observations. Lines are the model predictions."),
            # Grouping selector
            radioButtons(inputId="tag_attrition", label="Tag attrition grouping", choiceNames=c("Combined", "Region", "Program"), choiceValues=c("combined", "region", "program"), selected="combined", inline=TRUE),
            plotOutput("plot_tag_attrition", height="600px"))
        ),
        fluidRow(
          box(title="Tag attrition: residuals", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            # Grouping selector
            radioButtons(inputId="tag_attrition_residuals", label="Tag attrition grouping", choiceNames=c("Combined", "Region", "Program"), choiceValues=c("combined", "region", "program"), selected="combined", inline=TRUE),
            plotOutput("plot_tag_attrition_residuals", height="600px"))
        )
      ), # End of fittodata tab

      # **** Model outputs ****
      tabItem(tabName="modeloutput", h2("Model outputs"),
        fluidRow(
          box(title="Movement", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            plotOutput("plot_movement", height="800px"))
        ),
        fluidRow(
          box(title="SRR", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            plotOutput("plot_srr", height="500px"))
        ),
        fluidRow(
          box(title="Total recruitment distribution", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            plotOutput("plot_rec_dist", height="500px"))
        ),
        fluidRow(
          box(title="Recruitment deviates", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            plotOutput("plot_rec_devs", height="500px"))
        ),
        fluidRow(
          box(title="Selectivity", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            # Scale selector
            radioButtons(inputId="age_select_sel", label="By age or length", choiceNames=c("Age", "Length"), choiceValues=c("age", "length"), selected="age", inline=TRUE),
            plotOutput("plot_selectivity", height="500px"))
        ),
        fluidRow(
          box(title="Natural mortality", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            plotOutput("plot_natmort", height="500px"))
        ),
        fluidRow(
          box(title="Growth", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            plotOutput("plot_growth", height="500px"))
        ),
        fluidRow(
          box(title="Maturity", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            # Scale selector
            radioButtons(inputId="age_select_mat", label="By age or length", choiceNames=c("Age", "Length"), choiceValues=c("age", "length"), selected="age", inline=TRUE),
            plotOutput("plot_maturity", height="500px"))
        )
      ), # End of modeloutput tab

      # **** Stock status ****
      tabItem(tabName="stockstatus", h2("Stock status"),
        fluidRow(
          box(title="Recruitment", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            # Area selector
            column(6, radioButtons(inputId="area_select_recruitment", label="Region selector", choiceNames=c("Separate", "Combined"), choiceValues=c("separate", "combined"), selected="combined", inline=TRUE)),
            # Scale selector
            column(6, radioButtons(inputId="scale_select_recruitment", label="Different scales?", choiceNames=c("Yes", "No"), choiceValues=c(TRUE, FALSE), selected=TRUE, inline=TRUE)),
            plotOutput("plot_rec", height="500px"))
        ),
        fluidRow(
          box(title="Depletion", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=8,
            # Area selector
            radioButtons(inputId="area_select_sbsbf0", label="Region selector", choiceNames=c("Separate", "Combined"), choiceValues=c("separate", "combined"), selected="combined", inline=TRUE),
            plotOutput("plot_sbsbf0", height="500px"))
        ),
        fluidRow(
          box(title="Spawning biomass", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=8,
            # Area selector
            column(6, radioButtons(inputId="area_select_sb", label="Region selector", choiceNames=c("Separate", "Combined"), choiceValues=c("separate", "combined"), selected="combined", inline=TRUE)),
            # Scale selector
            column(6, radioButtons(inputId="scale_select_sb", label="Different scales?", choiceNames=c("Yes", "No"), choiceValues=c(TRUE, FALSE), selected=TRUE, inline=TRUE)),
            plotOutput("plot_sb", height="500px"))
        ),
        fluidRow(
          box(title="Unfished biomass", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=8,
            # Area selector
            column(6, radioButtons(inputId="area_select_sbf0", label="Region selector", choiceNames=c("Separate", "Combined"), choiceValues=c("separate", "combined"), selected="combined", inline=TRUE)),
            # Scale selector
            column(6, radioButtons(inputId="scale_select_sbf0", label="Different scales?", choiceNames=c("Yes", "No"), choiceValues=c(TRUE, FALSE), selected=TRUE, inline=TRUE)),
            plotOutput("plot_sbf0", height="500px"))
        ),
        fluidRow(
          box(title="Stock status summary", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12, DTOutput("status_table"))
        ),
      ), # End of stockstatus tab

      # **** About SPC ****
      tabItem(tabName="about", h2("About SPC"),
          fluidRow(column(12, spc_about()))
      ) # End of about tab
    ) # End of tabItems
  ) # End of dashboardBody
)

#---------------------------------------------------------------------------
# Server function

server <- function(input, output){
  # Pixel height for each fishery plot. i.e row height when plotting fisheries by row
  height_per_fishery <- 250

  # Colour palette for the fisheries
  get_model_colours <- function(all_model_names, chosen_model_names){
    nmodels <- length(all_model_names)
    # Palette matching Figure 14b from YFT 2020 report
    all_cols <- c("grey65","royalblue3","deepskyblue1","gold","orange1","indianred1","firebrick2","#AC2020","black")
#    all_cols <- c("black", rich.colors(nmodels-1))
#    all_cols <- c(brewer.pal(n=nmodels-1, "Set1"), "black")
#    all_cols <- c(brewer.pal(n=nmodels-1, "Greens"), "black")
#    all_cols <- c(brewer.pal(n=nmodels-1, "YlOrRd"), "black")
#    all_cols <- c(brewer.pal(n=nmodels-1, "Spectral"), "black")
#    all_cols <- c(brewer.pal(n=nmodels, "Spectral"))
#    all_cols <- viridis(nmodels)
    all_cols <- c(rainbow(nmodels))
    all_cols[nmodels] <-"black"
    names(all_cols) <- all_model_names
    model_cols <- all_cols[as.character(chosen_model_names)]
    return(model_cols)
  }

  nice_blue <- "steelblue1"
  obs_col <- "steelblue1" # Colours for observed data
  nice_red <- "tomato3"

  output$llhood_table <- renderDT({
    # dom option drops the search and other stuff
    ll_tab_dat <- datatable(ll_tab_dat, options=list(pageLength=nmodels, dom='t'), rownames=FALSE)
    if(length(input$model_select) > 0){
      ll_tab_dat <- ll_tab_dat %>% formatStyle('Model', target='row', backgroundColor=styleEqual(input$model_select,'yellow'))
    }
    return(ll_tab_dat)
  })

  output$plot_catch_size_dist <- renderPlot({
    # Which models and fisheries
    models <- input$model_select
    fisheries <- fishery_map[fishery_map$group %in% input$fishery_group, "fishery"]
    if(length(models) < 1 || length(fisheries) < 1){
      return()
    }
    pdat <- lfits_dat[fishery %in% fisheries & model %in% models]
    if(nrow(pdat) == 0){
      return()
    }
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    bar_width <- diff(unique(sort(pdat$length)))[1]
    # Assume that the observed (the bars) is the same for all models
    p <- ggplot(pdat[model==last(model)], aes(x=length))
    # Observed as barchart
    p <- p + geom_bar(aes(y=obs), fill=obs_col, colour="black", stat="identity", width=bar_width)
    # Predicted as red line
    p <- p + geom_line(data=pdat, aes(y=pred, colour=model), size=1.25)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + facet_wrap(~fishery_name, scales="free", ncol=2)
    p <- p + xlab("Length (cm)") + ylab("Samples")
    p <- p + theme_bw()
    return(p)
  },
  height=function(){
    ncol <- 2  # should match that in the main plot function
    fisheries <- fishery_map[fishery_map$group %in% input$fishery_group, "fishery"]
    return(max(height_per_fishery*1.5, (height_per_fishery * ceiling(length(fisheries) / ncol))))
  })

  output$plot_tag_returns_time <- renderPlot({
    models <- input$model_select
    tag_groups <- fishery_map[fishery_map$group %in% input$fishery_group, "tag_recapture_group"]
    if(length(models) < 1 || length(tag_groups) < 1){
      return()
    }
    pdat <- tag_returns_time[tag_recapture_group %in% tag_groups & model %in% models]
    if(nrow(pdat) == 0){
      return()
    }
    # Assume that the observed are the same across the models
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=recap.ts, y=recap.obs))
    p <- p + geom_point(colour=obs_col, na.rm=TRUE, size=3)
    p <- p + geom_line(aes(y=recap.pred, colour=model), na.rm=TRUE, size=1.25)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + facet_wrap(~tag_recapture_name, scales="free")
    p <- p + xlab("Time") + ylab("Tag recaptures")
    p <- p + theme_bw()
    return(p)
  })

  output$plot_tag_returns_residuals_time <- renderPlot({
    models <- input$model_select
    fishery_groups <- input$fishery_group
    tag_groups <- fishery_map[fishery_map$group %in% fishery_groups, "tag_recapture_group"]
    if(length(models) < 1 || length(tag_groups) < 1){
      return()
    }
    pdat <- tag_returns_time[tag_recapture_group %in% tag_groups & model %in% models]
    if(nrow(pdat) == 0){
      return()
    }
    # Assume that the observed are the same across the models
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=recap.ts, y=diff))
    p <- p + geom_smooth(aes(colour=model), method='loess', formula='y~x', na.rm=TRUE, se=FALSE)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + facet_wrap(~tag_recapture_name, scales="free")
    p <- p + geom_hline(aes(yintercept=0.0), linetype=2)
    p <- p + xlab("Time") + ylab("Obs. - pred. recaptures (scaled)")
    p <- p + theme_bw()
    return(p)
  })

  output$plot_cpue_time <- renderPlot({
    models <- input$model_select
    if(length(models) < 1){
      return()
    }
    # Already subset out index fisheries in the data creation step
    pdat <- cpue_dat[model %in% models]
    if(nrow(pdat) == 0){
      return()
    }
    # Bring in the fishery names
    pdat <- merge(pdat, fishery_map[,c("fishery", "fishery_name")], by="fishery")
    # Observed is not the same across models
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=ts, y=cpue_obs))
    p <- p + geom_point(colour=obs_col, na.rm=TRUE, size=3)
    p <- p + geom_line(aes(y=cpue_pred, colour=model), na.rm=TRUE, size=1.25)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + facet_grid(fishery_name~model, scales="free")
    p <- p + xlab("Time") + ylab("CPUE")
    p <- p + theme_bw()
    return(p)
  })

  output$plot_cpue_residuals_time <- renderPlot({
    models <- input$model_select
    if(length(models) < 1){
      return()
    }
    # Already subset out index fisheries in the data creation step
    pdat <- cpue_dat[model %in% models]
    if(nrow(pdat) == 0){
      return()
    }
    # Bring in the fishery names
    pdat <- merge(pdat, fishery_map[,c("fishery", "fishery_name")], by="fishery")
    # Observed is not the same across models
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=ts, y=scale_diff))
    p <- p + geom_smooth(aes(colour=model), method='loess', formula='y~x', na.rm=TRUE, se=FALSE)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + facet_wrap(~fishery_name, scales="free")
    p <- p + geom_hline(aes(yintercept=0.0), linetype=2)
    p <- p + xlab("Time") + ylab("Obs. - pred. CPUE (scaled)")
    p <- p + theme_bw()
    return(p)
  })

  output$plot_tag_attrition <- renderPlot({
    models <- input$model_select
    if(length(models) < 1){
      return()
    }
    facet <- input$tag_attrition
    # Grouping by user choice
    if(facet == "combined"){
      grouping_names <- c("model", "period_at_liberty")
    }
    if(facet == "program"){
      grouping_names <- c("model", "period_at_liberty", "program")
    }
    if(facet == "region"){
      grouping_names <- c("model", "period_at_liberty", "region")
    }
    pdat <- tag_attrition[model %in% models, .(recap.obs=sum(recap.obs, na.rm=TRUE), recap.pred=sum(recap.pred, na.rm=TRUE), diff=sum(diff, na.rm=TRUE)), by=mget(grouping_names)]
    if(facet %in% c("combined", "region")){
      pdat[,"program" := "All programs"]
    }
    if(facet %in% c("combined", "program")){
      pdat[,"region" := "All recapture regions"]
    }
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=period_at_liberty))
    p <- p + geom_point(aes(y=recap.obs), colour=obs_col, size=3)
    p <- p + geom_line(aes(y=recap.pred, colour=model), size=1.25)
    p <- p + scale_colour_manual("Model", values=model_cols)
    if(facet == "program"){
      p <- p + facet_wrap(~program, scales="free")
    }
    if(facet == "region"){
      p <- p + facet_wrap(~region, scales="free")
    }
    p <- p + xlab("Periods at liberty (quarters)")
    p <- p + ylab("Number of tag returns")
    p <- p + ylim(c(0, NA))
    p <- p + theme_bw()
    return(p)
  })

  output$plot_tag_attrition_residuals <- renderPlot({
    models <- input$model_select
    if(length(models) < 1){
      return()
    }
    facet <- input$tag_attrition_residuals
    # Grouping by user choice
    if(facet == "combined"){
      grouping_names <- c("model", "period_at_liberty")
    }
    if(facet == "program"){
      grouping_names <- c("model", "period_at_liberty", "program")
    }
    if(facet == "region"){
      grouping_names <- c("model", "period_at_liberty", "region")
    }
    pdat <- tag_attrition[model %in% models, .(recap.obs=sum(recap.obs, na.rm=TRUE), recap.pred=sum(recap.pred, na.rm=TRUE), diff=sum(diff, na.rm=TRUE)), by=mget(grouping_names)]
    # To scale the difference don't group by period at liberty - keep other choices
    grouping_names <- grouping_names[grouping_names!="period_at_liberty"]
    # This is for the residuals plot - the next one
    mean_recaptured <- pdat[,.(mean_obs_recap=mean(recap.obs, na.rm=TRUE)), by=mget(grouping_names)]
    pdat <- merge(pdat, mean_recaptured)
    pdat[, diff := diff / mean_obs_recap]
    if(facet %in% c("combined", "region")){
      pdat[,"program" := "All programs"]
    }
    if(facet %in% c("combined", "program")){
      pdat[,"region" := "All recapture regions"]
    }
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=period_at_liberty, y=diff))
    p <- p + geom_smooth(aes(colour=model), method='loess', formula='y~x', na.rm=TRUE, se=FALSE, size=1.25)
    p <- p + scale_color_manual("Model", values=model_cols)
    p <- p + geom_hline(aes(yintercept=0.0), linetype=2)
    if(facet == "program"){
      p <- p + facet_wrap(~program, scales="free")
    }
    if(facet=="region"){
      p <- p + facet_wrap(~region, scales="free")
    }
    p <- p + xlab("Periods at liberty (quarters)")
    p <- p + ylab("Obs. - pred. recaptures (scaled)")
    p <- p + theme_bw()
    return(p)
  })

  output$plot_movement <- renderPlot({
    models <- input$model_select
    if(length(models) < 1){
      return()
    }
    pdat <- subset(move_coef, Age==1 & model %in% models)
    pdat$From <- paste0("From ", pdat$From)
    pdat$To <- paste0("To ", pdat$To)
    if(nrow(pdat) == 0){
      return()
    }
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=Season, y=value))
    p <- p + geom_bar(aes(fill=model), stat="identity", position="dodge")
    p <- p + scale_fill_manual("Model", values=model_cols)
    p <- p + facet_grid(To~From)
    p <- p + theme_bw()
    p <- p + xlab("Season") + ylab("Movement coefficient")
    return(p)
  })

  output$plot_srr <- renderPlot({
    models <- input$model_select
    if(length(models) < 1){
      return()
    }
    # Process the SRR data into summarised form
    pdat <- srr_dat[,.(sb=sum(sb, na.rm=TRUE), rec=sum(rec, na.rm=TRUE)), by=.(model, year, season)]
    # Assume that annualised relationship i.e. flagval(par, 2, 182) == 1
    # flagval(par, 2, 182)$value # 1 - annualised SRR fit
    # The Beverton-Holt stock-recruitment relationship is fitted to total "annualised" recruitments and average annual biomass
    pdat <- pdat[,.(sb=mean(sb, na.rm=TRUE), rec=sum(rec, na.rm=TRUE)), by=.(model, year)]
    pdat <- pdat[model %in% models]
    # Label formatting
    xlab <- paste0("Spawning biomass (mt; ", format(sb_units, big.mark=",", trim=TRUE, scientific=FALSE), "s)")
    ylab <- paste0("Recruitment (N; ", format(rec_units, big.mark=",", trim=TRUE, scientific=FALSE), "s)")
    sbmax <- max(pdat$sb) * 1.2
    fdat <- srr_fit_dat[model %in% models & sb <= sbmax]
    # Do all lines and points on same plot and colour by model
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=sb/sb_units, y=rec/rec_units))
    # The points
    p <- p + geom_point(aes(fill=model, colour=model), shape=21, size=3)
    # The fitted model
    p <- p + geom_line(data=fdat, aes(x=sb/sb_units, y=rec/rec_units, colour=model), size=1.25)
    p <- p + scale_fill_manual("Model", values=model_cols)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + ylim(c(0,NA)) + xlim(c(0,NA))
    p <- p + theme_bw()
    p <- p + xlab(xlab) + ylab(ylab)
    return(p)
  })

  output$plot_rec_dist <- renderPlot({
    models <- input$model_select
    if(length(models) < 1){
      return()
    }
    # Average recruitment over time series by region and quarter
    av_rec <- srr_dat[,.(av_rec=mean(rec)), by=.(model, season, area)]
    # Total average recruitment of whole time series
    total_rec <- av_rec[,.(total_rec=sum(av_rec)), by=.(model)]
    pdat <- merge(av_rec, total_rec)
    pdat[, c("prop_rec", "season") := .(av_rec/total_rec, as.character(season))]
    pdat <- pdat[model %in% models]
    # Sanity check
    # pdat[,.(sum=sum(prop_rec)), by=.(model)]
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    pdat[,area := paste0("Area ", area)]
    # Or bar charts - nice
    p <- ggplot(pdat, aes(x=season, y=prop_rec))
    p <- p + geom_bar(aes(fill=model), stat="identity", position="dodge")
    p <- p + scale_fill_manual("Model", values=model_cols)
    p <- p + facet_wrap(~area, ncol=4)
    p <- p + xlab("Season") + ylab("Proportion of total recruitment")
    p <- p + theme_bw()
    return(p)
  })

  output$plot_rec_devs <- renderPlot({
    models <- input$model_select
    if(length(models) < 1){
      return()
    }
    pdat <- rec_dev_dat[model %in% models]
    pdat[,c("area", "season") := .(paste0("Area ", area), paste0("Season ", season))]
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=year, y=value))
    p <- p + geom_point(aes(fill=model, colour=model), alpha=0.5, size=3)
    p <- p + geom_smooth(aes(colour=model), method='loess', formula='y~x', na.rm=TRUE, se=FALSE, size=1.25)
    p <- p + scale_fill_manual("Model", values=model_cols)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + facet_grid(season~area)
    p <- p + xlab("Year") + ylab("Recruitment deviate")
    p <- p + theme_bw()
    return(p)
  })

  output$plot_selectivity <- renderPlot({
    models <- input$model_select
    age_or_length <- input$age_select_sel
    fisheries <- fishery_map[fishery_map$group %in% input$fishery_group, "fishery"]
    if(length(models) < 1 || length(fisheries) < 1){
      return()
    }
    xlab <- "Age class"
    if(age_or_length == "length"){
      xlab <- "Length (cm)"
    }
    pdat <- sel_dat[model %in% models & fishery %in% fisheries]
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes_string(x=age_or_length, y="value"))
    p <- p + geom_line(aes(colour=model), size=1.25)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + facet_wrap(~fishery_name, nrow=2)
    p <- p + ylim(c(0, NA))
    p <- p + xlab(xlab) + ylab("Selectivity")
    p <- p + theme_bw()
    return(p)
  })

  output$plot_natmort <- renderPlot({
    models <- input$model_select
    if(length(models) < 1){
      return()
    }
    pdat <- m_dat[model %in% models]
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=age, y=m))
    p <- p + geom_line(aes(colour=model), size=1.25)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + ylim(c(0, NA))
    p <- p + xlab("Age class") + ylab("Natural mortality")
    p <- p + theme_bw()
    return(p)
  })

  output$plot_growth <- renderPlot({
    models <- input$model_select
    if(length(models) < 1){
      return()
    }
    pdat <- sel_dat[model %in% models]
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=age))
    p <- p + geom_ribbon(aes(ymax=length_upper, ymin=length_lower, fill=model), alpha=0.25)
    p <- p + geom_line(aes(y=length, colour=model), size=1.25)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + scale_fill_manual("Model", values=model_cols)
    p <- p + xlab("Age class") + ylab("Length (cm)")
    p <- p + theme_bw()
    # Tighten the axes
    p <- p + scale_y_continuous(limits=c(0, NA), expand=c(0, 0))
    p <- p + scale_x_continuous(expand=c(0, 0))
    return(p)
  })

  output$plot_maturity <- renderPlot({
    models <- input$model_select
    if(length(models) < 1){
      return()
    }
    age_or_length <- input$age_select_mat
    if(age_or_length == "age"){
      pdat <- mat_age_dat[model %in% models]
    }
    if(age_or_length == "length"){
      pdat <- mat_length_dat[model %in% models]
    }
    xlab <- "Age class"
    if(age_or_length == "length"){
      xlab <- "Length (cm)"
    }
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes_string(x=age_or_length, y="mat"))
    p <- p + geom_line(aes(colour=model), size=1.25)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + ylim(c(0, 1))
    p <- p + xlab(xlab) + ylab("Maturity")
    p <- p + theme_bw()
    return(p)
  })

  output$plot_rec <- renderPlot({
    models <- input$model_select
    area_select <- input$area_select_recruitment
    areas <- c(1:nregions, "All")
    if(area_select == "combined"){
      areas <- "All"
    }
    if(length(areas) < 1 || length(models) < 1){
      return()
    }
    scale_choice <- "fixed"
    if(input$scale_select_recruitment){
      scale_choice="free"
    }
    # Need to sum recruitment over areas - could do in advance if slow
    pdat <- srr_dat[model %in% models, -"sb"]
    total_rec <- pdat[, .(area="All", rec=sum(rec)), by=.(model, year, season)]
    pdat <- rbindlist(list(pdat, total_rec))
    pdat <- pdat[area %in% areas]
    pdat[, ts := year + (season-1) / 4 + 1/8]
    # Show annual recruitment instead of seasonal (could provide a annual/seasonal selector)
    if(TRUE){
      pdat <- pdat[, .(rec=sum(rec), ts=year), by=.(model, year, area)]
    }
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    ylab <- paste0("Total recruitment (N; ", format(rec_units, big.mark=",", trim=TRUE, scientific=FALSE), "s)")
    p <- ggplot(pdat, aes(x=ts, y=rec / rec_units))
    p <- p + geom_bar(aes(fill=model), stat="identity", position="dodge")
    p <- p + scale_fill_manual("Model", values=model_cols)
    p <- p + facet_wrap(~area, nrow=2, scales=scale_choice)
    p <- p + xlab("Year") + ylab(ylab)
    p <- p + theme_bw()
    return(p)
  })

  output$plot_sbsbf0 <- renderPlot({
    models <- input$model_select
    area_select <- input$area_select_sbsbf0
    areas <- c(1:nregions, "All")
    if(area_select == "combined"){
      areas <- "All"
    }
    if(length(areas) < 1 || length(models) < 1){
      return()
    }
    pdat <- biomass_dat[model %in% models & area %in% areas, ]
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=year, y=SBSBF0))
    p <- p + geom_line(aes(colour=model), size=1.25)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + facet_wrap(~area, nrow=2)
    p <- p + xlab("Year") + ylab("SB/SBF=0")
    p <- p + ylim(c(0, 1))
    p <- p + theme_bw()
    p <- p + geom_hline(aes(yintercept=0.2), linetype=2)
    return(p)
  })

  output$plot_sb <- renderPlot({
    models <- input$model_select
    area_select <- input$area_select_sb
    areas <- c(1:nregions, "All")
    if(area_select == "combined"){
      areas <- "All"
    }
    if(length(areas) < 1 || length(models) < 1){
      return()
    }
    scale_choice <- "fixed"
    if(input$scale_select_sb){
      scale_choice="free"
    }
    ylab <- paste0("Spawning biomass (mt; ", format(sb_units, big.mark=",", trim=TRUE, scientific=FALSE), "s)")
    pdat <- biomass_dat[model %in% models & area %in% areas]
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=year, y=SB/sb_units))
    p <- p + geom_line(aes(colour=model), size=1.25)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + facet_wrap(~area, nrow=2, scales=scale_choice)
    p <- p + ylim(c(0, NA))
    p <- p + xlab("Year") + ylab(ylab)
    p <- p + theme_bw()
    return(p)
  })

  output$plot_sbf0 <- renderPlot({
    models <- input$model_select
    area_select <- input$area_select_sbf0
    areas <- c(1:nregions, "All")
    if(area_select == "combined"){
      areas <- "All"
    }
    if(length(areas) < 1 || length(models) < 1){
      return()
    }
    scale_choice <- "fixed"
    if(input$scale_select_sbf0){
      scale_choice="free"
    }
    ylab <- paste0("Unfished biomass (mt; ", format(sb_units, big.mark=",", trim=TRUE, scientific=FALSE), "s)")
    pdat <- biomass_dat[model %in% models & area %in% areas]
    model_cols <- get_model_colours(all_model_names=all_models, chosen_model_names=models)
    p <- ggplot(pdat, aes(x=year, y=SBF0/sb_units))
    p <- p + geom_line(aes(colour=model), size=1.25)
    p <- p + scale_colour_manual("Model", values=model_cols)
    p <- p + facet_wrap(~area, nrow=2, scales=scale_choice)
    p <- p + ylim(c(0, NA))
    p <- p + xlab("Year") + ylab(ylab)
    p <- p + theme_bw()
    return(p)
  })

  output$status_table <- renderDT({
    # dom option drops the search and other stuff
    reftab <- datatable(status_tab_dat, options=list(pageLength=nmodels, dom='t'), rownames=FALSE)
    if(length(input$model_select) > 0){
      reftab <- reftab %>% formatStyle('Model', target='row', backgroundColor=styleEqual(input$model_select,'yellow'))
    }
    return(reftab)
  })
} # End of server

shinyApp(ui, server)
