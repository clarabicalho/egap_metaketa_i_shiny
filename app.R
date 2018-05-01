# TODO
# Replace table with call to model estimates
# DONE Fix weights and
# include note on weights(brz, ug2, meta)
# DONE xlim symmetrical?
# add description + EGAP link
# DONE fix covariate input
# DONE allow select sample that gets calculated in meta (separately from default meta)
# DONE add contested specs (weights, exclude councilors, contested elections, adding: uganda 1 coding of good news)
# include bayesian graphs
# integrate data upload
# add download data/code option

# publish as repo https://shiny.wzb.eu/apps/ipi/mk1/

library(shiny)
library(dplyr)
library(tidyr)
library(lfe)
library(shinythemes)
library(shinyBS)

load("../madat.Rda")
source("../functions.R")

# Parameters passed onto Shiny --------------------------------------------

# Dependent Variable
depvar_opts <- list("Incumbent Vote Choice (M1)" = "m1",
                    "Voter Turnout (M3)"         = "m3",
                    "Effort (M5)"                = "m5",
                    "Dishonesty (M6)"            = "m6",
                    "Backlash (M8)"              = "m8",
                    "Correct recollection"       = "correct")

# Subsetting

# Good / Bad / Overall news
good_opts <- list("Good"    = "good",
                  "Bad"     = "bad",
                  "Overall" = "both")

# Country
country_opts <- list("Benin"        = "ben",
                     "Brazil"       = "brz",
                     "Burkina Faso" = "bf",
                     "Mexico"       = "mex",
                     "Uganda 1"     = "ug1",
                     "Uganda 2"     = "ug2")

# Country
cov_opts <- list("Age (M14)" = "m14i",
                 "Education (M17)" = "m17i",
                 "Wealth (M18)" = "m18i",
                 "Voted last election (M20)" = "m20i",
                 "Supported incumbent last election (M21)" = "m21i",
                 "Clientelism (M22)" = "m22i",
                 "Credibility (M24)" = "m24i",
                 "Secret ballot (M26)" = "m26i",
                 "Free and fair election (M27)" = "m27i")
  
# Exclude
# excl_opts <- list("")

# Define UI for Metaketa app ----------------------------------------------

ui <- fluidPage(
  
  theme = shinytheme("yeti"),
  # shinythemes::themeSelector(),
  
  # App title ----
  fluidRow(align = "center",
  titlePanel("Metaketa I - Information and Accountability")),
  
  br(),
  
  bsCollapsePanel("About", value="About",
                  p("Reference to book? Explain MK in general? Refer to EGAP website?"),
                  p("Metaketa I Pre-meta-analysis Plan:", a("20150309AA", href="http://egap.org/registration/736")),
                  p("Visit the ", a("EGAP website", href="http://egap.org/metaketa/metaketa-information-and-accountability"), "to learn more.")
  ),
  
  br(),
  
  # Side bar ----
  fluidRow(align = "center",
    # Results ----
    column(6, align = "center",
           plotOutput("plotResults", width = 500)),
    column(6, align = "center",
           tableOutput("tableResults"))
  ),
  
  wellPanel(
  fluidRow(
    # verbatimTextOutput("print"),
    column(3,
             #Option 1: Depedent variable
             selectInput('depvar', 'Dependent Variable',
                         choices = depvar_opts),
             #Option 2: Subset to good/bad/overall news
             selectInput("news", "News", selected = good_opts[1],
                          choices = good_opts),
             #Option 5: Weights
             radioButtons("weight", "",
                          choices = list("Weighted"   = "TRUE",
                                         "Unweighted" = "FALSE"),
                          selected = "TRUE")),
    column(2,
             #Option 4: Country subset
           checkboxGroupInput('country', 'Study', 
                                choices = c("Meta (default)" = "all",
                                            "Meta (alternative)" = "Meta (alternative)",
                                            country_opts),
                                selected = c("all","alt"))),
    column(1,
           checkboxGroupInput('ma_alt', 'Meta (alternative)', 
                              choices = country_opts,
                              selected = country_opts)),
    column(3,
           #Option 6: Covariates
           checkboxGroupInput('cov', 'Covariates', 
                                choices = cov_opts)),

    column(3, 
           #Option 6: Covariates
           radioButtons('n_cov', 'Include Nij as covariate?', 
                        choices = c("Yes" = "TRUE", "No" = "FALSE"),
                        selected = "FALSE"),
           checkboxGroupInput("contested", "Contested specifications",
                              choices = c("Exclude non-contested elections (Uganda 2 study)" = "contested_elections",
                                          "Exclude LCV councilors (Uganda 2 study)" = "councilors",
                                          "Use alternative coding of good news (Uganda 1 study)" = "n_alt"),
                              selected = c("contested_elections","councilors")),
           downloadButton("downloadData", "Download data")))
    ),
  
  bsCollapsePanel("Notes", value="Notes",
                  p("Notes on options above (weights only affect certain countries), discussion of contested decisions? "))
  )

# Define server logic required to plot output ----

server <- function(input, output) {


  cov <- reactive({
   paste(input$cov, collapse = "+")
  })
  
  # wts <- reactive({
  #   if(deparse(input$weight) == "TRUE")
  #     TRUE
  #   else
  #     FALSE
  # })

  output$print <- renderPrint({
    cov()
    # str(table())
    # contested()
    # input$contested
    # cov <- ifelse(as.character(cov())=="", "NULL", as.character(cov()))
    # if(is.null(cov())) print("it's null!")
    # str(input$cov)
  })
  
  
  contested <- reactive({
    ret <- "FALSE"
    if(!is.null(input$contested)) if("contested_elections" %in% input$contested) ret <- "TRUE"
    ret
  })
  
  councilors <- reactive({
    ret <- "FALSE"
    if(!is.null(input$contested)) if("councilors" %in% input$contested) ret <- "TRUE"
    ret
  })
  
  N_alternative <- reactive({
    ret <- "FALSE"
    if(!is.null(input$contested)){
      if("n_alt" %in% input$contested) ret <- "TRUE"
    }
    ret
  })  
  
  data <- reactive({
  madat <- madat
  if(N_alternative()=="TRUE"){
    madat$N_good[madat$ctry=="ug1"] <- 1*(madat$N_alt[madat$ctry=="ug1"]>0)
  }
  madat
  })
  
  madat_alt <- reactive({
    madat[madat$ctry %in% input$ma_alt,]
  })
  
  table <- reactive({
    # covar <- cov()
    # cov <- input$cov
    
    c_list <- input$country[!input$country %in% "Meta (alternative)"]
    
    tab <- do.call("rbind", lapply(
      c_list, function(i) {
    results <- results(dat = data(),
                       depvar = input$depvar,
                       news = input$news,
                       country = i,
                       cov = cov(),
                       contested_seats = contested(),
                       exclude_councilors = councilors(),
                       with_N = input$n_cov,
                       weights = input$weight,
                       ri_p = "ignore")$estimates %>% summary %>% .$coefficients %>% .[1,]
    
    results
      }))
    
    meta_alt <-  NULL
    meta_alt <- if("Meta (alternative)" %in% input$country){
      est <- results(dat = madat_alt(),
                     depvar = input$depvar,
                     news = input$news,
                     country = "all",
                     # cov = covar,
                     contested_seats = contested(),
                     exclude_councilors = councilors(),
                     with_N = input$n_cov,
                     weights = input$weight,
                     ri_p = "ignore")
      est$estimates %>% summary() %>% .$coefficients %>% .[1,]
    }
    
    # tab <- rbind(tab[1,], meta_alt)
    country_list <- case_when(input$country == "ben" ~ "Benin",
                              input$country == "brz" ~ "Brazil",
                              input$country == "bf"  ~ "Burkina Faso",
                              input$country == "mex" ~ "Mexico",
                              input$country == "ug1" ~ "Uganda 1",
                              input$country == "ug2" ~ "Uganda 2",
                              input$country == "all" ~  "Meta (default)")
    
    rownames(tab) <- country_list[!is.na(country_list)]
    
    tab <- rbind(meta_alt, tab)

    if(!is.null(meta_alt)) rownames(tab)[1] <- "Meta (alternative)"
    
    tab
  })
  
  title <- reactive({
    news <- input$news
    depvar <- case_when(input$depvar == "m1" ~ "incumbent vote choice",
                        input$depvar == "m3" ~ "voter turnout",
                        input$depvar == "m5" ~ "effort",
                        input$depvar == "m6" ~ "honesty",
                        input$depvar == "m8" ~ "backlash",
                        input$depvar == "correct" ~ "correct recollection")
    
    if(input$news == "both") news <- "information"
    if(input$news == "good") news <- "good news"
    if(input$news == "bad") news <- "bad news"
    
    paste("Treatment effect of", news, "on", depvar)
      })
  
  output$tableResults <- renderTable({
    table()
  }, rownames = TRUE)
  
  output$plotResults <- renderPlot({
      par(mar = c(5, 8, 4, 2))
      plot(x = table()[nrow(table()):1,1],
           y = 1:nrow(table()),
           # xlim = c(min(table()[,1]-1.96*table()[,2])-.01, max(table()[,1]+1.96*table()[,2])+.01),
           xlim = c(-.2, .2),
           xlab = "Effect Sizes (95% confidence)", pch = 19, axes = FALSE, #ylim = c(.5,7),
           ylab = "", main = title())
      # text(x = .00, y = 1:nrow(table)-.2, labels = tab_g_p, pos = 4, cex = .8)
      axis(1)
      axis(2, at = 1:nrow(table()), labels = rev(rownames(table())), las = 1)
      segments(table()[nrow(table()):1,1]-1.96*table()[nrow(table()):1,2], 1:nrow(table()),
               table()[nrow(table()):1,1]+1.96*table()[nrow(table()):1,2], 1:nrow(table()))
      abline(v=0, col = "red")
      box()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      "mk1_ma_data.csv"
    },
    content = function(file) {
      write.csv(madat, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)

# run app -----------------------------------------------------------------

# runApp("App_v1", display.mode = "showcase")
