# Reducinfg the files to the open code

# ui
# Libraries
library(rio)
library(tidyverse)
library(shiny)
library(ggflags)
library(plotly)
library(countrycode)
library(sp)
library(maps)
library(maptools)
library(leaflet)
library(shinydashboard)
library(shinythemes)
library(wesanderson)
library(leaflet.extras)
library(rgeos)
#library(affiliation)
library(DT)
library(shinymanager)

# Pre-processing
# Affiliation -----------------------------------------------------------------------

all_traits_merged <- rio::import("data/affiliation/all_traits_merged.csv")

trait.options <- unique(all_traits_merged$Trait)
position.options <- unique(all_traits_merged$position)

world_aff <- map("world", fill=TRUE, plot=FALSE)

world_aff_map <- map2SpatialPolygons(world_aff, sub(":.*$", "", world_aff$names))
world_aff_map <- SpatialPolygonsDataFrame(world_aff_map,
                                          data.frame(country=names(world_aff_map),
                                                     stringsAsFactors=FALSE),
                                          FALSE)

cnt_aff <- all_traits_merged$Country_map
target_aff <- subset(world_aff_map, country %in% cnt_aff)

all_traits_merged2 <- merge(target_aff, all_traits_merged, by.x = "country", by.y = "Country_map", duplicateGeoms = TRUE)
risk.bins <-c(0, 5, 10, 15, 20, 30, 40, 100)
pal <- colorBin( palette = c('#f7fbff', '#deebf7','#c6dbef', '#9ecae1','#6baed6', '#4292c6', "#2171b5", "#084594"),
                 bins=risk.bins, na.color = 'dimgrey')


# Trends
complete_trends <- rio::import("data/affiliation/complete_trends_affiliations_country.csv")

# Samples -----------------------------------------------------------------------

all_samples_merged <- rio::import("data/samples/all_samples_merged.csv")

world_samples <- map("world", fill=TRUE, plot=FALSE)

unique(all_samples_merged[!(all_samples_merged$COUNTRY_OF_RECRUITMENT %in% sub(":.*$", "", world_samples$names)),]$COUNTRY_OF_RECRUITMENT)
sub(":.*$", "", world_samples$names)[grepl("Congo", sub(":.*$", "", world_samples$names), fixed = TRUE)]
all_samples_merged[all_samples_merged$COUNTRY_OF_RECRUITMENT == "Congo",]$COUNTRY_OF_RECRUITMENT <- "Republic of Congo"
sub(":.*$", "", world_samples$names)[grepl("Iran", sub(":.*$", "", world_samples$names), fixed = TRUE)]
all_samples_merged[all_samples_merged$COUNTRY_OF_RECRUITMENT == "Iran (Islamic Republic of)",]$COUNTRY_OF_RECRUITMENT <- "Iran"
sub(":.*$", "", world_samples$names)[grepl("Ireland", sub(":.*$", "", world_samples$names), fixed = TRUE)]
all_samples_merged[all_samples_merged$COUNTRY_OF_RECRUITMENT == "Republic of Ireland",]$COUNTRY_OF_RECRUITMENT <- "Ireland"
sub(":.*$", "", world_samples$names)[grepl("Korea", sub(":.*$", "", world_samples$names), fixed = TRUE)]
all_samples_merged[all_samples_merged$COUNTRY_OF_RECRUITMENT == "Republic of Korea",]$COUNTRY_OF_RECRUITMENT <- "South Korea"
sub(":.*$", "", world_samples$names)[grepl("Russia", sub(":.*$", "", world_samples$names), fixed = TRUE)]
all_samples_merged[all_samples_merged$COUNTRY_OF_RECRUITMENT == "Russian Federation",]$COUNTRY_OF_RECRUITMENT <- "Russia"
sub(":.*$", "", world_samples$names)[grepl("Macedonia", sub(":.*$", "", world_samples$names), fixed = TRUE)]
all_samples_merged[all_samples_merged$COUNTRY_OF_RECRUITMENT == "The former Yugoslav Republic of Macedonia",]$COUNTRY_OF_RECRUITMENT <- "Macedonia"
sub(":.*$", "", world_samples$names)[grepl("Tanzania", sub(":.*$", "", world_samples$names), fixed = TRUE)]
all_samples_merged[all_samples_merged$COUNTRY_OF_RECRUITMENT == "United Republic of Tanzania",]$COUNTRY_OF_RECRUITMENT <- "Tanzania"
sub(":.*$", "", world_samples$names)[grepl("Viet", sub(":.*$", "", world_samples$names), fixed = TRUE)]
all_samples_merged[all_samples_merged$COUNTRY_OF_RECRUITMENT == "Viet Nam",]$COUNTRY_OF_RECRUITMENT <- "Vietnam"

all_samples_merged$Country_map <- all_samples_merged$COUNTRY_OF_RECRUITMENT

sub(":.*$", "", world_samples$names)[grepl("UK", sub(":.*$", "", world_samples$names), fixed = TRUE)]
all_samples_merged[all_samples_merged$Country_map == "United Kingdom",]$Country_map <- "UK"
sub(":.*$", "", world_samples$names)[grepl("USA", sub(":.*$", "", world_samples$names), fixed = TRUE)]
all_samples_merged[all_samples_merged$Country_map == "United States",]$Country_map <- "USA"


world_samples_map <- map2SpatialPolygons(world_samples, sub(":.*$", "", world_samples$names))
world_samples_map <- SpatialPolygonsDataFrame(world_samples_map,
                                              data.frame(country=names(world_samples_map),
                                                         stringsAsFactors=FALSE),
                                              FALSE)

cnt_samples <- all_samples_merged$Country_map
target_samples <- subset(world_samples_map, country %in% cnt_samples)

all_samples_merged$popup_info <- paste0("<b>",all_samples_merged$COUNTRY_OF_RECRUITMENT,"</b>", "<br/>", round(all_samples_merged$Prop, 2), "%")
all_samples_merged2 <- merge(target_samples, all_samples_merged, by.x = "country", by.y = "Country_map", duplicateGeoms = TRUE)

# Trends
cohorts_trends <- rio::import("data/samples/samples_trends.csv")

# Gender -----------------

gender_df <- rio::import("data/gender/gender.csv")

# Trends

gender_trends <- rio::import("data/gender/gender_trends.csv")

# Income authors ----------------

income_authors <- rio::import("data/income/income_authors.csv")

# Income trends (authors)

income_trends <- rio::import("data/income/authors_income_trends.csv")

# Income cohorts ----------------

income_cohorts <- rio::import("data/income/income_samples.csv")

# Income trends (cohorts)

income_cohorts_trends <- rio::import("data/income/income_trends_samples.csv")

# ancestry ----------------

ancestry_df <- rio::import("data/ancestry/ancestry.csv")

# trends
ancestry_trends <- rio::import("data/ancestry/ancestry_trends.csv")

# affiliation institutions -----------------

institutions <- rio::import("data/affiliation/institutions.csv")

institutions_weighted <- rio::import("data/affiliation/institutions_weighted.csv")

score.options <- c("Ubiquity score", "Dominance score")

# ui -----------------------------------------------------------------------


ui <-  bootstrapPage('',
                                tags$style(type =

                                             '#controls {
                                background-color: #ddd;
                                opacity: 0.5;
                                }
                                #controls:hover{
                                opacity: 1;
                                }'


                                ),
                                navbarPage("Landscape of GWAS - Results page",
                                           theme = shinytheme("flatly"),
                                           tabPanel("Co-authors",
                                                    tabsetPanel(
                                                      type = "tabs",
                                                      tabPanel("Affiliations - Countries",
                                                               pageWithSidebar(
                                                                 headerPanel('Affiliations'),
                                                                 sidebarPanel("In this section, we present the top-20 countries of affiliation for GWAS papers belonging to all and each of the ten most burdensome non-communicative diseases (NCDs). Countries of affiliation were determined by applying text-mining and named-entity recognition (NER) to the affiliation field (extracted from PubMed). For data analysis, authors were categorized as: (1) all authors, comprising all authors list in each paper; (2) first authors, listed as the first in the author sequence or identified as joint first author with typographic marks; and (3) senior authors, listed as the last in the author sequence or identified as joint last author with typographic marks.",
                                                                              selectInput("trait_aff", "Select a trait:", trait.options),
                                                                              radioButtons("position_aff", "Authorship position:", position.options)
                                                                 ),
                                                                 mainPanel(
                                                                   fluidRow(
                                                                     plotOutput('affiliation.plot', height="600px")
                                                                   ),
                                                                   fluidRow(
                                                                     plotlyOutput('affiliation.trend')
                                                                   )
                                                                 )
                                                               )
                                                      ),
                                                      tabPanel("Affiliations - Map",
                                                               leafletOutput(outputId = "map.aff", height = 750),
                                                               #headerPanel('Affiliations'),
                                                               absolutePanel(id = "controls",
                                                                             top = 170, right = 70, width = 330,
                                                                             class = "panel panel-default",
                                                                             draggable =  TRUE, height = "auto",
                                                                             "Here, we present a map of the top-20 countries of affiliation for GWAS papers belonging to all and each of the ten most burdensome NCDs. ",
                                                                             selectInput("trait_aff2", "Select a trait:", trait.options),
                                                                             radioButtons("position_aff2", "Authorship position:", position.options)
                                                               )
                                                      ),

                                                      tabPanel("Affiliations - Institutions",
                                                               pageWithSidebar(
                                                                 headerPanel('Affiliations - Institutions'),
                                                                 sidebarPanel("Here we present the lists of worldwide institutions affiliated with GWAS publications ranked by two scores of representativeness. The dominance score reflects the extent to which coauthors from a given institution appear on GWAS papers as a proportion of all coauthors, with institutions fielding multiple coauthors scoring better than those with few coauthors. The ubiquity score, expresses the frequency with which a given institution appears within coauthor affiliations across GWAS publications. In this case, institutions that have at least one coauthor on many GWAS papers will score higher than those with coauthors distributed across fewer papers, regardless of the total number of coauthorships.",
                                                                              selectInput("trait_inst", "Select a trait:", trait.options),
                                                                              radioButtons("method_inst", "Select score:", score.options),
                                                                              radioButtons("position_inst", "Authorship position:", position.options)
                                                                 ),
                                                                 mainPanel(dataTableOutput('institution.table'))
                                                               )
                                                      ),

                                                      tabPanel("Gender",
                                                               pageWithSidebar(
                                                                 headerPanel('Gender'),
                                                                 sidebarPanel(
                                                                   tags$p('Authors`s gender was estimated using two online-based tools: ',
                                                                          tags$a(href="https://genderize.io/", "genderize.io"), ' and ',
                                                                          tags$a(href="https://gender-api.com/", "GenderAPI"), '. Both use first and last names and geographic locations to determine gender. The APIs made equal predictions for 93% of the names. For 4% of the data, one of the APIs could not make a conclusive prediction on gender (“unknow”) and for 3% the predictions between male/female were distinct. For the first case, we used the results of the API which could predict a gender, and for the second we choose the result that yielded higher accuracy. Here we present the final "combined" results, as well as a "comparison", showing results of each specific API.'),
                                                                   selectInput("trait_gender", "Select a trait:", trait.options),
                                                                   radioButtons("position_gender", "Authorship position:", position.options)
                                                                 ),
                                                                 mainPanel(
                                                                   fluidRow(
                                                                     tabsetPanel(
                                                                       type = "tabs",

                                                                       tabPanel("Merged",
                                                                                plotlyOutput('gender_general.plot'),
                                                                                fluidRow(
                                                                                  plotlyOutput("gender.trends")
                                                                                )
                                                                       ),

                                                                       tabPanel("Comparison",
                                                                                box(plotlyOutput('genderAPI.plot'), width=6, height=500, title = span(icon("venus-mars"), "GenderAPI"), status = "primary", solidHeader = TRUE, textOutput("info-circle"),
                                                                                    collapsible = TRUE),
                                                                                box(plotlyOutput('genderize.plot'), width=6, height=500, title = span(icon("venus-mars"), "genderize.io"), status = "primary", solidHeader = TRUE, textOutput("info-circle2"),
                                                                                    collapsible = TRUE)))
                                                                   )


                                                                 )
                                                               )
                                                      ),
                                                      tabPanel("Income",
                                                               pageWithSidebar(
                                                                 headerPanel('Income - Co-authors'),
                                                                 sidebarPanel(
                                                                   tags$p("To estimate the degree to which socioeconomy is related to published GWAS research we determined the socioeconomic level of the countries within which the authors’ institutions were located using the ",
                                                                          tags$a(href="https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups", "World Bank's"), " calculations of gross national income (GNI) per capita (in US dollars). Thereafter, the following socioeconomic categories were assigned to each author: high income (HIC), upper middle income (UMIC), low middle income (LMIC), and low income (LIC)"),
                                                                   selectInput("trait_income", "Select a trait:", trait.options),
                                                                   radioButtons("position_income", "Authorship position:", position.options)
                                                                 ),
                                                                 mainPanel(
                                                                   fluidRow(
                                                                     plotlyOutput('income_authors.plot')),
                                                                   fluidRow(
                                                                     plotlyOutput('income_trends.plot'))
                                                                 )

                                                               )
                                                      )
                                                    )
                                           ),
                                           tabPanel("Samples",
                                                    tabsetPanel(
                                                      type = "tabs",
                                                      tabPanel("Origin of samples",
                                                               pageWithSidebar(
                                                                 headerPanel('Origin of samples'),
                                                                 sidebarPanel(
                                                                   tags$p("In this section, we present the top-20 countries of origin for samples in GWAS papers belonging to all and each of the ten most burdensome NCDs. Information about the origin (origin and country of recruitment) of samples was obtained in the NHGRI-EBI ",
                                                                          tags$a(href="https://www.ebi.ac.uk/gwas/", "GWAS Catalog"),
                                                                          " of published genome-wide association studies."),
                                                                   selectInput("trait_cohorts", "Trait", trait.options)
                                                                 ),
                                                                 mainPanel(
                                                                   fluidRow(
                                                                     plotOutput('origin.plot', height="600px"),
                                                                     plotlyOutput('origin.trends')
                                                                   )
                                                                 )
                                                               )
                                                      ),
                                                      tabPanel("Origin of samples - Map",
                                                               leafletOutput(outputId = "map.cohort", height = 750),
                                                               #headerPanel('Affiliations'),
                                                               absolutePanel(id = "controls",
                                                                             top = 170, right = 70, width = 330,
                                                                             class = "panel panel-default",
                                                                             draggable =  TRUE, height = "auto",
                                                                             tags$p("In this section, we present the map of the top-20 countries of origin for samples in GWAS papers belonging to all and each of the ten most burdensome NCDs. Information about the origin (origin and country of recruitment) of samples was obtained in the NHGRI-EBI ",
                                                                                    tags$a(href="https://www.ebi.ac.uk/gwas/", "GWAS Catalog"),
                                                                                    " of published genome-wide association studies."),
                                                                             selectInput("trait_cohorts2", "Trait", trait.options)
                                                               )
                                                      ),
                                                      tabPanel("Income",
                                                               pageWithSidebar(
                                                                 headerPanel('Income - Samples'),
                                                                 sidebarPanel(
                                                                   tags$p("To estimate the degree to which socioeconomy is related to published GWAS research we determined the socioeconomic level of the countries within the samples were originated using the ",
                                                                          tags$a(href="https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups", "World Bank's"), " calculations of gross national income (GNI) per capita (in US dollars). Thereafter, the following socioeconomic categories were assigned to each sample group: high income (HIC), upper middle income (UMIC), low middle income (LMIC), and low income (LIC)"),
                                                                   selectInput("trait_income_cohort", "Select a trait:", trait.options)
                                                                 ),
                                                                 mainPanel(
                                                                   fluidRow(
                                                                     plotlyOutput('income_samples.plot')),
                                                                   fluidRow(
                                                                     plotlyOutput('income_cohorts.trends'))
                                                                 )
                                                               )
                                                      ),
                                                      tabPanel("Ancestry",
                                                               pageWithSidebar(
                                                                 headerPanel('Ancestry'),
                                                                 sidebarPanel(tags$p("In this section you find our results for samples' ancestry. We extracted these information from ",
                                                                                     tags$a(href="(https://www.ebi.ac.uk/gwas/docs/file-downloads", "GWAS Catalog’s “All ancestry data” file."), "  Here we grouped samples by broad ancestral categories, excluding samples for which no ancestry or country of recruitment information was available (marked as “Not Reported”)."),
                                                                              selectInput("trait_ancestry", "Select a trait:", trait.options)
                                                                 ),
                                                                 mainPanel(
                                                                   fluidRow(plotlyOutput('ancestry.plot')),
                                                                   fluidRow(plotlyOutput('ancestry.trend'))
                                                                 )
                                                               )
                                                      )
                                                    )
                                           )

                                )
                  )



# server -----------------------------------------------------------------------

server <- function(input, output) {

  output$affiliation.plot <- renderPlot({

    all_traits_merged %>%
      filter(Trait == input$trait_aff) %>%
      filter(position == input$position_aff) %>%
      mutate(rank = min_rank(desc(Prop)), Country_Rank = paste(rank, ":", Country)) %>%
      filter(rank <=20) %>%
      ggplot(aes(x = reorder(Country_Rank, Prop), y = Prop)) +
      geom_bar(stat="identity", fill="dimgray") +
      labs(title = paste0("Country of affiliation: ", input$trait_aff), subtitle = paste0(input$position_aff, " authors"), x = "", y = "",
           caption = "") +
      theme_bw() +
      geom_flag(y = -1, aes(country = tolower(ISO2)), size = 7) +
      scale_y_continuous(expand = c(0.02, 1.2)) +
      geom_text(aes(reorder(Country_Rank, Prop), Prop + 1.2, label = paste0(sprintf("%2.2f", Prop), "%")),
                position = position_dodge(width = 1)) +
      coord_flip() +
      theme(axis.text.x= element_text(colour = "black",  size = 10),
            axis.text.y = element_text(colour = "black",  size = 12),
            plot.title = element_text(size = 20, face = "bold"),
            plot.subtitle = element_text(size = 16))


  })

  rank_df <- reactive(all_traits_merged %>%
                        filter(Trait == input$trait_aff) %>%
                        filter(position == input$position_aff) %>%
                        mutate(rank = min_rank(desc(Prop)), Country_Rank = paste(rank, ":", Country)) %>%
                        filter(rank <=20))


  output$affiliation.trend <- renderPlotly(

    complete_trends %>%
      filter(Country %in% rank_df()$Country) %>%
      filter(Trait == input$trait_aff) %>%
      filter(position == input$position_aff) %>%
      filter(Prop != 0) %>%
      plot_ly(x = ~DATE,  y = ~Prop * 100,  mode = 'lines+markers', type = 'scatter', connectgaps = TRUE,
              color = ~Country,
              hoverinfo = 'text',
              hovertext = ~paste('</br> Country: ', Country,
                                 '</br> Year: ', DATE,
                                 '</br>', round(Prop * 100, 2), "%"))  %>%
      layout(title = paste0("Proportion of countries of affiliation (2005-2022): ", input$trait_aff, " (", input$position_aff, " authors)"),
             xaxis = list(title = "Year"),
             yaxis = list (title = "",
                           tickmode = "array"))
  )

  dailyData2 <- reactive(all_traits_merged2[all_traits_merged2$Trait == input$trait_aff2 & all_traits_merged2$position == input$position_aff2 & round(all_traits_merged2$Prop, 2) > 0,])

  labels_to_place <- labels_to_place <- c("< 5%", "5 - 10%", "10 - 15%", "15 - 20%", "20 - 30%", "30 - 40%", "> 40%", "NA")


  output$map.aff <- renderLeaflet({

    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = dailyData2(),
                  color = 'black',
                  fillColor =  ~pal(Prop),
                  popup = dailyData2()$popup_info,
                  smoothFactor = 0.2, fillOpacity = 1,  weight = 1,
                  highlightOptions = highlightOptions(stroke = 4, weight = 3, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal,
                values  = dailyData2()$Prop,
                position = "topleft",
                title = "",
                opacity = 1,
                labFormat = function(type, cuts, p) {  # Here's the trick
                  paste0(labels_to_place)
                }) %>%
      addFullscreenControl(pseudoFullscreen = TRUE)
  })

  institution_input <- reactive({
    if (input$method_inst == "Ubiquity score")
      institutions[institutions$Trait == input$trait_inst & institutions$position == input$position_inst, c(1,2,4)]
    else if (input$method_inst == "Dominance score")
      institutions_weighted[institutions_weighted$position == input$position_inst & institutions_weighted$Trait == input$trait_inst, c(1,2,3)]
    else
      stop("Unexpected dataset")
  })

  output$institution.table <- DT::renderDataTable(institution_input(), rownames= FALSE)

  #pie_palette <- wes_palette("Royal1")
  pie_palette <- c("#1B9E77", "#D95F02")

  output$gender_general.plot <- renderPlotly(
    gender_df %>%
      filter(plataform == "Both") %>%
      filter(Trait == input$trait_gender) %>%
      filter(position == input$position_gender) %>%
      plot_ly(labels = ~gender,
              values = ~prop * 100,
              type = 'pie',
              marker = list(colors = pie_palette, line = list(color = '#FFFFFF', width = 1)),
              hoverinfo = 'text',
              hovertext = ~paste('</br> Gender: ', gender,
                                 '</br>', round(prop * 100, 2), "%")) %>%
      layout(title = '',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  )

  output$genderAPI.plot <- renderPlotly(
    gender_df %>%
      filter(plataform == "genderAPI") %>%
      filter(Trait == input$trait_gender) %>%
      filter(position == input$position_gender) %>%
      plot_ly(labels = ~gender,
              values = ~prop * 100,
              type = 'pie',
              marker = list(colors = pie_palette, line = list(color = '#FFFFFF', width = 1)),
              hoverinfo = 'text',
              hovertext = ~paste('</br> Gender: ', gender,
                                 '</br>', round(prop * 100, 2), "%")) %>%
      layout(title = '',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  )

  output$genderize.plot <- renderPlotly(
    gender_df %>%
      filter(plataform == "genderize.io") %>%
      filter(Trait == input$trait_gender) %>%
      filter(position == input$position_gender) %>%
      plot_ly(labels = ~gender,
              values = ~prop * 100,
              type = 'pie',
              marker = list(colors = pie_palette, line = list(color = '#FFFFFF', width = 1)),
              hoverinfo = 'text',
              hovertext = ~paste('</br> Gender: ', gender,
                                 '</br>', round(prop * 100, 2), "%")) %>%
      layout(title = '',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  )

  output$gender.trends <- renderPlotly(
    gender_trends %>%
      filter(Trait == input$trait_gender) %>%
      filter(position == input$position_gender) %>%
      filter(gender == "Female") %>%
      plot_ly(x = ~YEAR,  y = ~final_freq * 100,  mode = 'lines+markers', type = 'scatter', connectgaps = TRUE,
              line=list(color="#1B9E77"), marker = list(color = "#1B9E77"),
              hoverinfo = 'text',
              hovertext = ~paste('</br> Gender: ', gender,
                                 '</br> Year: ', YEAR,
                                 '</br>', round(final_freq * 100, 2), "%")) %>%
      layout(title = paste0("Proportion of female authors across the years: ", input$trait_gender, " (", input$position_gender, " authors)"),
             xaxis = list(title = "Year"),
             yaxis = list (title = "% of female co-authors", range = c(0, 100),
                           tickmode = "array"),
             annotations =
               list(x = 1, y = -0.1, text = "",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=10))
      )

  )

  bar_palette <- wes_palette("Royal1")

  output$income_authors.plot <- renderPlotly(
    income_authors %>%
      filter(Trait == input$trait_income) %>%
      filter(position == input$position_income) %>%
      plot_ly(x = ~reorder(Income_group, desc(Prop)), y = ~Prop, color = ~Income_group, type = "bar",colors = c(bar_palette[[1]], bar_palette[[2]], bar_palette[[3]], bar_palette[[4]]),
              hoverinfo = 'text',
              hovertext = ~paste('</br> Income: ', Income_group,
                                 '</br>', round(Prop, 2), "%")) %>%
      layout(title = paste0("Level of income: ", input$trait_income, " (", input$position_income, " authors)"),
             xaxis = list(title = ""),
             yaxis = list (title = "% of co-authors", range = c(0, 100),
                           ticktext = list("0","20%", "40%", "60%", "80%", "100%"),
                           tickvals = list(0,20,40,60,80,100),
                           tickmode = "array"))
  )

  output$income_trends.plot <- renderPlotly(
    income_trends %>%
      filter(Trait == input$trait_income) %>%
      filter(position == input$position_income) %>%
      plot_ly(x = ~DATE,  y = ~Prop * 100,  mode = 'lines+markers', type = 'scatter', connectgaps = TRUE,
              color = ~Income_group,
              colors = c(bar_palette[[1]], bar_palette[[2]], bar_palette[[3]], bar_palette[[4]]),
              hoverinfo = 'text',
              hovertext = ~paste('</br> Income: ', Income_group,
                                 '</br>', round(Prop * 100, 2), "%")) %>%
      layout(title = paste0("Proportion of income groups (2005-2022): ", input$trait_income, " (", input$position_income, " authors)"),
             xaxis = list(title = "Year"),
             yaxis = list (title = "% of female co-authors",
                           tickmode = "array"),
             annotations =
               list(x = 1, y = -0.1, text = "",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=10))
      )

  )

  output$origin.plot <- renderPlot({

    all_samples_merged %>%
      filter(Trait == input$trait_cohorts) %>%
      mutate(rank = min_rank(desc(Prop)), Country_Rank = paste(rank, ":", COUNTRY_OF_RECRUITMENT)) %>%
      filter(rank <=20) %>%
      ggplot(aes(x = reorder(Country_Rank, Prop), y = Prop)) +
      geom_bar(stat="identity", fill="dimgray") +
      labs(title = paste0("Origin of samples (country of recruitment): ", input$trait_cohorts), subtitle = "", x = "", y = "",
           caption = "") +
      theme_bw() +
      geom_flag(y = -1, aes(country = tolower(ISO2)), size = 7) +
      scale_y_continuous(expand = c(0.02, 1.2)) +
      geom_text(aes(reorder(Country_Rank, Prop), Prop + 1.2, label = paste0(sprintf("%2.2f", Prop), "%")),
                position = position_dodge(width = 1)) +
      coord_flip() +
      theme(axis.text.x= element_text(colour = "black",  size = 10),
            axis.text.y = element_text(colour = "black",  size = 12),
            plot.title = element_text(size = 20, face = "bold"))

  })

  rank_samples_df <-  reactive(all_samples_merged %>%
                                 filter(Trait == input$trait_cohorts) %>%
                                 mutate(rank = min_rank(desc(Prop)), Country_Rank = paste(rank, ":", COUNTRY_OF_RECRUITMENT)) %>%
                                 filter(rank <=20))



  output$origin.trends <- renderPlotly(

    cohorts_trends %>%
      filter(Trait == input$trait_cohorts) %>%
      filter(COUNTRY_OF_RECRUITMENT %in% rank_samples_df()$COUNTRY_OF_RECRUITMENT) %>%
      filter(Prop != 0) %>%
      plot_ly(x = ~YEAR,  y = ~Prop * 100,  mode = 'lines+markers', type = 'scatter', connectgaps = TRUE,
              color = ~COUNTRY_OF_RECRUITMENT,
              hoverinfo = 'text',
              hovertext = ~paste('</br> Country: ', COUNTRY_OF_RECRUITMENT,
                                 '</br> Year: ', YEAR,
                                 '</br>', round(Prop * 100, 2), "%"))  %>%
      layout(title = paste0("Proportion of countries of recruitment (2005-2022): ", input$trait_cohorts, " (", input$position_gender, " authors)"),
             xaxis = list(title = "Year"),
             yaxis = list (title = "",tickmode = "array"))
  )



  dailyData3 <- reactive(all_samples_merged2[all_samples_merged2$Trait == input$trait_cohorts2 & round(all_samples_merged2$Prop, 2) > 0,])


  output$map.cohort <- renderLeaflet({

    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = dailyData3(),
                  color = 'black',
                  fillColor =  ~pal(Prop),
                  popup = dailyData3()$popup_info,
                  smoothFactor = 0.2, fillOpacity = 1,  weight = 1,
                  highlightOptions = highlightOptions(stroke = 4, weight = 3, bringToFront = TRUE)
      )  %>%
      addLegend(pal = pal,
                values  = dailyData3()$Prop,
                position = "topleft",
                title = "",
                opacity = 1,
                labFormat = function(type, cuts, p) {  # Here's the trick
                  paste0(labels_to_place)
                }) %>%
      addFullscreenControl(pseudoFullscreen = TRUE)
  })

  output$income_samples.plot <- renderPlotly(
    income_cohorts %>%
      filter(Trait == input$trait_income_cohort) %>%
      plot_ly(x = ~reorder(Income_group, desc(Prop * 100)), y = ~Prop * 100, color = ~Income_group, type = "bar",colors = c(bar_palette[[1]], bar_palette[[2]], bar_palette[[3]], bar_palette[[4]]),
              hoverinfo = 'text',
              hovertext = ~paste('</br> Income: ', Income_group,
                                 '</br>', round(Prop *100 , 2), "%")) %>%
      layout(title = paste0("Level of income: ", input$trait_income_cohort),
             xaxis = list(title = ""),
             yaxis = list (title = "% of GWAS participants", range = c(0, 100),
                           ticktext = list("0","20%", "40%", "60%", "80%", "100%"),
                           tickvals = list(0,20,40,60,80,100),
                           tickmode = "array"))

  )

  output$income_cohorts.trends <- renderPlotly(
    income_cohorts_trends %>%
      filter(Trait == input$trait_income_cohort) %>%
      plot_ly(x = ~DATE,  y = ~Prop * 100,  mode = 'lines+markers', type = 'scatter', connectgaps = TRUE,
              color = ~Income_group,
              colors = c(bar_palette[[1]], bar_palette[[2]], bar_palette[[3]], bar_palette[[4]]),
              hoverinfo = 'text',
              hovertext = ~paste('</br> Income: ', Income_group,
                                 '</br>', round(Prop * 100, 2), "%")) %>%
      layout(title = paste0("Proportion of income groups (2005-2022): ", input$trait_income_cohort),
             xaxis = list(title = "Year"),
             yaxis = list (title = "",
                           tickmode = "array"),
             annotations =
               list(x = 1, y = -0.1, text = "",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=10))
      )

  )

  ancestry_pallete <- c("#bd6173","#4fb958","#a259c7","#8bb533","#d04798","#55c1a1","#d64053","#3d8658","#636ec6",
                        "#d2a136","#5ba5d7","#cd6137","#c281c1","#607629","#a87a41","#a6b263")

  output$ancestry.plot <- renderPlotly(
    ancestry_df %>%
      filter(Trait == input$trait_ancestry) %>%
      plot_ly(x = ~reorder(BROAD_ANCESTRAL_CATEGORY, desc(Prop * 100)), y = ~Prop * 100, color = ~BROAD_ANCESTRAL_CATEGORY, type = "bar",colors = ancestry_pallete,
              hoverinfo = 'text',
              hovertext = ~paste('</br> Ancestry: ', BROAD_ANCESTRAL_CATEGORY,
                                 '</br>', round(Prop * 100, 2), "%"))%>%
      layout(title = paste0("Ancestry: ", input$trait_ancestry),
             xaxis = list(title = ""),
             yaxis = list (title = "% of GWAS participants",
                           tickmode = "array"))
  )


  output$ancestry.trend <- renderPlotly(

    ancestry_trends %>%
      filter(Trait == input$trait_ancestry) %>%
      filter(Prop != 0) %>%
      plot_ly(x = ~YEAR,  y = ~Prop * 100,  mode = 'lines+markers', type = 'scatter', connectgaps = TRUE,
              color = ~BROAD_ANCESTRAL_CATEGORY,
              colors = ancestry_pallete,
              hoverinfo = 'text',
              hovertext = ~paste('</br> Ancestry: ', BROAD_ANCESTRAL_CATEGORY,
                                 '</br>', round(Prop * 100, 2), "%")) %>%
      layout(title = paste0("Proportion of GWAS participants by ethnicity (2005-2022): ", input$trait_ancestry),
             xaxis = list(title = "Year"),
             yaxis = list (title = "",tickmode = "array"))

  )

}

shinyApp(ui, server)
