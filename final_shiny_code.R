library(tidyverse)
library(shiny)
library(stringr)
library(ggplot2)
library(Lahman)

bwar_bat <- read_csv("bwar_bat.csv")
bwar_pit <- read_csv("bwar_pit.csv")

#Data preperation for Pitching k-means clustering
pitching <- Pitching%>%
  filter(yearID > 1899 & yearID != 1994 & yearID != 1995 & yearID != 2020)

unique_pitching <- unique(pitching$teamID)
new_names_pitching <- setNames(c
                               ("SFG", "CHC", "CHW", "BOS", "SEA", "NYY", "NYM", "ATL", "CAL", "BAL", "LAD", "HOU", "WSN", "OAK", "MIN", "COL", "CIN", "DET", "MIN", "PHI", "CLE", 
                                 "KCR", "TBR", "KC1", "PHA", "WSH", "STL", "NYG", "SLB", "TEX", "TOR", "FOL", "LAA", "PIT", "SDP", "ARI", "KCF", "ML4", "PTF", "BRO", "SE1", "MON", 
                                 "MIA", "BSN", "BLA", "ANA", "BUF", "ML1", "CHF", "WSN", "IND", "NEW", "BRF", "BLA", "SLF", "MLA"), unique_pitching)
pitching$teamID <- new_names_pitching[match(pitching$teamID, names(new_names_pitching))]

test5 <- pitching %>%
  filter(stint == 1) %>%
  mutate(
    FIP_C = case_when(
      yearID == 2009 ~ 3.097,
      yearID == 2010 ~ 3.079,
      yearID == 2011 ~ 3.025,
      yearID == 2012 ~ 3.095,
      yearID == 2013 ~ 3.048,
      yearID == 2014 ~ 3.132,
      yearID == 2015 ~ 3.134
    )
  ) %>%
  drop_na%>%
  group_by(yearID, playerID, teamID)  

team_FIP <- test5 %>%
  mutate(IP = IPouts / 3) %>%
  group_by(teamID, yearID) %>%
  summarise(
    HR = sum(HR),
    BB = sum(BB),
    HBP = sum(HBP),
    SO = sum(SO),
    IP = sum(IP),
    FIP_C = first(FIP_C)  
  ) %>%
  mutate(FIP = ((13 * HR + 3 * (BB + HBP) - 2 * SO) / IP) + FIP_C)%>%
  rename(team_FIP = FIP)

test7 <- pitching%>%
  filter(stint == 1)%>%
  group_by(yearID, playerID, teamID)

team_SOper9 <- test7 %>%
  group_by(teamID, yearID) %>%
  summarise(
    total_SO = sum(SO),
    total_BB = sum(BB),
    total_IPouts = sum(IPouts)
  ) %>%
  mutate(team_SOper9 = (total_SO / (total_IPouts / 3)) * 9)

team_SOperBB <- test7 %>%
  group_by(teamID, yearID) %>%
  summarise(
    total_SO = sum(SO),
    total_BB = sum(BB)
  ) %>%
  mutate(team_SOperBB = total_SO / total_BB)



teams <- Teams%>%
  filter(yearID > 1899 & yearID != 1994 & yearID != 1995 & yearID != 2020)

unique_teams <- unique(teams$teamID)
new_names_teams <- setNames(c("BRO", "BSN", "CHC", "CIN", "NYG", "PHI", "PIT", "STL", "BLA", "BOS", "CHW", "CLE", "DET", "MLA", "PHA", "WSH", "SLB", "NYY", "BLA", "BRF", "BUF", "CHF", "IND", "KCF", "PTF", "SLF", "NEW", "ML1", "BAL", "KC1", "LAD", "SFG", "LAA", "MIN", "WSN", "HOU", "NYM", "CAL", "ATL", "OAK", "KCR", "SE1", "MON", "SDP", "ML4", "TEX", "SEA", "TOR", "COL", "FOL", "ANA", "TBR", "ARI", "MIN", "WSN", "MIA"), unique_teams)
teams$teamID <- new_names_teams[match(teams$teamID, names(new_names_teams))]
sort(unique(teams$teamID))


test2 <- bwar_pit%>%
  filter(stint_ID == 1)%>%
  group_by(year_ID, player_ID, team_ID)%>%
  summarise(mean_ERA_plus = mean(ERA_plus))%>%
  drop_na%>%
  select(year_ID, player_ID, team_ID, mean_ERA_plus)

team_ERA_plus <- test2 %>%
  group_by(team_ID, year_ID)%>%
  summarise(team_ERA_plus = mean(mean_ERA_plus))%>%
  rename(teamID = team_ID, yearID = year_ID)

team_ERA <- teams%>%
  group_by(teamID, yearID)%>%
  summarise(team_ERA = mean(ERA))

team_WHIP <- teams%>%
  group_by(teamID, yearID)%>%
  summarise(
    HA = sum(HA),
    BBA = sum(BBA),
    IPouts = sum(IPouts)
  )%>%
  mutate(team_WHIP = 3*(HA + BBA)/IPouts)
  

pitching_stats <- team_FIP %>%
  inner_join(team_ERA, by = c("yearID", "teamID")) %>%
  inner_join(team_ERA_plus, by = c("yearID", "teamID"))%>%
  inner_join(team_SOper9, by = c("yearID", "teamID"))%>%
  inner_join(team_SOperBB, by = c("yearID", "teamID"))%>%
  inner_join(team_WHIP, by = c("yearID", "teamID"))%>%
  select(teamID, yearID, team_ERA, team_ERA_plus, team_FIP, team_SOper9, team_SOperBB, team_WHIP)

pitching_stats <- pitching_stats%>%
  ungroup()%>%
  filter(teamID == "SFG")%>%
  filter(yearID %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015))


#Data preperation for Batting k-means clustering
batting <- Batting%>%
  filter(yearID > 1899 & yearID != 1994 & yearID != 1995 & yearID != 2020)
unique_batting <- unique(batting$teamID)
new_names_batting <- setNames(c
                              ("SFG", "CHC", "CHW", "BOS", "SEA", "NYY", "NYM", "ATL", "CAL", "BAL", "LAD", "HOU", "WSN", "OAK", "MIN", "COL", "CIN", "DET", "MIN", "PHI", "CLE", 
                                "KCR", "TBR", "KC1", "PHA", "WSH", "STL", "NYG", "SLB", "TEX", "TOR", "FOL", "LAA", "PIT", "SDP", "ARI", "KCF", "ML4", "PTF", "BRO", "SE1", "MON", 
                                "MIA", "BSN", "BLA", "ANA", "BUF", "ML1", "CHF", "WSN", "IND", "NEW", "BRF", "BLA", "SLF", "MLA"), unique_batting)
batting$teamID <- new_names_batting[match(batting$teamID, names(new_names_batting))]

test6 <- teams%>%
  select(R, RA, G, H, BB, HBP, AB, BB, HBP, SF, X2B, X3B, HR, HA, BBA, IPouts, FP, yearID, teamID)%>%
  filter(yearID > 1900)%>%
  replace_na(list(HBP = 0, SF = 0)) %>% 
  mutate(RD = (R - RA) / G) %>% 
  mutate(OBP = (H + BB + HBP)/(AB + BB + HBP + SF)) %>%  
  mutate(SLG = (H + X2B + 2*X3B + 3*HR)/AB) %>% 
  mutate(OPS = OBP + SLG)

team_OPS <- test6%>%
  select(yearID, teamID, OPS)%>%
  rename(team_OPS = OPS)
team_RD <- test6%>%
  select(yearID, teamID, RD)%>%
  rename(team_RD = RD)
team_HR <- test6%>%
  select(yearID, teamID, HR)%>%
  rename(team_HR = HR)

team_BRruns <- bwar_bat %>%
  select(team_ID, year_ID, runs_br) %>%
  drop_na(runs_br) %>%
  rename(yearID = year_ID)%>%
  rename(teamID = team_ID)%>%
  group_by(yearID, teamID) %>%
  summarise(team_BRruns = sum(runs_br, na.rm = TRUE), .groups = "drop")

test8 <- batting%>%
  filter(stint == 1)%>%
  group_by(yearID, playerID, teamID)

team_RBI <- test8%>%
  group_by(yearID, , teamID)%>%
  summarise(team_RBI = mean(RBI))

batting_stats <- team_RBI %>%
  inner_join(team_OPS, by = c("yearID", "teamID")) %>%
  inner_join(team_BRruns, by = c("yearID", "teamID"))%>%
  inner_join(team_HR, by = c("yearID", "teamID"))%>%
  inner_join(team_RD, by = c("yearID", "teamID"))%>%
  select(teamID, yearID, team_OPS, team_RBI, team_BRruns, team_HR, team_RD)

batting_stats <- batting_stats%>%
  ungroup()%>%
  filter(teamID == "SFG")%>%
  filter(yearID %in% c(2009, 2010, 2011, 2012, 2013, 2014, 2015))

#Data prep for Giants vs. the League
team_stats = Teams %>%
  mutate(teamID = franchID) %>%
  mutate(
    Singles = H - X2B - X3B - HR,
    TB = Singles + 2 * X2B + 3 * X3B + 4 * HR,
    SLG = TB / AB,
    OPS = (H + BB) / AB + SLG,
    WHIP = (BB + H) / IPouts * 3,
    WinPct = W / (W + L)
  )

team_stats_clean = team_stats %>%
  filter(
    yearID >= 1900,
    !is.na(OPS), !is.na(WHIP), !is.na(FP),
    OPS > 0, WHIP > 0, FP > 0
  )

league_avg = team_stats_clean %>%
  filter(yearID >= 2010, yearID <= 2015) %>%
  group_by(yearID) %>%
  summarise(
    OPS = mean(OPS, na.rm = TRUE),
    WHIP = mean(WHIP, na.rm = TRUE),
    FP = mean(FP, na.rm = TRUE),
    WinPct = mean(WinPct, na.rm = TRUE)
  ) %>%
  mutate(Source = "League Average")

giants_stats = team_stats_clean %>%
  filter(teamID == "SFG", yearID >= 2010, yearID <= 2015) %>%
  select(yearID, OPS, WHIP, FP, WinPct) %>%
  mutate(Source = "Giants")

comparison_long = bind_rows(giants_stats, league_avg) %>%
  pivot_longer(cols = c("OPS", "WHIP", "FP", "WinPct"), names_to = "Metric", values_to = "Value")


#Shiny app
ui <- fluidPage(
  titlePanel("Giants vs League Average: 2010–2015 and K-Means Clustering"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Giants vs. the League",
        selectInput("metric", "Select Metric:", choices = c("OPS", "WHIP", "FP", "WinPct"))
      ),
      conditionalPanel(
        condition = "input.tabs == 'Pitching' || input.tabs == 'Batting'",
        helpText("Select statistics and explore how the Giants compared to other MLB teams during their World Series winning seasons."),
        helpText("Note: K-means clustering may take a few seconds to run."),
        uiOutput("dynamic_sidebar")  
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",  
        tabPanel("Pitching", plotOutput("pitching_plot")),
        tabPanel("Batting", plotOutput("batting_plot")),
        
        tabPanel("Giants vs. the League",
                 plotOutput("linePlot"),
                 tableOutput("summaryTable")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  observe({
    if (is.null(input$tabs)) {
      updateTabsetPanel(session, "tabs", selected = "Pitching")
    }
  })
  
  output$dynamic_sidebar <- renderUI({
    selected_tab <- input$tabs
    
    if (selected_tab == "Pitching") {
      # UI elements for Pitching
      tagList(
        checkboxGroupInput("pitching_vars", "Select variables for clustering:",
                           choices = c("team_ERA", "team_ERA_plus", "team_FIP", "team_SOper9", "team_SOperBB", "team_WHIP"),
                           selected = c("team_ERA", "team_ERA_plus", "team_FIP", "team_SOper9", "team_SOperBB", "team_WHIP")),
        selectInput("pitching_x_var", "X-axis variable:", choices = NULL),
        selectInput("pitching_y_var", "Y-axis variable:", choices = NULL),
        sliderInput("pitching_k", "Number of clusters:", min = 2, max = 4, value = 2),
        sliderInput("pitching_year_range", "Year range:",
                    min = min(pitching_stats$yearID), max = max(pitching_stats$yearID),
                    value = c(2009, 2015), step = 1)
      )
    } else if (selected_tab == "Batting") {
      # UI elements for Batting
      tagList(
        checkboxGroupInput("batting_vars", "Select variables for clustering:",
                           choices = c("team_RBI", "team_OPS", "team_BRruns", "team_HR", "team_RD"),
                           selected = c("team_RBI", "team_OPS", "team_BRruns", "team_HR", "team_RD")),
        selectInput("batting_x_var", "X-axis variable:", choices = NULL),
        selectInput("batting_y_var", "Y-axis variable:", choices = NULL),
        sliderInput("batting_k", "Number of clusters:", min = 2, max = 4, value = 2),
        sliderInput("batting_year_range", "Year range:",
                    min = min(batting_stats$yearID), max = max(batting_stats$yearID),
                    value = c(2009, 2015), step = 1)
      )
    }
  })
  
  observeEvent(input$pitching_vars, {
    updateSelectInput(session, "pitching_x_var", choices = input$pitching_vars, selected = input$pitching_vars[1])
    updateSelectInput(session, "pitching_y_var", choices = input$pitching_vars, selected = input$pitching_vars[2])
  })
  
  observeEvent(input$batting_vars, {
    updateSelectInput(session, "batting_x_var", choices = input$batting_vars, selected = input$batting_vars[1])
    updateSelectInput(session, "batting_y_var", choices = input$batting_vars, selected = input$batting_vars[2])
  })
  
  filtered_pitching_stats <- reactive({
    req(input$pitching_vars)
    pitching_stats %>%
      filter(yearID >= input$pitching_year_range[1],
             yearID <= input$pitching_year_range[2]) %>%
      select(yearID, teamID, all_of(input$pitching_vars)) %>%
      na.omit()
  })
  
  filtered_batting_stats <- reactive({
    req(input$batting_vars)
    batting_stats %>%
      filter(yearID >= input$batting_year_range[1],
             yearID <= input$batting_year_range[2]) %>%
      select(yearID, teamID, all_of(input$batting_vars)) %>%
      na.omit()
  })
  
  # K-means clustering for pitching
  kmeans_pitching_result <- reactive({
    features <- filtered_pitching_stats() %>% select(-yearID, -teamID)
    req(ncol(features) >= 1)
    set.seed(42)
    kmeans(features, centers = input$pitching_k, iter.max = 100)
  })
  
  # K-means clustering for batting
  kmeans_batting_result <- reactive({
    features <- filtered_batting_stats() %>% select(-yearID, -teamID)
    req(ncol(features) >= 1)
    set.seed(42)
    kmeans(features, centers = input$batting_k, iter.max = 100)
  })
  
  # Render the Pitching Plot
  output$pitching_plot <- renderPlot({
    req(input$pitching_x_var, input$pitching_y_var)
    df <- filtered_pitching_stats()
    df$cluster <- factor(kmeans_pitching_result()$cluster)
    
    ggplot(df, aes_string(x = input$pitching_x_var, y = input$pitching_y_var)) +
      geom_point(aes(color = cluster), size = 3, alpha = 0.7) +
      geom_text(data = df %>% filter(teamID == "SFG"),
                aes(label = yearID),
                vjust = -1, fontface = "bold") +
      theme_minimal() +
      labs(
        title = "K-Means Clustering For Giants Pitching",
        subtitle = "San Francisco Giants labeled with year",
        x = input$pitching_x_var,
        y = input$pitching_y_var
      )
  })
  
  # Render the Batting Plot
  output$batting_plot <- renderPlot({
    req(input$batting_x_var, input$batting_y_var)
    df <- filtered_batting_stats()
    df$cluster <- factor(kmeans_batting_result()$cluster)
    
    ggplot(df, aes_string(x = input$batting_x_var, y = input$batting_y_var)) +
      geom_point(aes(color = cluster), size = 3, alpha = 0.7) +
      geom_text(data = df %>% filter(teamID == "SFG"),
                aes(label = yearID),
                vjust = -1, fontface = "bold") +
      theme_minimal() +
      labs(
        title = "K-Means Clustering For Giants Batting",
        subtitle = "San Francisco Giants labeled with year",
        x = input$batting_x_var,
        y = input$batting_y_var
      )
  })
  
# Giants vs. the League plot and table for the Metric tab
  filtered_data <- reactive({
    comparison_long %>%
      filter(Metric == input$metric)
  })
  
  output$linePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = yearID, y = Value, color = Source)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      labs(
        title = paste("Giants vs League Average for", input$metric),
        x = "Year", y = input$metric
      ) +
      scale_x_continuous(breaks = 2010:2015) +
      theme_minimal() +
      theme(legend.title = element_blank())
  })
  
  output$summaryTable <- renderTable({
    filtered_data() %>%
      select(Year = yearID, Team = Source, !!input$metric := Value) %>%
      arrange(Year, Team)
  }, digits = 3)
}

shinyApp(ui = ui, server = server)
