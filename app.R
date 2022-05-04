#load packages
library(shiny)
library(rsconnect)
library(tidyverse)
library(janitor)

#Load the data
contributions <- read_csv("data.1/campfin/mo_contributions.csv")
candidates <- read_csv("data.1/campfin/candidates.csv")
committees <- read_csv("data.1/campfin/committees.csv")

# DEAL WITH REFUNDS
refunds <- contributions %>% filter(transaction_tp=="22Y") %>%
  mutate_if(is.numeric, funs(. * -1))
no_refunds <- contributions %>% filter(transaction_tp!="22Y")
clean_contribs<-no_refunds %>% add_row(refunds)
party_contribs <- candidates %>% 
  filter(election_yr=="2022" & office_st=="MO" & office=="S") %>%
  inner_join(clean_contribs, by=c("pcc"="cmte_id")) %>%
  group_by(party) %>%
  summarise(total = sum(transaction_amt))

#totals for all candidates, then remove blunt for graphs
all_cand_totals <- candidates %>% 
  filter(election_yr=="2022" & office_st=="MO" & office=="S") %>%
  inner_join(clean_contribs, by=c("pcc"="cmte_id")) %>%
  group_by(cand_name) %>% 
  summarise(total=sum(transaction_amt)) %>% 
  arrange(desc(total))

all_no_blunt <- all_cand_totals %>% 
  filter(cand_name!="BLUNT, ROY")

#totals for candidates, republican, then remove blunt for graphs
rep_cand_totals <- candidates %>% 
  filter(election_yr=="2022" & office_st=="MO" & office=="S") %>%
  inner_join(clean_contribs, by=c("pcc"="cmte_id")) %>%
  filter(party=="REP") %>% 
  group_by(cand_name) %>% 
  summarise(total=sum(transaction_amt)) %>% 
  arrange(desc(total))

rep_no_blunt <- rep_cand_totals %>% 
  filter(cand_name!="BLUNT, ROY")

#totals for cand, dems
dem_cand_totals <- candidates %>% 
  filter(election_yr=="2022" & office_st=="MO" & office=="S") %>%
  inner_join(clean_contribs, by=c("pcc"="cmte_id")) %>%
  filter(party=="DEM") %>% 
  group_by(cand_name) %>% 
  summarise(total=sum(transaction_amt)) %>% 
  arrange(desc(total))

#totals for cands, unk
unk_cand_totals <- candidates %>% 
  filter(election_yr=="2022" & office_st=="MO" & office=="S") %>%
  inner_join(clean_contribs, by=c("pcc"="cmte_id")) %>%
  filter(party=="UNK") %>% 
  group_by(cand_name) %>% 
  summarise(total=sum(transaction_amt))

ui <- fluidPage(
  
  titlePanel("Missouri Senate Race"),
  
  verticalLayout(
    sidebarLayout(
      sidebarPanel( selectInput("var", 
                                label = "Choose a variable to display",
                                choices = c("Republican", "Democrat",
                                            "Unknown"),
                                selected = "Republican"),
                    p("After Sen. Roy Blunt announced he would not be running for reelection for the U.S. Senate, the race for his seat quickly became crowded. Browse this application to learn more about the funding behind the race for Missouri's open Senate seat."),
                    
      ),
      
      mainPanel(
        tableOutput("table"),
        plotOutput("varPlot"))
    ),
  ),
  
  tags$hr(),  
  
  h2("Totals"),
  sidebarLayout(
    sidebarPanel(
      h4("Total contributions by party:"),
      tableOutput("TBL"),
      h4("Total contributions by candidate"),
      tableOutput("all_cands")
    ),
    
    mainPanel(
      h3("Contribution sums for all candidates"),
      plotOutput("candPlot")
    )
  ),
  tags$hr()  
  
)

server <- function(input, output){
  
  output$TBL <- renderTable({
    party_contribs
  })
  
  output$all_cands <- renderTable({
    all_cand_totals
  })
  
  output$table <- renderTable({data <- switch(input$var, 
                                              "Republican" = rep_cand_totals,
                                              "Democrat" = dem_cand_totals,
                                              "Unknown" = unk_cand_totals)
  
  })
  output$candPlot <- renderPlot(ggplot(all_no_blunt, aes(reorder(all_no_blunt$cand_name, all_no_blunt$total))) +
                                  geom_bar(aes(weight = all_no_blunt$total), fill = "#3CAEA3") + 
                                  coord_flip() +
                                  ggtitle("Total candidate contributions") +
                                  xlab("Candidate") +
                                  ylab("Sum of contributions") +
                                  theme_bw(base_size = 16))
  
  output$varPlot <- renderPlot(ggplot({data <- switch(input$var, 
                                                      "Republican" = rep_no_blunt,
                                                      "Democrat" = dem_cand_totals,
                                                      "Unknown" = unk_cand_totals)}, aes(reorder(cand_name, total))) +
                                 geom_bar(aes(weight = total), fill = "#3CAEA3") + 
                                 coord_flip() +
                                 ggtitle("Total candidate contributions") +
                                 xlab("Candidate") +
                                 ylab("Sum of contributions") +
                                 theme_bw(base_size = 16))
  
}

shinyApp(ui=ui , server = server)

deployApp()
