#load packages
library(shiny)
library(rsconnect)
library(tidyverse)
library(janitor)

#Load the data
contribs <- read_csv("data.1/contribs.csv")
  
  #make totals for each party

party_contribs <- contribs %>% 
  filter(total_receipts!="0.00") %>%
  group_by(party_full) %>%
  summarise(total = sum(total_receipts))%>%
  mutate_each(funs(prettyNum(., big.mark=","))) %>% 
  mutate(party_full = str_to_title(party_full))


#totals for all candidates, then add commas
names<- data.frame(names = c("Lucas Kunce", "Eric Schmitt", "Vicky J. Hartzler", "Dave Schatz", "Eric Greitens", "Mark T. Mccloskey", "Scott Sifton", "Spencer Ross Toder", "Timothy Jacob Shepard", "Jewel William Kelly Jr.", "Steven Craig Price", "Gena Ross", "Nicholas Crane Strauss", "Trudy Busch Valentine"))

all_cand_totals <- contribs %>% 
  filter(total_receipts!=0.00)%>%
  arrange(desc(total_receipts)) %>% 
  mutate(party_full = str_to_title(party_full)) %>% 
  mutate(names, .before=1)%>%  
  select(names, party_full, total_receipts)

#names<-sapply(strsplit(all_cand_totals$candidate_name, split=", "),function(x)  {paste(rev(x),collapse=" ")})  %>%
#str_to_title())
#I got so frustrated by not knowing what to do with the Mr.s and Jr.s and the initials whatnot that I'm just going to write their names in a vector since there are only 14. 

all_cand_commas <- all_cand_totals %>%
  mutate_each(funs(prettyNum(., big.mark=",")))

all_cand_graph <- all_cand_totals %>% 
  select(names, total_receipts)


#totals for republican candidates
rep_cand_totals <- all_cand_totals %>% 
  filter(party_full=="Republican Party")%>%  
  select(names, total_receipts)

rep_cand_commas <- rep_cand_totals %>% 
  mutate_each(funs(prettyNum(., big.mark=",")))

#totals for dems

dem_cand_totals <- all_cand_totals %>% 
  filter(party_full=="Democratic Party")%>%  
  select(names, total_receipts)

dem_cand_commas <- dem_cand_totals %>% 
  mutate_each(funs(prettyNum(., big.mark=",")))

#totals for unk, independent
unk_cand_totals <- all_cand_totals %>% 
  filter(party_full=="Independent" | party_full=="Unknown")%>%  
  select(names, total_receipts)

unk_cand_commas <- unk_cand_totals %>%
  mutate_each(funs(prettyNum(., big.mark=",")))


#Run the shiny app template

ui <- fluidPage(
  
  h1("Missouri Senate Race"),
  
  verticalLayout(
    sidebarLayout(
      sidebarPanel( selectInput("var", 
                                label = "Choose a party to display",
                                choices = c("Republican", "Democrat",
                                            "Unknown/Independent"),
                                selected = "Republican"),
                    p("After Sen. Roy Blunt announced he would not be running for reelection for the U.S. Senate, the race for his seat quickly became crowded. Browse this application to learn more about the funding behind the race for Missouri's open Senate seat."),
                    p("This data is taken from the Federal Exchange Commission's website. It was accessed in early May."),
                    tags$a(href="https://www.fec.gov/data/elections/senate/MO/2022/", "Click here to access the FEC's campaign finance data.")
      ),
      
      mainPanel(
        tableOutput("table"),
        plotOutput("varPlot")
      )
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
    all_cand_commas
  })
  
  output$table <- renderTable({data <- switch(input$var, 
                                              "Republican" = rep_cand_commas,
                                              "Democrat" = dem_cand_commas,
                                              "Unknown/Independent" = unk_cand_commas)
  
  })
  output$candPlot <- renderPlot(ggplot(all_cand_graph, aes(reorder(names, total_receipts))) +
                                  geom_bar(aes(weight = total_receipts), fill = "#3CAEA3") + 
                                  coord_flip() +
                                  ggtitle("Total candidate contributions") +
                                  xlab("Candidate") +
                                  ylab("Sum of contributions") +
                                  theme_bw(base_size = 16)+
                                  scale_y_continuous())
  
  
  output$varPlot <- renderPlot(ggplot({data <- switch(input$var,
                                                      "Republican" = rep_cand_totals,
                                                      "Democrat" = dem_cand_totals,
                                                      "Unknown/Independent" = unk_cand_totals)}, aes(reorder(names, total_receipts))) +
                                 geom_bar(aes(weight = total_receipts), fill = switch(input$var, 
                                                                                      "Republican" = "#ED5533",
                                                                                      "Democrat" = "#173F5F",
                                                                                      "Unknown/Independent" = "#F6D55C")) + 
                                 coord_flip() +
                                 ggtitle("Candidate contributions") +
                                 xlab("Candidate") +
                                 ylab("Sum of contributions") +
                                 theme_bw(base_size = 16)+
                                 scale_y_continuous())
  
  
}

shinyApp(ui=ui , server = server)
