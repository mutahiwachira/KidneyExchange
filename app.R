ui <- fluidPage(
  titlePanel("KDX | Multiple-Transplant Kidney Exchange"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      strong("How it works"),
      hr(),
      p("This app allows non-matching donor-patient pairs to find other pairs in the same situation and donate kidneys between them."),
      br(),
      p("If it happens that the donor in pair A matches the patient in pair B, and the donor in pair B matches patient A, then a paired donation can take place."),
      br(),
      p("Through paired donation, two people who had a willing but unmatched donor can end up getting a kidney transplation without the need for a third party like donation from a deceased organ donor."),
      br(),
      p("This app generalizes this idea to allow two, three, four or even more pairs of unmatched donor-patients to exchange kidneys in donation chain.")
    ),
    mainPanel(
      rHandsontableOutput("patient_donor_rhot"),
      hr(),
      htmlOutput("results"),
      br(),
      hr(),
      tags$img(src = "kidney-chain-illustration.png", width = 500)
    )
  )
)

server <- function(input, output) {
  patient_donor_data <- reactive({
    
    num_pairs <- sample(c(5,6,7,8,9,10), 1)
    
    df <- instantiate_patient_donor_df(num_pairs = num_pairs,
                                 min_num_preferences = 2,
                                 max_num_preferences = 4)
    return(df)
  })
  
  output$patient_donor_rhot <- renderRHandsontable({
    patients_and_donors_df <- patient_donor_data()
    df_rhot <- patients_and_donors_df |> 
      patient_donor_rhot()
    
    return(df_rhot)
  })
  
  output$results <- renderUI({
    df <- patient_donor_data()
    kidney_chain <- df |> 
      make_exchange_matrix() |> 
      run_ttcc()
    
    if(is.null(kidney_chain$error)){
      roster <- HTML("<ul>", matchings_roster(kidney_chain, df), "</ul>")
      return(roster) 
    } else {
      return("No matching was found.")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
