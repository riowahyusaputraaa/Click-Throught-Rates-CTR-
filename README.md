##2
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)

#load data ad placement
ads <- data.frame(
  Day = 1:10,
  Left_Sidebar = c(2.5,2.7,2.8,2.6,3,2.4,2.9,2.5,2.6,2.7),
  Center_Page = c(3.8,3.5,4,3.7,3.9,3.6,4.1,3.4,3.8,3.9),
  Right_Sidebar = c(3.1,2.9,3,3.2,3.3,2.8,3.4,3.1,3.2,3.5)
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Website CTR"),
  dashboardSidebar(
    sidebarMenu(
      
      # menuItem untuk menambahkan sebuah tab menu pada sidebar
      menuItem(
        # text untuk memeberikan tampilan nama pada tab
        text = "Input Data",
        # tabName untuk memberikan identitas yang mewakili tab tersebut
        tabName = "menu_1",
        icon = icon("circle-down")
      ),
      
      menuItem(
        text = "ANOVA Analysis",
        tabName = "menu_2",
        icon = icon("desktop")
      ))
  ),
  dashboardBody(
    tabItems(
      #---Menu 1----
      tabItem(
        tabName = "menu_1",
        fluidPage(
          h2(tags$b("Input Ad Placement Data")),
          fluidRow(
            box(
              title = "New Data",
              width = 4,
              solidHeader = TRUE,
              numericInput("input_x1", "Left Sidebar Data :", value = 2.5),
              numericInput("input_x2", "Center Page Data :", value = 4),
              numericInput("input_x3", "Right Sidebar Data :", value = 3),
              actionButton("addBtn", "Add Data")
            ),
            box(
              status = "primary",
              headerPanel("Ad Placement Data"),
              solidHeader = TRUE,
              br(),
              fluidRow(
                column(
                  width = 12,
                  DT::dataTableOutput("data_table", height = "350px")
                ),
                column(
                  width = 10,
                  align = "left",
                  actionButton("deleteBtn", "Delete Selected Data")
                )
              ),
              width = 8,
              height = "620px"
            )
          )
        )
      ),
      #----menu2---
      tabItem(
        tabName = "menu_2",
        fluidPage(
          h2(tags$b("ANOVA Analysis")),
          fluidRow(
            column(
              width = 8,
              verbatimTextOutput("anova_summary"),
              column(
                width = 12,
                tags$p("Using a 95% confidence level, if p-value < 0.05, then there is at least one significant difference in average CTR based on ad placement on the website. Besides, if p-value > 0.05 then there is no significant difference in average CTR based on ad placement on the website.",
                       style = "color:black; font-size: 18px; text-align:left;")
              )
            )
          ),
          h2(tags$b("Visualization of CTR Performance")),
          plotOutput("boxplot_ctr")
        )
      )
    )
  )
)

# Define server
server <- function(input, output) {
  # Variabel reaktif untuk menyimpan data
  ads_reactive <- reactiveVal(ads)
  
  # Menambahkan dataset ad_placement_data
  ad_placement_data <- reactiveVal(data.frame(
    ad_placement = rep(c("Left_Sidebar", "Center_Page", "Right_Sidebar"), each = 10),
    CTR = c(ads$Left_Sidebar, ads$Center_Page, ads$Right_Sidebar)
  ))
  
  # Variabel reaktif untuk menyimpan hasil ANOVA
  anova_results <- reactiveVal(NULL)
  
  # Fungsi untuk melakukan ANOVA
  perform_anova <- function(data) {
    anova_result <- aov(CTR ~ ad_placement, data = data)
    return(list(
      summary = summary(anova_result)
    ))
  }
  
  # Memperbarui dataset saat tombol "Add Data" ditekan
  observeEvent(input$addBtn, {
    new_data <- data.frame(
      Day = max(ads_reactive()$Day, na.rm = TRUE) + 1,
      Left_Sidebar = input$input_x1,
      Center_Page = input$input_x2,
      Right_Sidebar = input$input_x3
    )
    names(new_data) <- names(ads)
    ads_reactive(rbind(ads_reactive(), new_data))
    
    # Memperbarui dataset ad_placement_data
    new_ad_placement_data <- data.frame(
      ad_placement = rep(c("Left_Sidebar", "Center_Page", "Right_Sidebar"), each = 1),
      CTR = c(new_data$Left_Sidebar, new_data$Center_Page, new_data$Right_Sidebar)
    )
    updated_ad_placement_data <- rbind(ad_placement_data(), new_ad_placement_data)
    ad_placement_data(updated_ad_placement_data)
  })
  
  # Menghapus data yang dipilih saat tombol "Delete Selected Data" ditekan
  observeEvent(input$deleteBtn, {
    # Mendapatkan baris yang dipilih
    selected_rows <- input$data_table_rows_selected
    
    # Menghapus baris yang dipilih dari dataset
    ads_reactive(ads_reactive()[-selected_rows, , drop = FALSE])
    
    # Memperbarui dataset ad_placement_data
    ad_placement_data(ad_placement_data()[-selected_rows, , drop = FALSE])
  })
  
  # Tampilkan dataset dalam tabel dengan checkbox pemilihan
  output$data_table <- DT::renderDataTable({
    DT::datatable(ads_reactive(), selection = 'multiple', options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Menampilkan hasil summary ANOVA dan kesimpulan pada Menu 2
  output$anova_summary <- renderPrint({
    # Tampilkan hasil ANOVA berdasarkan ad_placement_data saat ini
    anova_results(perform_anova(ad_placement_data()))
    anova_results()$summary
  })
  
  # Menampilkan boxplot
  output$boxplot_ctr <- renderPlot({
    boxplot(CTR ~ ad_placement, data = ad_placement_data(), main = "CTR Performance by Ad Placement", ylab = "CTR")
  })
  
  # Memperbarui hasil ANOVA dan boxplot saat ada perubahan pada data
  observe({
    anova_results(perform_anova(ad_placement_data()))
    output$boxplot_ctr <- renderPlot({
      boxplot(CTR ~ ad_placement, data = ad_placement_data(), main = "CTR Performance by Ad Placement", ylab = "CTR")
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)
