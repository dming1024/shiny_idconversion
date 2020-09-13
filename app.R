
require(shiny)
require(shinydashboard)
require(biomaRt)

ui <- dashboardPage(
  dashboardHeader(title = "医学统计园"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("ID Convert", tabName = "id_transfrom", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "id_transfrom",
        fluidRow(
          box(
            status = "warning",
            title = "Control Panel",
            height = 450,
            width = 5,
            column(
              8,
              fileInput("file_df", label ="Choose csv File",
                        accept = ".csv")
            ),
            
            column(
              4,
              br(),
              actionButton("review","Review Genes",class="btn btn-primary",width="150px")
            ),
            column(
              5,
              radioButtons("input_lables",
                           "Input genes belong to:",
                           choices = c("EntrezID",
                                       "EnsembleID",
                                       "AliasSymbol",
                                       "OfficalSymbol"),
                           selected = "OfficalSymbol")
            ),
            column(
              5,offset = 1,
              radioButtons("out_lables",
                           "Converts to:",
                           choices = c("EntrezID",
                                       "EnsembleID",
                                       "AliasSymbol",
                                       "OfficalSymbol"),
                           selected = "EntrezID")
            ),
            column(
              12,
              radioButtons("species","Species",
                           choices=c("Human","Mus"),
                           selected = "Human",
                           inline = T)
            ),
            column(
              5,
              actionButton("convert_button","Analyze",class="btn btn-primary",width="150px")
            ),
            column(
              5,offset = 1,
              downloadButton("downloadData","Download",class="btn btn-success",style="width:150px")
            )
          ),
          box(
            status = "warning",
            title = "Review",
            height = 450,
            width = 5,
            column(
              3,
              tableOutput("gene_list")
            ),
            column(
              5,offset = 1,
              tableOutput("gene_result")
            ),
            column(12,
                   verbatimTextOutput('process',
                                      placeholder = T)
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  df<-reactive({
    if(is.null(input$file_df)){gene_list=read.csv("genelist.csv",
                                                  header = T,stringsAsFactors = F)}
    else{gene_list=read.csv(input$file_df[1,4],header = T,stringsAsFactors = F)}
    colnames(gene_list)="genelist"
    return(gene_list)
  })
  
  
  input_symbol=reactive({
    switch (input$input_lables,
            EntrezID = "entrezgene_id",
            EnsembleID="ensembl_gene_id",
            AliasSymbol="external_synonym",
            OfficalSymbol="hgnc_symbol"
    )
  })
  
  output_symbol=reactive({
    switch (input$out_lables,
            EntrezID = "entrezgene_id",
            EnsembleID="ensembl_gene_id",
            AliasSymbol="external_synonym",
            OfficalSymbol="hgnc_symbol"
    )
  })
  
  selected_species=reactive({
    switch (input$species,
            Human = "hsapiens_gene_ensembl",
            Mus="mmusculus_gene_ensembl"
    )
  })
  
  output$process=renderText({
    invalidateLater(500, session)
    paste(Sys.time(),"",sep=" ")
    
  })
  
  observeEvent(input$convert_button,
               {
                 ensemble=useEnsembl("ensembl")
                 ensemble_human=useEnsembl("ensembl",dataset = selected_species())
                 
                 gene_results=getBM(
                   attributes = c(input_symbol(),output_symbol()),
                   filters = input_symbol(),
                   values = df()$genelist,
                   mart = ensemble_human
                 )
                 
                 output$gene_result<-renderTable(
                   {
                     head(gene_results,8)
                   }
                 )
                 output$downloadData <- downloadHandler(
                   filename = function() {
                     paste("data-", Sys.Date(), ".csv", sep="")
                   },
                   content = function(file) {
                     write.csv(gene_results, file)
                   }
                 )
                 
               })
  observeEvent(input$review,{
    output$gene_list<-renderTable(
      {
        head(df(),8)
      }
    )
  }
  )
  
}

shinyApp(ui, server)
