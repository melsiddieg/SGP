
source('global.R')

ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel("Sudanese Genetic Variation Portal"),
  sidebarLayout(position = "right", 
    sidebarPanel(
      hidden(tags$div(id="tab2",list(highchartOutput('plot1')))),
      conditionalPanel(condition = 'input.main_table_rows_selected',
                       highchartOutput('plot2')
                       
                       
      )   
      
      
    ),
    mainPanel(
      searchInput(
        inputId = "gene", 
        label = "Enter your search :", 
        placeholder = "BRCA2", 
        btnSearch = icon("search"), 
        width = "100%"
      ),
      
      
      # textInput('gene',label = h4(strong("Gene")),value = 'BRCA2',placeholder = 'BRCA2'),
      # actionButton(inputId = 'act', label = 'GO',class = "btn-primary"),
      textOutput('restxt'),
      hidden(tags$div(id='tab1', list(
        dataTableOutput("main_table")))),
      dataTableOutput('ctable'),
      
      conditionalPanel(condition = 'input.main_table_rows_selected>=1',
                       actionButton('more', 'See More info about this variant',class = "btn-primary"),
                       hidden(tags$div(id='tab3',list(
                         wellPanel(
                           tabsetPanel(
                             tabPanel('Consequence Types', dataTableOutput('conseq')),
                             tabPanel('Population Frequency',dataTableOutput('pops')),
                             tabPanel('Clinvar',dataTableOutput('clin')),
                             tabPanel('Cosmic',dataTableOutput('cosmic')),
                             tabPanel('Drug',dataTableOutput('drugs'))
                           ))
                         )
                       ))
             

            )

      
    )
    
  )
)

server <- function(input, output, session){
  res <- eventReactive(input$gene,{
    reset(input$more)
    reset(input$main_table_rows_selected)
    hide('tab3')
    gene <- toupper(trimws(input$gene))
    aug[HGNC==gene,] %>% head(100)
  })
  
  output$restxt <- renderText(
    if(nrow(res())==0){
      "Query unsuccessful! No results found for your query"
    }else{
      "Query successful!"
      tags$hr()
      show('tab1',anim = TRUE)
      show('tab2',anim = TRUE)
      show('tab3')
      
    })
  
  output$main_table <- renderDataTable({
    sub <- res()
    names(sub)[11:17] <- c('Arb_MAF', 'Nub_MAF','Eas_MAF','Wes_Maf','Sot_MAf','NE_MAF','SW_MAF')
    sub[,1:17]
    },selection='single',extensions = 'Buttons',options = list( scrollX=TRUE,dom = 'Bfrtip', buttons =
                                              c('copy', 'csv', 'excel', 'pdf', 'print')))
  
  output$plot1 <- renderHighchart({
    pData <- res()
    fData <- pData[,.N, by=.(conseq)]
    hchart(fData, 'pie',hcaes(x='conseq', y='N',color='conseq')) %>% hc_title(text='Count of SNV BY Consequence Type')

  })
  
  output$ctable <- renderDataTable({
    s = input$main_table_rows_selected
    hide('tab3')
    if(length(s)){
    }
    cat('you selected row number ',s,"\n")
    if(length(s)){
      sub <- res()
      pData <- sub[s,]
      pData <- pData[,c(11:17)]
      pData
      datatable(pData)%>% formatRound(c('Arb_MAF', 'Nub_MAF','Eas_MAF','Wes_Maf','Sot_MAf','NE_MAF','SW_MAF'),3)
    }
    
  })
  
  output$plot2 <- renderHighchart({
    s = input$main_table_rows_selected
    sub <- res()
    if(length(s)){
      pData <- sub[s,]
      pData <- pData[,c(11:17)]
      test <- as.data.frame(t(pData))
      Pop <- rownames(test)
      test <- cbind(test,Pop)
      names(test) <- c("Allele_Frquency", "Population")
      hchart(test, 'bar',hcaes(x='Population', y='Allele_Frquency',color='Population')) %>% 
        hc_title(text='Population Allele Frquencies') %>% hc_yAxis(max=1)
      
    }
    
  })
  
  
  observeEvent(input$more, {
    sub <- res()
    reset(input$main_table_rows_selected)
    s = input$main_table_rows_selected
    if(length(s)){
      query <- with(sub[s,],paste(Chr,Start, REF, ALT, sep = ":"))
      response <- getVariant(cb, query,resource = 'annotation')
      pops <- response$populationFrequencies[[1]]
      conseq <- response$consequenceTypes[[1]]
      clinvar <- response$variantTraitAssociation$clinvar[[1]]
      cosmic <- response$variantTraitAssociation$cosmic[[1]]
      drugs <- response$geneDrugInteraction[[1]]
        
      show('tab3')
      
    }

    # scores <- conseq$proteinVariantAnnotation$substitutionScores[[3]]
    ## consequence types
    output$conseq <- renderDataTable({
      cons <- conseq %>% select(1:5,transcriptAnnotationFlags,sequenceOntologyTerms) %>% 
        unnest(sequenceOntologyTerms)
      cons
    })
    ## population frequencies
    output$pops <- renderDataTable({
      if (!is.null(pops)){
        pops
      }else{
        data.frame(Message='No Results found')
      }

    })
    ## clinvar
    output$clin <- renderDataTable({
      if(!is.null(clinvar)){
        clinvar
      }else{
        data.frame(Message='No Results found')
      }

    })
    ## cosmic
    output$cosmic <- renderDataTable({
      if(!is.null(cosmic)&length(cosmic)>1){
        cosmic
      }else{
        data.frame(Message='No Results found')
      }
      
    })
    ## drugs
    output$drugs <- renderDataTable({
      if(!is.null(drugs)&length(drugs>1)){
        drugs
      }else{
        data.frame(Message='No Results found')
      }
      
    })
    
  })

    
 
  
  
}

shinyApp(ui, server)