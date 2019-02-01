
source('global.R')
library(rlang)



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
        value='TET1',
        placeholder = "BRCA2",
        btnSearch = icon("search"),
        width = "100%"
      ),
      # selectizeInput('gene', label=NULL, choices=NULL, selected = 'TET1', multiple = FALSE,
      #                options = list(placeholder = 'select a gene',maxOptions = 5)),
      
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
# genes <- getXref(object=cb, ids="BRC", resource="starts_with", param = cbp) %>% select(name)
 # updateSelectizeInput(session, 'gene', choices = genes, server = TRUE)
  res <- eventReactive(input$gene,{
    cat("input is: ",str(input$gene),"\n")
    reset(input$more)
    reset(input$main_table_rows_selected)
    hide('tab3')
    if(nchar(input$gene)>0 ){
      gene <- toupper(trimws(input$gene))
      cat("your input gene is :", gene,"\n")
      aug[HGNC==gene,] 
    }
    
    
  })
  
  output$restxt <- renderText(
    if(!toupper(trimws(input$gene))%in%genes){
      "Query unsuccessful! Either your query was not found,\n or you did not input a valid Hugo Symbol gene name"
    }else{
      "Query successful!"
      tags$hr()
      show('tab1',anim = TRUE)
      show('tab2',anim = TRUE)
      show('tab3')
      
    })
  
  output$main_table <- renderDataTable({
    
    sub <- res()
    #rint(nrow(sub))
    if(nrow(sub)>0){
      # names(sub)[10:16] <- c('Arb_MAF', 'Nub_MAF','Eas_MAF','Wes_MAF','Sot_MAF','NE_MAF','SW_MAF')
      sub[,1:16]
    }
    
    },selection='single',extensions = 'Buttons',options = list( scrollX=TRUE,scrollY=TRUE,dom = 'Bfrtip', buttons =
                                              c('copy', 'csv', 'excel', 'print')))
  
  output$plot1 <- renderHighchart({
    if(nrow(res())>0){
      pData <- res()
      fData <- pData[,.N, by=.(conseq)]
      hchart(fData, 'pie',hcaes(x='conseq', y='N',color='conseq')) %>% hc_title(text='Count of SNV BY Consequence Type')
        }
   
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
      pData <- pData[,c(10:16)]
      pData
      datatable(pData)%>% formatRound(c('Arb_MAF', 'Nub_MAF','Eas_MAF','Wes_MAF','Sot_MAF','NE_MAF','SW_MAF'),3)
    }
    
  })
  
  output$plot2 <- renderHighchart({
    s = input$main_table_rows_selected
    sub <- res()
    if(length(s)){
      pData <- sub[s,]
      pData <- pData[,c(10:16)]
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
      if(!is.null(conseq)&length(conseq)>1){
        cons <- conseq %>% select(1:5,transcriptAnnotationFlags,sequenceOntologyTerms) %>% 
          unnest(sequenceOntologyTerms)
        N <- nrow(cons)-1
        cons[1:N,c(1:5,7,8)]
      }
      
      
    })
    ## population frequencies
    output$pops <- renderDataTable({
      if (!is.null(pops)&length(pops)>1){
        pops[,c(1:2,5:9)]
      }else{
        data.frame(Message='No Results found')
      }

    })
    ## clinvar
    output$clin <- renderDataTable({
      if(!is.null(clinvar)&length(clinvar)>1){
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