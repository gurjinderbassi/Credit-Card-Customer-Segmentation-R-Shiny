library(shiny)
library(gsheet)
library(tidyverse)
library(shinyWidgets)
library(corrplot)
library(plotly)
library(FactoMineR)
library(ggrepel)
library(psych)
library(NbClust)
library(shinydashboard)
library(viridis)
library(cluster)
library(factoextra)
library(ggfortify)
library(GGally)
library(ggpubr)
library(shinycssloaders)
library(shiny)
library(cluster)
library(dplyr)
library(magrittr)
library(ggplot2)
library(plotly)
library(data.table)
#library(caret)
#library(ggbiplot)
library(tidyr)
library(factoextra)
library(DataExplorer)
library(clValid)

# Import and cleaning

#Loading Data
credit_df <- read.csv('/Users/gurjinderkaur/Documents/R/CC GENERAL.csv')

#Removing customer id
library(dplyr)
credit_df <- select(credit_df,-c('CUST_ID'))

#Removing NA
credit_df <- na.omit(credit_df)

#scale each variable to have a mean of 0 and sd of 1
# credit_df <- scale(credit_df)

set.seed(1)

head(credit_df)

# preprocessing of data to handle noise and outliers
outlier_norm <- function(df){
  df2 <- data.frame(df)
  for(i in 1:ncol(df2)) {       # for-loop over columns
    #print(i)
    mean = mean(df2[,i])
    #print(mean)
    std = sd(df2[,i])
    Tmin = mean-(std)
    Tmax = mean+(std)
    a1 = df2[,i] < Tmin
    #print(nrow(a1))
    a2 = df2[,i] > Tmax
    
    df2[a1,i] <- median(df2[,i])
    df2[a2,i] <- median(df2[,i])
    #print(head(df2))
  }
  return(df2)
}
outlier_removed <- outlier_norm(credit_df)
outlier_removed$AMT_SPENT <- outlier_removed$CREDIT_LIMIT - outlier_removed$BALANCE
outlier_removed$CREDIT_UTILIZATION <- outlier_removed$AMT_SPENT/outlier_removed$CREDIT_LIMIT
credit_df <- outlier_removed
credit_df_scaled <- scale(outlier_removed)
attributes <- colnames(credit_df)

X <- outlier_removed
linkage_methods <- c('ward','single','complete','average')

ui <- function(request) {
  
  dashboardPage(
    
    dashboardHeader(title = "Credit Card Customer Segmentation",
                    titleWidth = 350),
    
    dashboardSidebar(
      
      sidebarMenu(
        
        id = "sidebar",
        
        menuItem(text = "Clustering",
                 tabName = "clustering",
                 icon = icon(name = "fas fa-tachometer-alt"),
                 
                 menuSubItem(text = "K-means clustering",
                             tabName = "Kmeans",
                             icon = icon(name = "fas fa-angle-double-right")),
                 menuSubItem(text = "Hierarchical clustering",
                             tabName = "hierarchical",
                             icon = icon(name = "fas fa-angle-double-right"))
                 
              
        ),
        menuItem(text = 'About',
                 tabName = 'abt',
                 icon = icon(name = 'fas fa-tachometer-alt'))
        )),
  
  dashboardBody(
    
    tags$style(type = "text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    
    fluidRow(
      
      tabItems(
        
        tabItem(tabName = 'abt',
                h2('Statistical Learning II Project'),
                
                ),
        
        
        tabItem(tabName = "Kmeans",
                h2("K-means Clustering"),
                
                column(width = 3,
                       
                       tabPanel(title = "Kmeans",
                                numericInput("k", "Enter the value of k:", value = 3, min = 1, max = 6),
                                
                                fluidRow(valueBoxOutput(outputId = "CLUS.INDEX",
                                                        width = 12)))),
                
                column(width = 9,
                       
                       tabBox(width = 12,
                              
                              tabPanel(title = "CLUSTERING PLOT",
                                       plotOutput('kmean_plot')%>% withSpinner(color="#0dc5c1")),
                              tabPanel(title = 'SILHOUETTE', 
                                       plotlyOutput('sil_plot')%>% withSpinner(color="#0dc5c1")),
                              tabPanel(title = 'CLUSTER SIZE',
                                       plotlyOutput('pie_chart')%>% withSpinner(color ='#0dc5c1' )),
                              tabPanel(title = 'CREDIT LIMIT',
                                       plotlyOutput('creditlimit_plot')%>% withSpinner(color = '#0dc5c1')),
                              # tabPanel(title = 'PURCHASE FREQUENCY',
                              #          plotlyOutput('purch_freq_plot')%>% withSpinner(color = '#0dc5c1')),
                              tabPanel(title = 'CREDIT UTILIZATION',
                                       plotlyOutput('cred_util_plot')%>% withSpinner(color = '#0dc5c1')),
                              tabPanel('ANALYZE',
                                       selectInput('attribute','Select any attribute from the list:', attributes),
                                       plotlyOutput('analysis_plot')),
                              tabPanel(title = "HISTOGRAMS",
                                       numericInput('cluster_num','Choose cluster:', value = 1, min = 1, max = 6),
                                       br(),
                                       plotOutput('hist_plot')%>% withSpinner(color="#0dc5c1"))
                              
                              ))),
        tabItem(tabName = "hierarchical",
                h2("Hierarchical Clustering"),
                
                column(width = 3,
                       
                       tabPanel(title = "Hierarchical",
                                
                                selectInput("linkage", "Select Linkage method", linkage_methods),
                                br(),
                                numericInput('cut_dendrogram','Specify number of clusters for cutree():', value = 3, min=1, max = 6))),
                
                
                column(width = 9,
                       
                       tabBox(width = 12,
                              
                              tabPanel(title = "DENDROGRAM",
                                       plotOutput('hierarchical_plot')%>% withSpinner(color="#0dc5c1")),
                              tabPanel(title = 'SILHOUETTE',
                                       plotlyOutput('sil_hc_plot')%>% withSpinner(color='#0dc5c1')),
                              tabPanel(title = 'CLUSTER SIZE',
                                       plotlyOutput('pie_chart_hc')%>% withSpinner(color ='#0dc5c1' )),
                              tabPanel(title = 'CREDIT LIMIT',
                                       plotlyOutput('creditlimit_plot_hc')%>% withSpinner(color = '#0dc5c1')),
                              # tabPanel(title = 'PURCHASE FREQUENCY',
                              #          plotlyOutput('purch_freq_plot_hc')%>% withSpinner(color = '#0dc5c1')),
                              tabPanel(title = 'CREDIT UTILIZATION',
                                       plotlyOutput('cred_util_plot_hc')%>% withSpinner(color = '#0dc5c1')),
                              tabPanel('ANALYZE',
                                       selectInput('attribute_hc','Select any attribute from the list:', attributes),
                                       plotlyOutput('analysis_plot_hc')),
                              tabPanel(title = "HISTOGRAMS",
                                       numericInput('cluster_num_hc','Choose cluster:', value = 1, min = 1, max = 6),
                                       br(),
                                       plotOutput('hist_plot_hc')%>% withSpinner(color="#0dc5c1"))
                       )))

       ))))}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  CLUSTERING <- reactive(
    kmeans(credit_df_scaled,
           centers = input$k, nstart = 25)
    )
  # CLUSTERING <- kmeans(credit_df_scaled, centers = input$k, nstart = 25)
  
  output$kmean_plot <- renderPlot({
    plot(fviz_cluster(CLUSTERING(), data = credit_df_scaled))
   }, res = 96)
  
   
   
   DF2 <- reactive({
     X$cluster <- CLUSTERING()$cluster
     data.frame(X)
   })
  
  silhouette_score <- reactive({
    silhouette(DF2()$cluster, dist(credit_df))
  }
  )
  
  
  #  output$sil_plot <- renderPlot(
  #    plot(fviz_silhouette(silhouette_score(), print.summary=FALSE))
  # )
   
   
   
   output$sil_plot <- renderPlotly({
     
     ggplotly(
       
       ggplot2::autoplot(object = silhouette(DF2()$cluster, dist(credit_df))) +
         
         scale_fill_viridis_d(alpha = .75) +
         
         theme_minimal() +
         
         theme(legend.position = "none",
               panel.grid.major.y = element_blank(),
               axis.text.y = element_blank(),
               axis.title = element_text(size = 10))
       
     ) %>%
       
       layout(xaxis = list(autorange = T),
              yaxis = list(autorange = T))
     
   })
   
   
   output$pie_chart <- renderPlotly({
     
     table <- as.data.frame(x = table(as.factor(x = DF2()$cluster)))
     
     colnames(table) <- c("Cluster",
                          "Freq")
     
     plot_ly(data = table,
             labels = ~ Cluster,
             values = ~ Freq,
             type = "pie",
             textposition = "inside",
             textinfo = "label+percent",
             marker = list(colors = viridis(n = input$k,
                                            alpha = .75),
                           line = list(color = '#FFFFFF',
                                       width = 5)),
             
             showlegend = F)
     
   })
   
   output$creditlimit_plot <- renderPlotly({
     
     DF <- data.frame(Cluster = as.factor(DF2()$cluster),
                      Climit = DF2()$CREDIT_LIMIT)
     
     ggplotly(
       
       ggplot(data = DF,
              mapping = aes(x = Cluster,
                            y = Climit,
                            fill = Cluster)) +
         
         geom_boxplot() +
         
         scale_fill_viridis_d(alpha = .75) +
         
         theme_minimal() +
         
         theme(axis.title = element_blank(),
               axis.text.x = element_text(angle = 90),
               panel.grid.major.x = element_blank(),
               legend.position = "none")
       
     )
     
   })
   
   # output$purch_freq_plot <- renderPlotly({
   #   
   #   DF <- data.frame(Cluster = as.factor(DF2()$cluster),
   #                    pfreq = credit_df$PURCHASES_FREQUENCY)
   #   
   #   ggplotly(
   #     
   #     ggplot(data = DF,
   #            mapping = aes(x = Cluster,
   #                          y = pfreq,
   #                          fill = Cluster)) +
   #       
   #       geom_boxplot() +
   #       
   #       scale_fill_viridis_d(alpha = .75) +
   #       
   #       theme_minimal() +
   #       
   #       theme(axis.title = element_blank(),
   #             axis.text.x = element_text(angle = 90),
   #             panel.grid.major.x = element_blank(),
   #             legend.position = "none")
   #     
   #   )
   #   
   # })
   
   output$cred_util_plot <- renderPlotly({
     
     DF <- data.frame(Cluster = as.factor(DF2()$cluster),
                      pfreq = credit_df$CREDIT_UTILIZATION)
     
     ggplotly(
       
       ggplot(data = DF,
              mapping = aes(x = Cluster,
                            y = pfreq,
                            fill = Cluster)) +
         
         geom_boxplot() +
         
         scale_fill_viridis_d(alpha = .75) +
         
         theme_minimal() +
         
         theme(axis.title = element_blank(),
               axis.text.x = element_text(angle = 90),
               panel.grid.major.x = element_blank(),
               legend.position = "none")
       
     )
     
   })
   
   output$analysis_plot <- renderPlotly({
     attribute <- input$attribute
     temp_df <- credit_df[attribute]
     temp_df$cluster <- as.factor(DF2()$cluster)
     colnames(temp_df) <- c('Attribute','Cluster')
     
     ggplotly
     (
       ggplot(data = temp_df,
              aes(x=Attribute, 
                  fill=Cluster, colour=Cluster)) +
         geom_histogram(alpha=0.6, position = 'identity') +
         
         theme_minimal() +
         
         theme(axis.title = element_blank(),
               axis.text.x = element_text(angle = 90),
               panel.grid.major.x = element_blank())
       
     )
     
   })
   
   Y <- reactive({
     data <- subset(as.data.frame(mutate(as.data.frame(credit_df[,1:12]), cluster=as.factor(CLUSTERING()$cluster))), cluster %in% input$cluster_num)
   }
   )
   output$hist_plot <- renderPlot({ 
     plot_histogram(as.data.frame(Y()))
   })
  
   
   
   # Hierarchical clustering
   X_hier = reactive(hclust(dist(credit_df), method=input$linkage))

   DF3 <- reactive({
     X2 <- credit_df
     X2$cluster <- cutree(X_hier(),input$cut_dendrogram)
     as.data.frame(X2)
   })
   
   output$hierarchical_plot <- renderPlot({
     plot(X_hier())
     # rect.hclust(X_hier() , h = input$cut_dendrogram, border = 2:6)
     if(input$linkage=='ward'){
       rect.hclust(X_hier() , h = 600000, border = 2:6)
     }else if(input$linkage=='complete'){
       rect.hclust(X_hier() , h = 400000.3, border = 2:6)
     }
   })
   
   output$sil_hc_plot <- renderPlotly({
     
     ggplotly(
       
       ggplot2::autoplot(object = silhouette(DF3()$cluster, dist(credit_df))) +
         
         scale_fill_viridis_d(alpha = .75) +
         
         theme_minimal() +
         
         theme(legend.position = "none",
               panel.grid.major.y = element_blank(),
               axis.text.y = element_blank(),
               axis.title = element_text(size = 10))
       
     ) %>%
       
       layout(xaxis = list(autorange = T),
              yaxis = list(autorange = T))
     
   })
   
   output$pie_chart_hc <- renderPlotly({
     
     table <- as.data.frame(x = table(as.factor(x = DF3()$cluster)))
     
     colnames(table) <- c("Cluster",
                          "Freq")
     
     plot_ly(data = table,
             labels = ~ Cluster,
             values = ~ Freq,
             type = "pie",
             textposition = "inside",
             textinfo = "label+percent",
             marker = list(colors = viridis(n = input$k,
                                            alpha = .75),
                           line = list(color = '#FFFFFF',
                                       width = 5)),
             
             showlegend = F)
     
   })
   
   output$creditlimit_plot_hc <- renderPlotly({
     
     DF <- data.frame(Cluster = as.factor(DF3()$cluster),
                      pfreq = credit_df$CREDIT_LIMIT)
     
     ggplotly(
       
       ggplot(data = DF,
              mapping = aes(x = Cluster,
                            y = pfreq,
                            fill = Cluster)) +
         
         geom_boxplot() +
         
         scale_fill_viridis_d(alpha = .75) +
         
         theme_minimal() +
         
         theme(axis.title = element_blank(),
               axis.text.x = element_text(angle = 90),
               panel.grid.major.x = element_blank(),
               legend.position = "none")
       
     )
     
   })
   
   output$cred_util_plot_hc <- renderPlotly({
     
     DF <- data.frame(Cluster = as.factor(DF3()$cluster),
                      pfreq = credit_df$CREDIT_UTILIZATION)
     
     ggplotly(
       
       ggplot(data = DF,
              mapping = aes(x = Cluster,
                            y = pfreq,
                            fill = Cluster)) +
         
         geom_boxplot() +
         
         scale_fill_viridis_d(alpha = .75) +
         
         theme_minimal() +
         
         theme(axis.title = element_blank(),
               axis.text.x = element_text(angle = 90),
               panel.grid.major.x = element_blank(),
               legend.position = "none")
       
     )
   })
   
   output$analysis_plot_hc <- renderPlotly({
     attribute <- input$attribute_hc
     temp_df <- credit_df[attribute]
     temp_df$cluster <- as.factor(DF3()$cluster)
     colnames(temp_df) <- c('Attribute','Cluster')
     
     ggplotly
     (
       ggplot(data = temp_df,
              aes(x=Attribute, 
                  fill=Cluster, colour=Cluster)) +
         geom_histogram(alpha=0.6, position = 'identity') +
         
         
         theme_minimal() +
         
         theme(axis.title = element_blank(),
               axis.text.x = element_text(angle = 90),
               panel.grid.major.x = element_blank())
       
     )
     
   })
   
   Y2 <- reactive({
     data <- subset(as.data.frame(mutate(as.data.frame(credit_df[,1:12]), cluster=as.factor(cutree(X_hier(),input$cut_dendrogram)), cluster %in% input$cluster_num_hc)))
   }
   )
   output$hist_plot_hc <- renderPlot({ 
     plot_histogram(as.data.frame(Y2()))
   })
   
   # dunn_km_sc <- reactive({
   #   dunn(clusters = CLUSTERING()$cluster, Data = scale(credit_df))
   # })
   
   # output$CLUS.INDEX <- renderValueBox({
   #   
   #   valueBox(value = dunn_km_sc,
   #            subtitle = "Dunn index - The higher the better",
   #            icon = icon("fas fa-users"),
   #            color = "purple")
   # })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
