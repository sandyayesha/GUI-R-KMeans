#K-Means Clustering Tahanan Indonesia

library(readxl)
library(cluster)
library(factoextra)
library(tidyverse)
library(gridExtra)
library(shiny)
library(shinythemes)


#membuat UI
tampilan_Kmeans<-fluidPage(theme = shinytheme("united"),
                          titlePanel(tags$b("K-Means Clustering")),
                          h4("oleh Muhammad Ayesha Arif Sandy (24050120120002)"),
                          navbarPage("K-Means Clustering",
                                     tabPanel("Input Data",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  fileInput("inputan","Masukkan data (Excel)",accept = ".xlsx"),
                                                ),
                                                mainPanel(
                                                  tabsetPanel(type = "pills",
                                                              tabPanel("Tampilan Awal Data",tableOutput("tabel_inputan")),
                                                              tabPanel("Statistik Deskriptif",verbatimTextOutput("statdeskriptif")))
                                                ))),
                                     tabPanel("Estimasi Jumlah Cluster Optimal",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  selectInput("metode","Pilih metode:",choices = c("Average silhouette width","Total within sum of square","Gap statistics"))
                                                ),
                                                mainPanel(plotOutput("plot_estimasi_cluster"))
                                              )),
                                     tabPanel("Model K-Means",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  textInput("jumlah_cluster","Jumlah Cluster:"),
                                                  actionButton("pemodelan","Pemodelan",class="btn-success")
                                                ),
                                                mainPanel(
                                                  tabsetPanel(type = "pills",
                                                              tabPanel("Model K-Means Clustering",verbatimTextOutput("model")),
                                                              tabPanel("Tampilan Awal Data",tableOutput("tabel_model")),
                                                              tabPanel("Plot Cluster",plotOutput("plot_cluster")))
                                                ))),
                                     tabPanel("Perbandingan Plot K-Means",
                                              sidebarLayout(
                                                sidebarPanel(
                                                  textInput("jumlah_cluster1","Jumlah Cluster 1:"),
                                                  textInput("jumlah_cluster2","Jumlah Cluster 2:"),
                                                  textInput("jumlah_cluster3","Jumlah Cluster 3:"),
                                                  textInput("jumlah_cluster4","Jumlah Cluster 4:"),
                                                  actionButton("pembandingan","Bandingkan",class="btn-success")
                                                ),
                                                mainPanel(
                                                  tabsetPanel(type = "pills",
                                                              tabPanel("Pembandingan Plot Cluster",plotOutput("plot_cluster_pembandingan")))
                                                )))
                                    
                          )
)

#membuat server
program_Kmeans<-function(input,output){
  #Output Tampilan Awal
  output$tabel_inputan<-renderTable({
    datainput=input$inputan
    if (is.null(datainput)){return()}
    data=read_excel(datainput$datapath)
  })
  output$statdeskriptif<-renderPrint({
    datainput=input$inputan
    if (is.null(datainput)){return()}
    data=read_excel(datainput$datapath)
    data_napi=data[,c(-1,-2)]
    summary(data_napi)
  })
  
  #Output Pemilihan Cluster Optimal
  output$plot_estimasi_cluster<-renderPlot({
    datainput=input$inputan
    if (is.null(datainput)){return()}
    data=read_excel(datainput$datapath)
    data_napi=data[,c(-1,-2)]
    if (input$metode=="Average silhouette width"){
      plot(fviz_nbclust(data_napi,kmeans,method="silhouette"))
     }
    if (input$metode=="Total within sum of square"){
      plot(fviz_nbclust(data_napi,kmeans,method="wss"))
    }
    if (input$metode=="Gap statistics"){
      plot(fviz_nbclust(data_napi,kmeans,method="gap_stat"))
    }
    })
  
  #Output Pemodelan
  observeEvent(input$pemodelan,{
    datainput=input$inputan
    if (is.null(datainput)){return()}
    data=read_excel(datainput$datapath)
    data_napi=data[,c(-1,-2)]
    jumlah_cluster=as.numeric(input$jumlah_cluster)
    final=kmeans(data_napi,jumlah_cluster,nstart=25)
    tabel_final=data.frame(data,final$cluster)
    tabel_urut=tabel_final[order(tabel_final$final.cluster),]
    tabel_urut=data.frame(tabel_urut)
    
    output$model<-renderPrint({
      final
    })
    
    output$tabel_model<-renderTable({
      tabel_urut
    })
    
    output$plot_cluster<-renderPlot({
      plot(fviz_cluster(final,data=data_napi))
    })
    
  })
  
  #Pembandingan Plot Model Cluster
  observeEvent(input$pembandingan,{
    datainput=input$inputan
    if (is.null(datainput)){return()}
    data=read_excel(datainput$datapath)
    data_napi=data[,c(-1,-2)]
    jumlah_cluster1=as.numeric(input$jumlah_cluster1)
    jumlah_cluster2=as.numeric(input$jumlah_cluster2)
    jumlah_cluster3=as.numeric(input$jumlah_cluster3)
    jumlah_cluster4=as.numeric(input$jumlah_cluster4)
    k2=kmeans(data_napi,centers=jumlah_cluster1,nstart=25)
    k3=kmeans(data_napi,centers=jumlah_cluster2,nstart=25)
    k4=kmeans(data_napi,centers=jumlah_cluster3,nstart=25)
    k5=kmeans(data_napi,centers=jumlah_cluster4,nstart=25)
    
    p1=fviz_cluster(k2,geom="point",data=data_napi)+ggtitle("k=2")
    p2=fviz_cluster(k3,geom="point",data=data_napi)+ggtitle("k=3")
    p3=fviz_cluster(k4,geom="point",data=data_napi)+ggtitle("k=4")
    p4=fviz_cluster(k5,geom="point",data=data_napi)+ggtitle("k=5")
    p=grid.arrange(p1,p2,p3,p4,nrow=2)
    
    output$plot_cluster_pembandingan<-renderPlot({
      plot(p)
    })
    
  })
  
  
}

#Running App
shinyApp(ui=tampilan_Kmeans,server=program_Kmeans)