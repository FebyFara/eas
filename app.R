library(shiny)
library(vroom)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(readxl)
library(reshape2)
library(scales)
library(plotly)
library(rsconnect)
library(renv)
rsconnect::deployApp('C:/Users/LENOVO/OneDrive/Documents/SEMESTER 5/Eksplorasi dan Visualisasi Data/EAS_Feby Fara Wita_2043211088')
renv::init()

setwd("C:/Users/LENOVO/OneDrive/Documents/SEMESTER 5/Eksplorasi dan Visualisasi Data/EAS_Feby Fara Wita_2043211088")
nilaioutput=read_xlsx("Data Project.xlsx",sheet = "Output")
glimpse(nilaioutput)
nilaioutput$thn = format(nilaioutput$Tahun, '%Y')
nilaioutput <- subset(nilaioutput, select = -c(Tahun))

proporsitenagakerja=read_xlsx("Data Project.xlsx",sheet = "Proporsi Tenaga Kerja")
glimpse(proporsitenagakerja)
proporsitenagakerja$thn = format(proporsitenagakerja$Tahun, '%Y')
proporsitenagakerja <- subset(proporsitenagakerja, select = -c(Tahun))

nilaiinput=read_xlsx("Data Project.xlsx",sheet = "Input")
glimpse(nilaiinput)
nilaiinput$thn = format(nilaiinput$Tahun, '%Y')
nilaiinput <- subset(nilaiinput, select = -c(Tahun))

nilaitambah=read_xlsx("Data Project.xlsx",sheet = "Nilai Tambah")
glimpse(nilaitambah)
nilaitambah$thn = format(nilaitambah$Tahun, '%Y')
nilaitambah <- subset(nilaitambah, select = -c(Tahun))

jumlahindustri=read_xlsx("Data Project.xlsx",sheet = "Jumlah Industri")
glimpse(jumlahindustri)
jumlahindustri$thn = format(jumlahindustri$Tahun, '%Y')
jumlahindustri <- subset(jumlahindustri, select = -c(Tahun))

pajak=read_xlsx("Data Project.xlsx",sheet = "Pajak")
glimpse(pajak)
pajak$thn = format(pajak$Tahun, '%Y')
pajak <- subset(pajak, select = -c(Tahun))

rataratapengeluaran=read_xlsx("Data Project.xlsx",sheet = "Rata-Rata pengeluaran TK")
glimpse(rataratapengeluaran)
rataratapengeluaran$thn = format(rataratapengeluaran$Tahun, '%Y')
rataratapengeluaran <- subset(rataratapengeluaran, select = -c(Tahun))

eksporimpor=read_xlsx("Data Project.xlsx",sheet = "Nilai Ekspor & Impor")
glimpse(eksporimpor)
eksporimpor$thn = format(eksporimpor$Tahun, '%Y')
eksporimpor <- subset(eksporimpor, select = -c(Tahun))

imporbarang=read_xlsx("Data Project.xlsx",sheet = "Impor Barang ")
glimpse(imporbarang)
imporbarang$thn = format(imporbarang$Tahun, '%Y')
imporbarang <- subset(imporbarang, select = -c(Tahun))
jumlah <- rataratapengeluaran %>%
  group_by(thn) %>%
  top_n(4, `Jumlah Tenaga Kerja`)%>%
  select(-`Pengeluaran Tenaga Kerja`)
selainjumlah <- rataratapengeluaran[!(rataratapengeluaran$`Jumlah Tenaga Kerja` %in% jumlah$`Jumlah Tenaga Kerja`),]
selainjumlah <- selainjumlah %>%
  group_by(thn) %>%
  summarise(`Jumlah Tenaga Kerja` = sum(`Jumlah Tenaga Kerja`),
            `Jenis Industri` = "Lainnya")
jumsemua <- rbind(jumlah,selainjumlah)
rm(jumlah,selainjumlah)

pengeluaran <- rataratapengeluaran %>%
  group_by(thn) %>%
  top_n(4, `Pengeluaran Tenaga Kerja`)%>%
  select(-`Jumlah Tenaga Kerja`)
selainpengeluar <- rataratapengeluaran[!(rataratapengeluaran$`Pengeluaran Tenaga Kerja` %in% pengeluaran$`Pengeluaran Tenaga Kerja`),]
selainpengeluar <- selainpengeluar %>%
  group_by(thn) %>%
  summarise(`Pengeluaran Tenaga Kerja` = sum(`Pengeluaran Tenaga Kerja`),
            `Jenis Industri` = "Lainnya")
pengelsemua <- rbind(pengeluaran,selainpengeluar)
rm(pengeluaran,selainpengeluar)

jumpeng <- rbind(jumsemua,pengelsemua)
jumpeng <- melt(jumpeng[, c("Jenis Industri","thn", "Jumlah Tenaga Kerja", "Pengeluaran Tenaga Kerja")])
jumpeng <- na.omit(jumpeng)

ggplot(jumsemua, aes(thn, y = `Jumlah Tenaga Kerja`, fill = `Jenis Industri`)) +
  geom_bar(stat = "identity",width = 0.5)+
  guides(fill=guide_legend(title="Jenis Industri"))+
  theme_minimal()+
  scale_y_continuous(labels = function(x) format(x, scientific = F))

jumlahindustri <- jumlahindustri%>% 
  mutate(
    thn = as.integer(thn)
  )
unique(jumlahindustri$thn)
listnya <- lapply(list(unique(jumlahindustri$thn)),sort)
listnya[[1]]

library(leaflet)
indon <- sf::st_read("indonesia.geojson")
look <- read.csv("lookupprov.csv",sep=";")
indon$state <- look$pingine[match(indon$state, look$asale)]
indon <- replicate(13, indon, simplify = FALSE) %>%
  bind_rows(.id = 'thn')
glimpse(indon)
indon <- indon %>% 
  mutate(
    Year = as.integer(thn)+2009
  )
indon <- indon[,c(6:8)]
indonesia <- merge(x=indon,y=nilaitambah,by.x=c("Year","state"),
                   by.y=c("thn","Provinsi"),all=TRUE)
eksporimpor=read_xlsx("Data Project.xlsx",sheet = "Nilai Ekspor & Impor")
## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "INDUSTRI BESAR SEDANG DI INDONESIA",  titleWidth = 650),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Informasi", tabName = "awal", icon = icon("database")),
                menuItem("Visualisasi Data", tabName = "Visual", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      #tab item informasi
      tabItem(
        tabName = "awal",
        fluidRow(
          column(width = 8, tags$br(), tags$img(src="industri.png", width =500 , height = 300),
                 a("Photo by Canva"), align = "center"),
          column(width = 12, tags$br() ,
                 tags$p("Perusahaan Industri Pengolahan dibagi dalam 4 golongan yaitu :"),
                 tags$p("a.Industri Besar (banyaknya tenaga kerja 100 orang atau lebih)"),
                 tags$p("b.Industri Sedang (banyaknya tenaga kerja 20-99 orang)"),
                 tags$p("c.Industri Kecil (banyaknya tenaga kerja 5-19 orang;Industri Rumah Tangga (banyaknya tenaga kerja 1-4 orang"),
                 tags$p("Penggolongan perusahaan industri pengolahan ini semata-mata hanya didasarkan kepada banyaknya tenaga kerja yang bekerja, tanpa memperhatikan apakah perusahaan itu menggunakan mesin tenaga atau tidak, serta tanpa memperhatikan besarnya modal perusahaan itu.")
          )
        )
      ),
      # tab item visualisasi data
      tabItem(  
        tabName = "Visual",
        fluidRow(
        box(selectInput("jumpel","Jumlah atau Pengeluaran Tenaga Kerja",unique(jumpeng$variable),
                        selected = unique(jumpeng$variable)[1])),
        box(selectInput("thnpie","Pilih Tahun:",listnya[[1]],
                        selected = listnya[[1]][1]))
      ),
      fluidRow(
        box(
          title = "Jumlah dan Pengeluaran Tenaga Kerja tiap Tahun"
          ,status = "primary"
          ,solidHeader = TRUE
          ,plotOutput("stack")
        ),
        box(title = "Presentase Jumlah Industri Indonesia"
            ,status = "primary"
            ,solidHeader = TRUE
            ,plotOutput("bulat"))
      ),
      fluidRow(
        box(selectInput("thnprov","Pilih Tahun:",unique(indonesia$Year),
                        selected = unique(indonesia$Year)[1])),
        box(selectInput("kegiatan","Jenis Kegiatan Perdagangan",unique(imporbarang$`Kegiatan Perdagangan`),
                        selected = unique(imporbarang$`Kegiatan Perdagangan`)[1]))
      ),
      fluidRow(
        box(title = "Proporsi Nilai Tambah Sektor Industri Manufaktur Terhadap PDB"
            ,status = "primary"
            ,solidHeader = TRUE
            ,leafletOutput("petaindo")),
        box(title = "Kegiatan Impor Barang Indonesia "
            ,status = "primary"
            ,solidHeader = TRUE
            ,plotOutput("anual"))
      ),
      fluidRow(
        box(title = "Nilai Ekspor dan Impor",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("plot"))
      )
      )

    )

      
  )
)

server <- function(input, output) {
  output$stack <- renderPlot({
    jomlah <- subset(jumpeng, jumpeng$variable==input$jumpel)
    ggplot(jomlah, aes(thn, y = value, fill = `Jenis Industri`)) +
      geom_bar(stat = "identity",width = 0.5)+
      guides(fill=guide_legend(title="Jenis Industri"))+
      theme_minimal()+
      scale_y_continuous(labels = function(x) format(x, scientific = F))
  })
  blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank()
    )
  
  output$bulat <- renderPlot({
    newData <- subset(jumlahindustri, jumlahindustri$thn==input$thnpie)
    
    ggplot(newData, aes(x="", y=`Jumlah Industri`, fill=Daerah)) +
      geom_bar(stat="identity", width=1) +
      geom_col() +
      geom_text(aes(label = percent(round(`Jumlah Industri`/sum(`Jumlah Industri`),digits = 3))),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y")+
      blank_theme +
      theme(axis.text.x=element_blank())
  })
  indo <- reactive({
    data <- subset(indonesia, indonesia$Year==input$thnprov)
    return(data)
  })
  # Create a continuous palette function
  
  output$petaindo <- renderLeaflet({
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = indo()$`Proporsi Nilai Tambah Sektor Industri Manufaktur Terhadap PDB`)
    labels <- sprintf(
      "%s dengan Proporsi Nilai Tambah Terhadap PDB sebesar %s",
      indo()$state, indo()$`Proporsi Nilai Tambah Sektor Industri Manufaktur Terhadap PDB`
    )
    
    leaflet(data=indo()) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% addPolygons(
          fillColor = ~pal(`Proporsi Nilai Tambah Sektor Industri Manufaktur Terhadap PDB`),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))
  })
  output$anual <- renderPlot({
    imporan <- subset(imporbarang, imporbarang$`Kegiatan Perdagangan`==input$kegiatan)
    ggplot(data=imporan, aes(x=thn, y=`Nilai CIF (Juta) US$`, group=`Jenis Barang`)) +
      geom_line(aes(color=`Jenis Barang`))+
      geom_point(aes(color=`Jenis Barang`))+
      theme_minimal()
  })
  output$plot <- renderPlotly({
    data1 <- eksporimpor
    data1 %>% count( jenisperdagangan  , wt =  nilai, sort = TRUE)
    data1 %>% count( jenisperdagangan  , wt =  nilai, sort = TRUE)
    summary <- data1 %>% 
      count(Tahun,  jenisperdagangan , wt = nilai)
    summary
    grafik1 = summary %>% 
      ggplot(aes( Tahun, n, colour = jenisperdagangan)) + 
      geom_line() + 
      labs(y = "Nilai Ekspor&Impor")
  })
}

shinyApp(ui, server)