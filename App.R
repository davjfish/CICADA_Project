
#Load required packages 
############
library(shiny)
library(tidyverse)
library(data.table)
library(shinythemes)
require(shinycssloaders)
library(DT)
require(leaflet)
require(sf)
require(readxl)
require(htmltools) #for pop up labels
require(shinyjs) #for enable / disable of buttons
require(viridisLite) #colors for GHM raster
require(stars) #for rasters
require(leafem) #for plotting rasters from stars package
############

ui <- navbarPage(

#UI global settings 
############
  useShinyjs(),
  theme = shinytheme("simplex"),
  title = "CICADA (CumulatIve effeCts spAtial DAta tool) version 1.0",
############

#About tab
############
  tabPanel(
    "About",
    p(
      "This tool compiles, summarizes and
      displays spatial information on fish and fish habitat in freshwaters across Canada, in support of DFO decision-making."
    ), br(),
    p("For guidance on using this tool, please consult the user manual available here."),
    br(),
    p(
      "Development and maintenance of this tool is led by the Chu Lab in DFO's Ecosystems and Oceans Science Sector (Ontario and Prairie Region), in partnership with
      other DFO Science groups, external researchers and the Fish and Fish Habitat Protection Program. Funding to support this tool is 
      provided by DFO's Competitive Science Research Fund. For inquiries related to this project please contact Cindy Chu (cindy.chu@dfo-mpo.gc.ca). 
      For questions or issues related to this web app please contact Cody Dey (cody.dey@dfo-mpo.gc.ca)"
    ),
    br(),
    p(
      "This tool relies on data shared by many government and 
      non-government organizations. Full details on data sourced 
      for this project, and on using the tool",
      tags$a(" are provided in the user manual", href="user_manual.pdf", target = "_blank")
    ), br(),
    
    p("DISCLAIMER: This software package is provided as is without any warranty of any kind. The developers of this software cannot guarantee the completeness, accuracy, or reliability of the data presented within the application. Data may be missing or incomplete, and users should exercise professional judgement when using this software. 
    Furthermore, the entire risk related to the use or performance of the software lies with the user. In no event shall the developers be liable for any damage whatsoever arising from the use of or inability to use this software, even if the developers have been advised of the possibility of such damages. 
    By using this software, you acknowledge that you have read and understood this disclaimer and agree to use the software at your own risk.")
    
  ),
############ 

#Interactive map tab
############
  tabPanel(
    "Interactive map",

    # page header
    p("View spatial data on fish and fish habitat for a site in Canada and the watershed in which it sits"),
    p(
      "Enter the site of interest below in decimal degrees",

      a(href = "http://rcn.montana.edu/Resources/Converter.aspx", " (convert from other units)", target = "_blank"),
      "or click on the map"
    ),

    # first row of Map tab, is a header with the input boxes and buttons
    sidebarLayout(
      sidebarPanel(
        width = 3,
        wellPanel(
          "Step 1 - Enter a location",
          textInput(inputId = "focal_y", label = "Latitude (°N)"),
          textInput(inputId = "focal_x", label = "Longitude (°E)"),
          actionButton(inputId = "check_watershed", label = "List available watersheds")
        ),
        wellPanel(
          "Step 2 - Select a watershed unit",
          selectInput(
            inputId = "watershed_scale",
            label = NULL,
            choices = c("[complete step 1 above]")
          ),
          actionButton(inputId = "go", label = "Generate map")
        )
      ),
      
      mainPanel(
        width = 9,

        # main section of Interactive map tab (i.e. the map)
        shinycssloaders::withSpinner(leafletOutput("map", height = "80vh")),

        ) # end of main panel
    ) # end of sideBarLayout
  ),
############ 

#Watershed summary tab
###########
tabPanel(
  "Watershed summary",
  textOutput("sum"),
  br(),
  textOutput("lulc"),
  br(),
  textOutput("zones"),
  br(),# br(),
  #
  textOutput("common_species"),
  br()
),
##########

#Fish species list tab
#######
  tabPanel(
    "Fish species list",
    p(
      "List of species present in the watershed, based on catch data and expert opinion"
    ),
    fluidRow(column(2, ""), column(8, DTOutput("table1")), column(2, "")),
    downloadButton("downloadTable1", "Download species list"),
  ),
#######

#Fish observations tab
#########
  tabPanel(
    "Fish observations",
    p(
      "Most recent capture of each species at each sampling site within the selected watershed"
    ),
    fluidRow(column(12, DTOutput("table2"))),
    br(),
    downloadButton("downloadTable2", "Download fish sampling data")
  ),
###########

#Water quality tab
###########
  tabPanel(
    "Water quality",
    p("Water temperature (°C), pH, chloride (mg/L), dissolved oxygen (mg/L), turbidity (NTU), conductivity (uS/cm), nitrates (mg/L), total phosphorus (ug/L), total selenium (ug/L), dissolved selenium (ug/L) and total dissolved solids (mg/L) data are median values from each site across samples collected between 2015 and 2021"), 
    fluidRow(column(12, DTOutput("table3"))),
    downloadButton("downloadTable3", "Download water quality data")
  ),
#######

#Aquatic barriers tab
##########
  tabPanel(
    "Aquatic barriers",
    p(
      "Dams, waterfalls and fishways occurring in the watershed"
    ),
    fluidRow(column(12, DTOutput("table4"))),
    downloadButton("downloadTable4", "Download barrier data")
  ),
############

#Industrial activities tab
########
tabPanel(
  "Industrial activities",
  p("Mining, Metalworks, Oil & Gas, Wastewater, National Pollution Release Inventory, Federal Contaminated Sites and Planned Major Projects"),
  fluidRow(column(12, DTOutput("table_ind"))),
  downloadButton("downloadTableInd", "Download industrial data")
  
),
###########

#Data and Methods tab
#############

tabPanel(
  "Data and Methods",
  
  p(strong("Watershed units"),
    br(),
    em("Map Layer"),
    br(),br(),
    "Four sets of watershed units are available:",br(),br(),
    "National Hydrological Network (NHN) Tertiary Watersheds. National Coverage, Produced by Natural Resources Canada", br(),
    "Ontario (ON) Quaternary Watersheds. Provincial Coverage, Produced by Ontario Ministry of Natural Resources and Forestry",br(),
    "British Columbia (BC) Freshwater Atlas watersheds. Provincial Coverage, Produced by BC Ministry of Agriculture and Lands",br(),
    "Alberta (AB) Hydrological Unit Code Level 8 watersheds. Provincial Coverage, Produced by Alberta Environment and Parks"
  ),
  
  br(),
  p(strong("Fish Species List"),
    br(),
    em("Data Tab"),
    br(),br(),
    "List of species present in the watershed, based on catch data (e.g. fish observations data described above) and expert opinion."
  ),
  
  br(),
  p(strong("Fish observations"),
    br(),
    em("Map Layer and Data Tab"),
    br(),br(),
    "Most recent observation of fish species at each sampling site. Based on a compilation of national, provincial and regional datasets conducted by the CICADA project team."
  ),
  
  br(),
  p(strong("Water quality"),
    br(),
    em("Map Layer and Data Tab"),
    br(),br(),
    "Map Layer - Water Quality Index (score from 0 - 100 with corresponding categorization as Excellent, Good, Fair, Marginal or Poor) based on water samples collected between 2000 and 2022.
    Calculation of water quality score follows the method used by the Canadian Council of Miniters of the Environment's Water Quality Index Manual 2017, and is based on the number, frequency and magnitude of deviation between observed water quality measurements and guideline values.
    Water quality parameters and associated guidelines considered for this analysis were dissolved oxygen (6.5 mg/L), pH (6.5 - 9), nitrates (2.93 N/L), total phosphorus (0.03 mg/L), total selenium ( 2 ug/L), chloride (120 mg/L) and turbidity (10 NTU).",
    br(), br(),
    "Data Tab - All water quality samples for each sampling location within the watershed, including the 7 parameters described above as well measurements of conductivity, total dissolved solids, dissolved selenium and water temperature.
     Data were compiled from national, provincial and regional datasets conducted by the CICADA project team.",
    ),
  
  br(),
  p(strong("Aquatic barriers"),
    br(),
    em("Map Layer and Data Tab"),
    br(),br(),
    "Dams, waterfall and fishways compiled in the Canadian Aquatic Barrier Database by the Canadian Wildlife Federation."
  ),
  
  br(),
  p(strong("Industrial activities"),
    br(),
    em("Map Layer and Data Tab"),
    br(),br(),
    "Site data associated with mining, oil & gas, metalworks, wastewater activities in Canada. Also, sites found in the Federal Contaminated Sites inventory and the National Pollutant Release Inventory, and
    in Natural Resources Canada's Major Projects Inventory."
  ),
  
  br(),
  p(strong("Landscape Modification"),
    br(),
    em("Map Layer"),
    br(),br(),
    "Visualizes the degree of human modification on a scale of 0 (low modification) to 1 (high modification). 
  These values are based on a variety of landscape stressors including urban and built-up areas, crop and pasture lands, livestock grazing, oil and gas production, mining and quarrying, power generation (renewable and nonrenewable), roads, railways, power lines and towers, logging and wood harvesting, human intrusion, reservoirs, and air pollution.
  These data were assembled and analyzed by Theobald et. al. (2020) in", 
    em("Earth transformed: detailed mapping of global human modification from 1990 to 2017."), "Earth Systems Science Data 12, p 1953–1972.",
    "Resolution is 0.09 km2. The dataset has been clipped by watershed unit for web-based display."
  ),
  
  br(),
  p(strong("Critical Habitat"),
    br(),
    em("Map Layer"),
    br(),br(),
    "Designated critical habitat for fish and mussels listed on Schedule 1 of the Species At Risk Act. Spatial data from DFO's publically available Species at Risk Critical Habitat and Distribution Map.
  Data for freshwater and diadromous species shown, while marine species are excluded."
  ),
  
  br(),
  p(strong("Protected Areas"),
    br(),
    em("Map Layer"),
    br(),br(),
    "Areas listed in the Canadian Protected and Conserved Areas Database, including and terrestrial protected areas and other effective area-based conservation measures (OECM) in Canada."
  ),
  
  br(),
  p(strong("Land use / Land cover"),
    br(),
    em("Map Layer"),
    br(),br(),
    " Land cover classifications at 30x30m resolution based on Sentinel-2 imagery. Data provided by ESRI and Microsoft. Land cover classes include 
    (i) water, (ii) trees, (iii) flooded vegetation, (iv) rangeland, (v) crops, (vi) bare ground, (vii) built areas, (viii) snow/ice and (ix) clouds.
    For some larger watersheds in northern Canada, resolution was further reduced to 100x100m to facilitate mapping",
    br(), br(),

    h3("Map symbology"),
    DT::dataTableOutput('symbology_table')

  )
)
###############

) # end of UI


server <- function(input, output, session) {

  #Server globals
  ############
  
  sf_use_s2(FALSE)
  
   dat <- tribble(
  ~layer, ~feature, ~symbol,
  "fish presence", "sampling site", "green fish",
  "water quality", "sampling site", "blue drop",
  "aquatic barriers", "dams", "red square",
  "aquatic barriers", "waterfalls", "blue square",
  "aquatic barriers", "fishways", "green square",
  "industrial activities", "mine sites", "grey circle",
  "industrial activities", "oil & gas sites", "black circle",
  "industrial activities", "metalwork sites", "white circle",
  "industrial activities", "national pollutant release inventory sites", "orange circle",
  "industrial activities", "wastewater sites", "cyan circle",
  "industrial activities", "federal contaminated sites", "brown circle",
  "industrial activities", "planned major projects", "purple circles and lines",
  "critical habitat", "critical habitat", "red polygon",
  "protected areas", "protected and conserved areas", "green polygon",
  "landscape modification", "landscape modification raster", "see legend on map",
  "land use / land cover", "land cover classifications", "see legend on map"
  )
   
   output$symbology_table <- renderDataTable(
     { dat },
     rownames = FALSE,
     options = list(
       pageLength = 15,
       dom = 't')
   )
  
  ############
  
  #Read in csv data layers
  ###########
 
  major_projects = fread("Major projects inventory/major_projects_line_wshd_joins.csv") 
   
  crossings = fread("Road crossings/NHNTWS_NRF_Sum.csv")
   
  pa_lookup = fread("Protected_areas/CPCAD_WSHD_join.csv")
  
  NHN_SARA = fread("Critical_habitat/DFO_SARA_CritHab_22/WSHD_Joins/SARA_NHN_Join.csv")
  PRV_SARA = fread("Critical_habitat/DFO_SARA_CritHab_22/WSHD_Joins/SARA_PRV_Join.csv")
  
  NHN_splist <- fread("Fish data/Watershed_species_lists_v2.csv")
  
  salmonCUs <-fread("Fish data/Salmon Conservation Units/Salmon_CU_wshd_Join.csv")
  
  wastewater = fread("Canada Wastewater sites/wastewater_merged.csv", encoding = 'Latin-1')
  
  contaminated_sites <- fread("Federal Contaminated Sites/fed_cont_sites_inv.csv", encoding = 'Latin-1')
  contaminated_sites = contaminated_sites %>% mutate(CurrentSit = ifelse(CurrentSit == "A", "Active",
                                                                         ifelse(CurrentSit == "C", "Closed", "Suspected")))
  oilgas <- fread("Mining Oil and Gas/900A_72nd_OilAndGas.csv", encoding = 'Latin-1')
  
  metalworks <- fread("Mining Oil and Gas/900A_72nd_MetalWorks.csv", encoding = 'Latin-1')
  
  mines <- fread("Mining Oil and Gas/900A_72nd_ProducingMines.csv", encoding = 'Latin-1')
  
  fish_data <- fread("Fish data/Nat_Fish_Recent_01-12-2023.csv")
  
  wq_sites<-fread("Water quality data/WQI_data.csv")
  wq_data<-fread("Water quality data/WQ_data_for_tab.csv")
  
  dams <- fread("Can_Aquatic_Barrier_database_WSHD/cabddams__WSHD.csv")
  
  wfs <- fread("Can_Aquatic_Barrier_database_WSHD/cabd_waterfalls_wshd.csv")
  
  fishways <- fread("Can_Aquatic_Barrier_database_WSHD/cabdfishways_wshd.csv")
  
  npri <- fread("National Pollutant Release Inventory/NPRI_Sites_V2.csv", encoding = 'Latin-1')
  npri_release <-fread("National Pollutant Release Inventory/NPRI_substance_list_2022.csv", encoding = 'Latin-1')
  
  fwbgz = read_csv("FreshwaterBiogeographicZone/FW_BIO_ZONES_WSHD_Join.csv")
  
  feow = fread("CDN_FWecoregions/FW_ECO_REG_WSHD_Join.csv")
  feow_names = tribble(~Name, ~FEOW_ID,
                       "Upper Yukon", 102,
                       "Alaska & Canada Pacific Coastal", 103,
                       "Upper Mackenzie", 104,
                       "Lower Mackenzie", 105,
                       "Central Arctic Coastal", 106,
                       "Upper Saskatchewan", 107,
                       "Middle Saskatchewan", 108,
                       "English - Winnipeg Lakes", 109,
                       "Southern Hudson Bay", 110,
                       "Western Hudson Bay", 111,
                       "Canadian Arctic Archipelago", 112,
                       "Eastern Hudson Bay - Ungava", 113,
                       "Gulf of St. Lawrence Coastal Drainages", 114,
                       "Canadian Atlantic Islands", 115,
                       "Laurentian Great Lakes", 116,
                       "St. Lawrence", 117,
                       "Northeast US & Southeast Atlantic Canada Drainages", 118,
                       "Scotia - Fundy", 119,
                       "Columbia Glaciated", 120,
                       "Upper Missouri", 142)
  
  ##############
  
  #Disable buttons unless conditions are met
  ############
  # disable check watershed unless lat long are entered
  observe({
    if (is.null(input$focal_y) || input$focal_y == "" || is.null(input$focal_x) || input$focal_x == "") {
      shinyjs::disable("check_watershed")
    } else {
      shinyjs::enable("check_watershed")
    }
  })


  # disable go unless focal site is set and valid
  observe({
    if (input$watershed_scale == "[complete step 1 above]" || input$watershed_scale == "[no data available]") {
      shinyjs::disable("go")
    } else {
      shinyjs::enable("go")
    }
  })
  ############ 
  
  #Load initial map and watershed cover layer and canada cover layer
  ############
  # show base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      fitBounds(
        lat1 = 48.5,
        lng1 = -142,
        lat2 = 80,
        lng2 = -53
      )
  })
  
  wshd_cvr = read_sf("Watershed_Layers/CE_WSHDs_cvr_WGS84_v3.gpkg")
  possible_watersheds = c("AB Hydrological Unit Code 8", "BC Freshwater Atlas", "NHN Tertiary Watersheds", "ON Quaternary Watersheds")
  
  can_cvr = read_sf("Watershed_Layers/simple_canada.gpkg")


  ############ 

  #Clicking on map
  ############
  
  # Create site icon
  siteIcon <- makeAwesomeIcon(
    markerColor = "black",
    iconColor = "black"
  )
  
  # when the map gets clicked, fill in the site lat/lng and add a project marker
  observeEvent(input$map_click, {
    event <- input$map_click
    lat <- round(event$lat, 6)
    lng <- round(event$lng, 6)
    updateTextInput(session, "focal_y", value = lat)
    updateTextInput(session, "focal_x", value = lng)

    leafletProxy("map") %>%
      clearGroup(group = "project_site") %>%
      addAwesomeMarkers(
        group = "project_site", # project site
        lng = lng,
        lat = lat,
        label = "Project site",
        icon = siteIcon
      )
   
  #reset watershed_scale options
    updateSelectInput(session, "watershed_scale", choices = "[complete step 1 above]")
  })
  ############ 

  #Check watershed button 
  #########

  observeEvent(input$check_watershed, {
    showModal(modalDialog("Checking for available watershed layers",
      footer = NULL
    ))

    avail_watersheds <- c()

    site_x <- abs(as.numeric(input$focal_x)) * -1
    site_y <- as.numeric(input$focal_y)
    focal_site <- st_sfc(st_point(c(site_x, site_y)), crs = 4326)
    
    #check if site is within Canada
    
    if(st_contains(can_cvr, focal_site, sparse = FALSE)[,1]){

    # check if site is covered by each of the watershed layers, and produce a list of the layers that it falls in

    x = which(st_contains(wshd_cvr, focal_site, sparse = FALSE)[,1])

    if(length(x)>0){
      updateSelectInput(session, "watershed_scale", choices = possible_watersheds[x])
    } else {
      updateSelectInput(session, "watershed_scale", choices = c("[no data available]"))
    }
    } else {
      updateSelectInput(session, "watershed_scale", choices = c("[no data available]"))
    }
    
    removeModal()
  })
  #########
 
  #Generate map and data tables button
    observeEvent(input$go, {
    showModal(
      modalDialog(
        HTML("Preparing data for your focal site - this could take up to 2 minutes."),
      footer = NULL
    ))

    #Identify focal watershed
    ###############
    site_x <- abs(as.numeric(input$focal_x)) * -1
    site_y <- as.numeric(input$focal_y)
    focal_site <- st_sfc(st_point(c(site_x, site_y)), crs = 4326)
    

    if(input$watershed_scale == "NHN Tertiary Watersheds"){searchstring = "NHN-TWS"} else {
      if(input$watershed_scale == "BC Freshwater Atlas") {searchstring = "BC-FWA"} else{
        if(input$watershed_scale == "ON Quaternary Watersheds") {searchstring = "ON_QUAT"} else{
          if(input$watershed_scale == "AB Hydrological Unit Code 8") {searchstring = "AB-HUC8"}
        }
      }
    } 
    
    query = paste0("SELECT * FROM CE_WSHDs_WGS84_v2 WHERE Dataset = ", "'",searchstring,"'")
    watersheds <- read_sf("Watershed_Layers/CE_WSHDs_WGS84_v2.gpkg", query = query)
    focal_watershed <- watersheds[st_contains(watersheds, focal_site) %>% lengths() > 0, ]
    focal_watershed_ID = focal_watershed %>% dplyr::select(PRV_WSHD_I, NHN_TWS_ID) %>% st_set_geometry(NULL) %>% as_vector() 
    focal_watershed_ID = focal_watershed_ID[!is.na(focal_watershed_ID)]
    
    #focal NHN for fish species list
    if(searchstring != "NHN-TWS"){
      query = paste0("SELECT * FROM CE_WSHDs_WGS84_v2 WHERE Dataset = 'NHN-TWS'")
      watersheds <- read_sf("Watershed_Layers/CE_WSHDs_WGS84_v2.gpkg", query = query)
      focal_NHN <-  watersheds[st_contains(watersheds, focal_site) %>% lengths() > 0, ]
      focal_NHN_ID = st_drop_geometry(focal_NHN)$NHN_TWS_ID[1]
    }
    
    area = round(as.numeric(st_area(focal_watershed) / 1000000),0)
    
    sum_text = paste(
      "The watershed selected is the", input$watershed_scale, "watershed located at",
      site_y, "°N and", site_x*-1, "°W.", "It is", area, "square km in area.")
    
    ############ 
    
    #PREP DATA LAYERS AND POP-UPS
    
    #major projects inventory
    ##################
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      keeps <- major_projects %>% filter(NHN_TWS_ID == focal_watershed_ID)
      
      query = paste0("SELECT * FROM major_projects_inventory_point WHERE NHN_TWS_ID = ", "'",focal_watershed_ID,"'")
      mp_points_plot <- read_sf("Major projects inventory/major_projects_inventory_point.gpkg", query = query)
      
      } else {
      keeps <- major_projects %>% filter(PRV_WSHD_I == focal_watershed_ID)
      
      query = paste0("SELECT * FROM major_projects_inventory_point WHERE PRV_WSHD_I = ", "'",focal_watershed_ID,"'")
      mp_points_plot <- read_sf("Major projects inventory/major_projects_inventory_point.gpkg", query = query)
        }
    
    #process the point data
    mp_points_clip = mp_points_plot %>% st_drop_geometry()    
    mp_points_labels <- lapply(seq(nrow(mp_points_clip)), function(i) {
      paste0(
        "Planned Major Project", "<br>",
        "Name: ", mp_points_clip[i, "proj_name"], "<br>",
        "Type: ", mp_points_clip[i, "sector"], "<br>",
        "Status: ", mp_points_clip[i, "status"]
      )
    })
    
    #load and process the line data
    ids = paste(formatC(unique(keeps$id), width = 4, flag="0"), collapse="', '")
    linequery = paste0("SELECT * FROM major_projects_inventory_line WHERE id IN ", "('", 
                  ids,
                   "')")
    mp_lines_plot <- read_sf("Major projects inventory/major_projects_inventory_line.gpkg", query = linequery)
    
    mp_lines_clip = mp_lines_plot %>% st_drop_geometry()    
    mp_lines_labels <- lapply(seq(nrow(mp_lines_clip)), function(i) {
      paste0(
        "Planned Major Project", "<br>",
        "Name: ", mp_lines_clip[i, "proj_name"], "<br>",
        "Type: ", mp_lines_clip[i, "type"], "<br>",
        "Status: ", mp_lines_clip[i, "status"]
      )
    })
    
    
    
    
    #################
    
    # Freshwater biogeographic zones
    ##############
    
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      keeps <- fwbgz %>% filter(NHN_TWS_ID == focal_watershed_ID)
    } else {
      keeps <- fwbgz %>% filter(PRV_WSHD_I == focal_watershed_ID)
    }
    
      focal_fwbgz = paste0(
        "and the ", keeps$En_Name, " COSEWIC Freshwater Biogeographic Zone.")
 
    ##########
    
    # Freshwater ecoregions
    ##############
    
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      keeps <- feow %>% filter(NHN_TWS_ID == focal_watershed_ID)
    } else {
      keeps <- feow %>% filter(PRV_WSHD_I == focal_watershed_ID)
    }
    
    feow_names_clip = feow_names %>% filter(FEOW_ID %in% keeps$FEOW_ID)

      focal_feow = paste0(
        "This watershed is part of the ", feow_names_clip$Name, " Freshwater Ecoregion "
      ) 
      
      zone_text = paste0(focal_feow, focal_fwbgz)
    
    ##########
    
    #Protected areas
    ###########
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      keeps <- pa_lookup %>% filter(NHN_TWS_ID == focal_watershed_ID)
    } else {
      keeps <- pa_lookup %>% filter(PRV_WSHD_I == focal_watershed_ID)
    } 
    
    query = paste0("SELECT * FROM CPCAD_Dec_2021_WGS84 WHERE CE_ID IN ", "('", 
                   paste(unique(keeps$CE_ID), collapse="', '"),
                   "')")
    pa_plot <- read_sf("Protected_areas/CPCAD_Dec_2021_WGS84.gpkg", query =  query)
    
    pa_clip <- st_drop_geometry(pa_plot)
    
    pa_labels <- lapply(seq(nrow(pa_clip)), function(i) {
      paste0(
        pa_clip[i, "NAME_E"], "<br>",
        "Type: ", pa_clip[i, "TYPE_E"], "<br>",
        "Owner: ", pa_clip[i, "OWNER_E"]
      )
    })
    ##############
    
    #NPRI 
    #########
    
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      npri_clip <- npri %>% filter(NHN_TWS_ID == focal_watershed_ID)
    } else {
      npri_clip <- npri %>% filter(PRV_WSHD_I == focal_watershed_ID)
    }
    npri_sites = npri_clip$NPRI_Site_ID
    npri_release_clip = npri_release %>% filter(NPRI_Site_ID %in% npri_sites) 
    npri_release_clip = npri_release_clip %>% group_by(NPRI_Site_ID)  %>%
      summarise(
      Substances = paste(unique(Substance_Name), collapse = ", "),
      Most_recent = max(Reporting_Year)
    )
    npri_clip = left_join(npri_clip, npri_release_clip, by = "NPRI_Site_ID")
    npri_plot <- st_as_sf(
      x = npri_clip,
      coords = c("Longitude", "Latitude"),
      crs = 4326
    )
    
    npri_labels <- lapply(seq(nrow(npri_clip)), function(i) {
      paste0(
        "National Pollutant Release Inventory", "<br>",
        "Company Name: ", npri_clip[i, "Company_Name"], "<br>",
        "Sector: ", npri_clip[i, "Key_Industrial_Sector"], "<br>",
        "Most recent release: ", npri_clip[i, "Most_recent"], "<br>",
        str_replace_all(str_wrap(paste0("Substances released: ", npri_clip[i, "Substances"]), width = 50), "\n", "<br>")
      
      )
    })
    ###########
    
    #Wastewater
    ########
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      wastewater_clip <- wastewater %>% filter(NHN_TWS_ID == focal_watershed_ID)
    } else {
      wastewater_clip <- wastewater %>% filter(PRV_WSHD_I == focal_watershed_ID)
    }
    
    wastewater_plot <- st_as_sf(
      x = wastewater_clip,
      coords = c("Longitude", "Latitude"),
      crs = 4326
    )
    
    wastewater_labels <- lapply(seq(nrow(wastewater_clip)), function(i) {
      paste0(
        "Wastewater source", "<br>",
        wastewater_clip[i, "System_Nam"], "<br>",
        "Average daily effluent (m^3): ", wastewater_clip[i, "Average_Da"], "<br>",
        "CBOD relative to limit (mg/L): ", wastewater_clip[i, "CBOD_Compliance"], "<br>",
        "Suspended solids relative to limit (mg/L): ", wastewater_clip[i, "Suspended_Solids_Compliance"]
      )
    })
    ##########
    
    #Federal Contaminated sites
    ##############
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      contaminated_sites_clip <- contaminated_sites %>% filter(NHN_TWS_ID == focal_watershed_ID)
    } else {
      contaminated_sites_clip <- contaminated_sites %>% filter(PRV_WSHD_I == focal_watershed_ID)
    }
    
    contaminated_sites_plot <- st_as_sf(
      x = contaminated_sites_clip,
      coords = c("Longitude", "Latitude"),
      crs = 4326
    )
    
    contaminated_sites_labels <- lapply(seq(nrow(contaminated_sites_clip)), function(i) {
      paste0(
        "Federal Contaminated Site", "<br>",
        contaminated_sites_clip[i, "FCSILink_E"],"<br>",
        "Status: ", contaminated_sites_clip[i, "CurrentSit"], "<br>",
        "Medium: ", contaminated_sites_clip[i, "Contamin_1"], "<br>",
        str_replace_all(str_wrap(paste0("Contaminant: ", contaminated_sites_clip[i, "Contaminan"]), width = 50), "\n", "<br>")
      )
    })
    ############
    
    #OIL AND GAS
    ###########
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      oilgas_clip <- oilgas %>% filter(NHN_TWS_ID == focal_watershed_ID)
    } else {
      oilgas_clip <- oilgas %>% filter(PRV_WSHD_I == focal_watershed_ID)
    }
    
    oilgas_plot <- st_as_sf(
      x = oilgas_clip,
      coords = c("Longitude", "Latitude"),
      crs = 4326
    )
    
    oilgas_labels <- lapply(seq(nrow(oilgas_clip)), function(i) {
      paste0(
        "Oil or Gas Site", "<br>",
        "Name: ", oilgas_clip[i, "MINESITE_E"], "<br>",
        "Product: ", oilgas_clip[i, "PRODUCT_E"], "<br>"
      )
    })
    ##############
    
    # METALWORKS
    #############
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      metalworks_clip <- metalworks %>% filter(NHN_TWS_ID == focal_watershed_ID)
    } else {
      metalworks_clip <- metalworks %>% filter(PRV_WSHD_I == focal_watershed_ID)
    }
    
    metalworks_plot <- st_as_sf(
      x = metalworks_clip,
      coords = c("Longitude", "Latitude"),
      crs = 4326
    )
    
    metalwork_labels <- lapply(seq(nrow(metalworks_clip)), function(i) {
      paste0(
        metalworks_clip[i, "OPERATIO_E"], " Metalwork", "<br>",
        "Operator: ", metalworks_clip[i, "OWNER_E"], "<br>",
        "Commodity: ", metalworks_clip[i, "COM_DESC_E"],"<br>"
      )
    })
    ##############
    
    # MINES
    ##########
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      mines_clip <- mines %>% filter(NHN_TWS_ID == focal_watershed_ID)
    } else {
      mines_clip <- mines %>% filter(PRV_WSHD_I == focal_watershed_ID)
    }
    
    mines_plot <- st_as_sf(
      x = mines_clip,
      coords = c("Longitude", "Latitude"),
      crs = 4326
    )
    
    mine_labels <- lapply(seq(nrow(mines_clip)), function(i) {
      paste0(
        mines_clip[i, "OPERATIO_E"], " Mine", "<br>",
        "Operator: ", mines_clip[i, "OWNER_E"], "<br>",
        "Commodity: ", mines_clip[i, "COM_DESC_E"],"<br>"
      )
    })
    ##########
    
    # Fish observations data
    ############
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      fish_data_clip <- fish_data %>% filter(NHN4DWSH == focal_watershed_ID)
    } else {
      fish_data_clip <- fish_data %>% filter(PRPWSHID == focal_watershed_ID)
    }

    fish_data_sum <- fish_data_clip %>%
      group_by(CE_Site_ID) %>%
      mutate(fish_present = paste0(Common_name, collapse = "<br>")) %>%
      slice(1)

    fish_plot <- st_as_sf(
      x = fish_data_sum,
      coords = c("Longitude", "Latitude"),
      crs = 4269
    )

    fish_plot <- st_transform(fish_plot, crs = 4326)

    fish_labels <- lapply(seq(nrow(fish_data_sum)), function(i) {
      paste0(
        "Site: ", fish_data_sum[i, "CE_Site_ID"], "<br>",
        "Fish Present:", "<br>",
        fish_data_sum[i, "fish_present"]
      )
    })
    
    common_fish = fish_data_clip %>% ungroup() %>% group_by(Common_name) %>% summarise(count = n()) %>% arrange(-count)
    nfish = nrow(common_fish)
    
    if (nfish>4){
    common_fish_text = paste0(common_fish$Common_name[1:4], collapse=", ")
    common_fish_text = paste0(
      "Common fishes captured in sampling programs include ", 
      common_fish_text, " and ", common_fish$Common_name[5])}else{
        if(nfish>0){
          common_fish_text = paste0(common_fish$Common_name[1:(nfish-1)], collapse=", ")
          common_fish_text = paste0(
            "Common fishes captured in sampling programs include ", 
            common_fish_text, " and ", common_fish$Common_name[nfish])
        }else{common_fish_text = ""}
        
      }
    ############
    
    #SAR Critical Habitat 
    ###############
  if (input$watershed_scale == "NHN Tertiary Watersheds") {
       keeps <- NHN_SARA %>% filter(WSCSSDA_ID == focal_watershed_ID)
    } else {
	      keeps <- PRV_SARA %>% filter(PRPWSHID == focal_watershed_ID)
		} 

    query = paste0("SELECT * FROM DFO_SARA_CritHab_22_WGS84 WHERE SARA_ID IN ", "('", paste(as_vector(keeps$SARA_ID), collapse="', '"), "')")
    crithab_plot <- read_sf("Critical_habitat/DFO_SARA_CritHab_22/DFO_SARA_CritHab_22_WGS84.gpkg", query =  query)
    
    crithab_clip <- st_drop_geometry(crithab_plot)

    ch_labels <- lapply(seq(nrow(crithab_clip)), function(i) {
      paste0(
        "Critical habitat for ", crithab_clip[i, "Common_Nam"], "<br>",
        "Population / Designated Unit: ", crithab_clip[i, "Population"], "<br>",
        "SARA Status: ", crithab_clip[i, "SARA_Statu"], "<br>",
        crithab_clip[i, "Species_Li"]
      )
    })
    #############
    
    #Water quality data
    ###########
    
    wq_sites$WQI_category[which(is.na(wq_sites$WQI_category))]<- "Data Deficient"
    wq_sites$WQI_category<-fct_relevel(as_factor(wq_sites$WQI_category), "Excellent", "Good", "Fair", "Marginal", "Poor", "Data Deficient")
    
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      wq_clip_sites <- wq_sites %>% filter(WSCSSDA == focal_watershed_ID) 
      wq_clip <- wq_data %>% filter(Site_ID %in% wq_clip_sites$Site_ID)
    } else {
      wq_clip_sites <- wq_sites %>% filter(PRPWSHID == focal_watershed_ID)
      wq_clip <- wq_data %>% filter(Site_ID %in% wq_clip_sites$Site_ID)
    }
    
    wq_clip_sites$WQI_cat_char = as.character(wq_clip_sites$WQI_category)
    
    wq_plot <- st_as_sf(
      x = wq_clip_sites,
      coords = c("Longitude", "Latitude"),
      crs = 4269
    )
    
    wq_plot <- st_transform(wq_plot, crs = 4326)
    
    wq_labels <- lapply(seq(nrow(wq_clip_sites)), function(i) {
      paste0(
        "Water quality", "<br>",
        "StationID: ", wq_clip_sites[i, "Site_ID"], "<br>",
        "Water quality index: ", round(wq_clip_sites[i, "WQI"],0), "<br>",
        "Water quality description: ", wq_clip_sites[i, "WQI_cat_char"]
      )
    })
    #############
    
    # Theobald Human Modification data
    ###########
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      fp <- paste0(
        "Human modification/GHM_17_NHN/GHM17_", focal_watershed_ID,
        "/GHM17_", focal_watershed_ID, ".tif"
      )
    } else {
      fp <- paste0(
        "Human modification/GHM_17_PRV/GHM17_", focal_watershed_ID,
        "/", focal_watershed_ID, ".tif"
      )
    } 
    
    hum <- read_stars(fp)
    
    hum_pal <- colorNumeric(
      palette = "magma",
      domain = c(0, 1),
      na.color = NA
    )
    
    # hum_text = paste("The watershed has a mean landscape modification value of",
    #                  round(cellStats(hum, stat='mean'),3), "with a range from", 
    #                  round(cellStats(hum, stat='min'), 3), "to",
    #                  round(cellStats(hum, stat='max'),3), ".")
    ########
    
    ###Sentinel LULC palette and classes
    #####
    sent_classes = tribble(
      ~ value,
      ~ class,
      ~ col,
      1,
      "Water",
      "blue",
      2,
      "Trees",
      "darkgreen",
      4,
      "Flooded vegetation",
      "cyan",
      5,
      "Crops",
      "yellow",
      7,
      "Built Area",
      "red",
      8,
      "Bare ground",
      "burlywood",
      9,
      "Snow/Ice",
      "black",
      10,
      "Clouds",
      "white",
      11,
      "Rangeland",
      "greenyellow"
    )
    
    sent_pal <- colorFactor(palette = sent_classes$col,
                            na.color = NA, 
                            sent_classes$value)
    ######
    
    # Sentinel LULC raster
    ###########
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      sent <- paste0(
         "Sentinel_LULC_v2/Sent21_NHN/", focal_watershed_ID,
       "/Sen_", focal_watershed_ID, "_3857.tif"
      )
    } else {
      sent <- paste0(
        "Sentinel_LULC_v2/Sent21_PRV/", focal_watershed_ID,
       "/Sen_", focal_watershed_ID, "_3857.tif"
      )
          } 
    
     sent_raster = read_stars(sent, proxy=FALSE)
     
#read in summary file
  
     if (input$watershed_scale == "NHN Tertiary Watersheds") {
       sentsum <- paste0(
         "Sentinel_LULC_v2/Sent21_NHN/", focal_watershed_ID,
         "/Sen_", focal_watershed_ID, "_summary.csv"
       )
     } else {
       sentsum <- paste0(
         "Sentinel_LULC_v2/Sent21_PRV/", focal_watershed_ID,
         "/Sen_", focal_watershed_ID, "_summary.csv"
       )
       
     } 
     
     sent_sum = read_csv(sentsum)
  
    LULC_statement = paste0(
      "The predominant land cover classes are ",
       paste0(sent_sum$class[1], " (", sent_sum$prop[1], "%), "),
       paste0(sent_sum$class[2], " (", sent_sum$prop[2], "%), "),
       paste0(sent_sum$class[3], " (", sent_sum$prop[3], "%) "), "and ",
       paste0(sent_sum$class[4], " (", sent_sum$prop[4], "%). "))
    #############
    
    # CABD dams
    ########
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      dams_clip <- dams %>% filter(NHN4DWSH == focal_watershed_ID)
    } else {
      dams_clip <- dams %>% filter(PRPWSHID == focal_watershed_ID)
    }

    dams_clip <- dams_clip %>% mutate(height_m = ifelse(height_m == 0, NA, height_m))

    dams_plot <- st_as_sf(
      x = dams_clip,
      coords = c("Longitude", "Latitude"),
      crs = 4326
    )
    
    dam_labels <- lapply(seq(nrow(dams_clip)), function(i) {
      paste0(
        "Dam", "<br>",
        "ID: ", dams_clip[i, "cabd_id"], "<br>",
        "Use: ", dams_clip[i, "dam_use"], "<br>",
        "Height: ", dams_clip[i, "height_m"], "<br>",
        "Passability: ", dams_clip[i, "passabilit"]
      )
    })
    ############
    
    # CABD waterfalls
    #############
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      wfs_clip <- wfs %>% filter(NHN4DWSH == focal_watershed_ID)
    } else {
      wfs_clip <- wfs %>% filter(PRPWSHID == focal_watershed_ID)
    }

    wfs_clip <- wfs_clip %>% mutate(fallheight = ifelse(fallheight == 0, NA, fallheight))

    wfs_plot <- st_as_sf(
      x = wfs_clip,
      coords = c("Longitude", "Latitude"),
      crs = 4326
    )

    wfs_labels <- lapply(seq(nrow(wfs_clip)), function(i) {
      paste0(
        "Waterfall", "<br>",
        "ID: ", wfs_clip[i, "cabd_id"], "<br>",
        "Height: ", wfs_clip[i, "fallheight"], "<br>",
        "Passability: ", wfs_clip[i, "passabilit"]
      )
    })
    ###########
    
    # CABD fishways
    ############
    if (input$watershed_scale == "NHN Tertiary Watersheds") {
      fishways_clip <- fishways %>% filter(NHN4DWSH == focal_watershed_ID)
    } else {
      fishways_clip <- fishways %>% filter(PRPWSHID == focal_watershed_ID)
    }

    fishways_clip <- fishways_clip %>% mutate(elevationm = ifelse(elevationm == 0, NA, elevationm))


    fishways_plot <- st_as_sf(
      x = fishways_clip,
      coords = c("Longitude", "Latitude"),
      crs = 4326
    )

    fishways_labels <- lapply(seq(nrow(fishways_clip)), function(i) {
      paste0(
        "Fishway", "<br>",
        "ID: ", fishways_clip[i, "cabd_id"], "<br>",
        "Type: ", fishways_clip[i, "fishpasst1"]
      )
    })
    ############
  
    # Make icons
    #############
   
    wq_icons = iconList( 
    
      greenDrop <- makeIcon(
        iconUrl = "Image_files/water_drop_green.png",
        iconWidth = 16, iconHeight = 16,
        iconAnchorX = 8, iconAnchorY = 8
      ),
      
      greenDrop2 <- makeIcon(
        iconUrl = "Image_files/water_drop_green.png",
        iconWidth = 16, iconHeight = 16,
        iconAnchorX = 8, iconAnchorY = 8
      ),

    yellowDrop <- makeIcon(
      iconUrl = "Image_files/water_drop_yellow.png",
      iconWidth = 16, iconHeight = 16,
      iconAnchorX = 8, iconAnchorY = 8
    ),
    
    orangeDrop <- makeIcon(
      iconUrl = "Image_files/water_drop_orange.png",
      iconWidth = 16, iconHeight = 16,
      iconAnchorX = 8, iconAnchorY = 8
    ),
    
    redDrop <- makeIcon(
      iconUrl = "Image_files/water_drop_red.png",
      iconWidth = 16, iconHeight = 16,
      iconAnchorX = 8, iconAnchorY = 8
    ),
    
    greyDrop <- makeIcon(
      iconUrl = "Image_files/water_drop_grey.png",
      iconWidth = 16, iconHeight = 16,
      iconAnchorX = 8, iconAnchorY = 8
    )
        )
    
    fishIcon <- makeIcon(
      iconUrl = "Image_files/fish_green.png",
      iconWidth = 16, iconHeight = 16,
      iconAnchorX = 8, iconAnchorY = 8
    )
    
    square_green <- makeIcon(
      iconUrl = "Image_files/square-green.png",
      iconWidth = 16, iconHeight = 16,
      iconAnchorX = 8, iconAnchorY = 8
    )
    
    square_red <- makeIcon(
      iconUrl = "Image_files/square-red.png",
      iconWidth = 16, iconHeight = 16,
      iconAnchorX = 8, iconAnchorY = 8
    )
    
    square_blue <- makeIcon(
      iconUrl = "Image_files/square-blue.png",
      iconWidth = 16, iconHeight = 16,
      iconAnchorX = 8, iconAnchorY = 8
    )
    #############
    
    # Render map
    ############

      grouplist=c("Landscape modification",
                  "Land use / Land cover",
                  "Fish observations",
                  "Water quality",
                  "Aquatic barriers",
                  "Industrial activities",
                  "Critical habitat",
                  "Protected areas")
    
    leafletProxy("map") %>%
      setView(lng = site_x, lat = site_y, zoom = 09) %>%
      clearGroup(group = c(grouplist, "focal_watershed")) %>%
      clearShapes() %>%
      clearControls() %>%

      addStarsImage(hum,
          project = FALSE, 
          opacity = 0.75,
          colors = hum_pal,
          group = "Landscape modification"
        ) %>%
        addLegend(
         colors =  viridis(11, alpha = 1, begin = 0, end = 1, direction = 1, option = "magma"),
          labels = c("0 - less", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6","0.7","0.8","0.9", "1 - more"),
          title = "Landscape<br>modification"
        ) %>%
      addStarsImage(sent_raster,
                     project = FALSE, 
                     opacity = 0.70,
                     colors = sent_pal,
                     group = "Land use / Land cover"
      ) %>%
      addLegend(
        colors = sent_classes$col[c(2,9,4,1,3,5,6,7,8)],
        labels = sent_classes$class[c(2,9,4,1,3,5,6,7,8)],
        title = "Land use /<br>Land cover"
      ) %>%
      addPolygons( #PAs
        data = pa_plot,
        stroke = 0.05,
        group = "Protected areas",
        color = "green",
        label = lapply(pa_labels, htmltools::HTML),
        fillColor = "green",
        opacity = 0.5,
        fillOpacity = 0.2
      ) %>%
      addPolylines( #major project lines
        data = mp_lines_plot,
        stroke = 0.5,
        group = "Industrial activities",
        color = "purple",
        label = lapply(mp_lines_labels, htmltools::HTML),
        fillColor = "purple"
      ) %>%
      addCircleMarkers( #major project points
        data = mp_points_plot,
        group = "Industrial activities",
        color = "black",
        label = lapply(mp_points_labels, htmltools::HTML),
        fillColor = "purple",
        radius = 6,
        weight = 1,
        opacity = 1,
        fillOpacity = 1
      )  %>%
        addPolygons(
          data = crithab_plot,
          stroke = 0.05,
          group = "Critical habitat",
          color = "red",
          label = lapply(ch_labels, htmltools::HTML),
          fillColor = "red",
          opacity = 0.5,
          fillOpacity = 0.2
        ) %>%
        addPolygons( # focal watershed
          data = focal_watershed,
          stroke = 0.1,
          color = "black",
          fillColor = "transparent",
          group = "focal_watershed"
        ) %>%
        addMarkers( # Fish observations
          data = fish_plot,
          group = "Fish observations",
          label = lapply(fish_labels, htmltools::HTML),
          #clusterOptions = markerClusterOptions(),
          icon = fishIcon
  
        ) %>%
      addCircleMarkers( # NPRI pollutants
        data = npri_plot,
        group = "Industrial activities",
        label = lapply(npri_labels, htmltools::HTML),
        radius = 6,
        weight = 1,
        opacity = 1,
        color = "black",
        fillColor = "orange",
        fillOpacity = 1
      ) %>%
      addCircleMarkers( # Wastewater
        data = wastewater_plot,
        group = "Industrial activities",
        label = lapply(wastewater_labels, htmltools::HTML),
        radius = 6,
        weight = 1,
        opacity = 1,
        color = "black",
        fillColor = "aquamarine",
        fillOpacity = 1
      ) %>%
      addCircleMarkers( # Federal contaminated sites
        data = contaminated_sites_plot,
        group = "Industrial activities",
        label = lapply(contaminated_sites_labels, htmltools::HTML),
        radius = 6,
        weight = 1,
        opacity = 1,
        color = "black",
        fillColor = "brown",
        fillOpacity = 1
      ) %>%
      addCircleMarkers( # Oil and gas
        data = oilgas_plot,
        group = "Industrial activities",
        label = lapply(oilgas_labels, htmltools::HTML),
        radius = 6,
        weight = 1,
        opacity = 1,
        fillColor = "black",
        fillOpacity = 1
      ) %>%
      addCircleMarkers( # Metalworks
        data = metalworks_plot,
        group = "Industrial activities",
        label = lapply(metalwork_labels, htmltools::HTML),
        radius = 6,
        weight = 1,
        opacity = 1,
        color = "black",
        fillColor = "white",
        fillOpacity = 1
      ) %>%
      addCircleMarkers( # Mines
        data = mines_plot,
        group = "Industrial activities",
        label = lapply(mine_labels, htmltools::HTML),
        radius = 6,
        weight = 1,
        opacity = 1,
        color = "black",
        fillColor = "grey",
        fillOpacity = 1
      ) %>%
        addMarkers( # waterfalls
          data = wfs_plot,
          group = "Aquatic barriers",
          label = lapply(wfs_labels, htmltools::HTML),
          icon = square_blue
        ) %>%
        addMarkers( # fishways
          data = fishways_plot,
          group = "Aquatic barriers",
          label = lapply(fishways_labels, htmltools::HTML),
          icon = square_green
        ) %>%
        addMarkers( # dams
          data = dams_plot,
          group = "Aquatic barriers",
          label = lapply(dam_labels, htmltools::HTML),
          icon = square_red
        ) %>%
        addMarkers( # water quality
          data = wq_plot,
          group = "Water quality",
          label = lapply(wq_labels, htmltools::HTML),
          icon = ~wq_icons[as.numeric(WQI_category)]
        ) %>%
        addAwesomeMarkers(
          group = "project_site", # project site
          lng = site_x,
          lat = site_y,
          label = "Project site",
          icon = siteIcon
        ) %>%
        hideGroup(group = grouplist) %>%

        addLayersControl(
          overlayGroups = grouplist,
          position = "bottomleft",
          options = layersControlOptions(collapsed = FALSE)
        )


    ############
    
    # Build watershed summary text
    ##########
    
    #output$feow <- renderText({ focal_feow })
    #output$fwbgz <- renderText({ focal_fwbgz })
    output$lulc <- renderText({ LULC_statement })
    output$sum <- renderText({ sum_text })
    #output$humtext <- renderText({hum_text})
    output$common_species <- renderText({common_fish_text})
    output$zones <- renderText({zone_text})
 
    ##########
   
    # Table 1 - Fish species list
    #############

data_t1<- reactive({
 if (input$watershed_scale == "NHN Tertiary Watersheds") {
        tmp = NHN_splist %>%
        filter(NHN_TWS == focal_watershed_ID) %>%
        dplyr::select(-NHN_TWS)
    } else {
      tmp = NHN_splist %>%
        filter(NHN_TWS == focal_NHN_ID) %>%
        dplyr::select(-NHN_TWS)
    }
tmp = tmp %>% dplyr::select(Common_name, Scientific_name, Species_origin) %>% arrange(Common_name)
tmp = tmp[!base::duplicated(tmp$Common_name),]
tmp
})

    output$table1 <- renderDT(
      {
        data_t1()
      },
      rownames = FALSE,
      options = list(pageLength = 20)
    )

    output$downloadTable1 <- downloadHandler(
      filename = function() {
        paste0("Watershed_", focal_watershed_ID, "_Species_list.csv")
      },
      content = function(file) {
        write_csv(data_t1(), file)
      }
    )
    #############

    # Table 2 - Fish captures
    #############
    data_t2 <- reactive( {
            tmp = fish_data_clip %>% dplyr::select(
            Common_name,
            Scientific_name,
            CE_Site_ID,
            LastOfDate,
            Latitude,
            Longitude,
            Waterbody_name)
           
           tmp <- tmp %>% arrange(CE_Site_ID, Common_name, LastOfDate)

         colnames(tmp) <-
            c(
              "Common name",
              "Scientific name",
              "Site ID",
              "Last caught",
              "Latitude",
              "Longitude",
              "Waterbody name"
            )
	   tmp$Latitude <-round(tmp$Latitude, 6)
	   tmp$Longitude <-round(tmp$Longitude, 6)
             tmp })


    output$table2 <- DT::renderDataTable(

        {data_t2()}, 

        rownames = FALSE,
        options = list(pageLength = 20)
      )
    

    output$downloadTable2 <- downloadHandler(
      filename = function() {
        paste0("Watershed_", focal_watershed_ID, "_Fish_presence_by_site.csv")
      },
      content = function(file) {
        write_csv(data_t2(), file)
      }
    )
    ##############

    ## Table 3 - water quality
    ############
	data_t3 <- reactive({
        tmp <- wq_clip %>% dplyr::select(
      -WSCSSDA, -PRPWSHID )
	  tmp }) 

    output$table3 <- renderDataTable(
      { data_t3()  },
      rownames = FALSE,
      options = list(pageLength = 20)
    )

    output$downloadTable3 <- downloadHandler(
      filename = function() {
        paste0("Watershed_", focal_watershed_ID, "_Water_quality_data.csv")
      },
      content = function(file) {
        write_csv(data_t3(), file)
      }
    )
    ############
    
   ## Table 4 - aquatic barriers
   ##############
	data_t4<-reactive({

tmp_dam <- dams_clip %>% dplyr::select(
          featuretyp, cabd_id, Latitude, Longitude, height_m, passabilit, dam_use
        )
        names(tmp_dam)[7] <- "known_use"

        tmp_wfs <- wfs_clip %>% dplyr::select(
          featuretyp, cabd_id, Latitude, Longitude, fallheight, passabilit
        )
        names(tmp_wfs)[5] <- "height_m"

        tmp_fw <- fishways_clip %>% dplyr::select(
          featuretyp, cabd_id, Latitude, Longitude, elevationm, known_use
        )
        tmp_fw$passabilit <- "assumed"
        names(tmp_fw)[5] <- "height_m"

        sp <- str_replace_all(tmp_fw$known_use, ",", "XXXX")
        sp <- str_replace_all(sp, " \\s*\\([^\\)]+\\)", "")
        sp <- str_replace_all(sp, "[:punct:]", "")
        sp <- str_replace_all(sp, "XXXX", ", ")
        tmp_fw$known_use <- sp

        tmp <- bind_rows(tmp_dam, tmp_fw, tmp_wfs)
        names(tmp) <- c(
          "Barrier_type", "ID", "Latitude", "Longitude", "Height_m",
          "Passability", "Known_use"
        )
        tmp$Latitude <-round(tmp$Latitude, 6)
	   tmp$Longitude <-round(tmp$Longitude, 6)

       tmp
})

    output$table4 <- renderDataTable(
      {
        data_t4()
      },
      rownames = FALSE,
      options = list(pageLength = 20)
    )

    output$downloadTable4 <- downloadHandler(
      filename = function() {
        paste0("Watershed_", focal_watershed_ID, "_aquatic_barriers.csv")
      },
      content = function(file) {
        write_csv(data_t4(), file)
      }
    )
   #################
    
   ## Table Industrial Activities
   #######
    data_ind<- reactive({
      
      mp_lines_tmp = mp_lines_clip %>%
        rename(Name = proj_name, Owner = company) %>%
        mutate(Facility_Type = "Major Project",
               Known_Releases = NA,
               Latitude = NA,
               Longitude = NA,
               Description = str_c(status, type, sep = " ")) %>%
        dplyr::select(Facility_Type, Name, Owner, Latitude, Longitude, Description, Known_Releases) %>%
        mutate_all( as.character)
      
      mp_points_tmp = mp_points_plot %>%
        dplyr::mutate(Longitude = sf::st_coordinates(.)[,1],
                      Latitude = sf::st_coordinates(.)[,2]) %>%
        rename(Name = proj_name, Owner = company) %>%
        mutate(Facility_Type = "Major Project",
               Longitude = round(Longitude, 6),
               Latitude = round(Latitude, 6),
               Known_Releases = NA,
              
               Description = str_c(status, sector, "Project", sep = " ")) %>%
        dplyr::select(Facility_Type, Name, Owner, Latitude, Longitude, Description, Known_Releases) %>%
        mutate_all( as.character) %>% st_drop_geometry()
      
        
      mines_tmp = mines_clip %>% 
        rename(Name = OPERATIO_E, Owner = OWNER_E) %>%
        mutate(Facility_Type = "Mine", 
               Known_Releases = NA,
               Description = str_c(FAC_DESC_E, COMMODIT_E, sep = " - ")) %>% 
        dplyr::select(Facility_Type, Name, Owner, Latitude, Longitude, Description, Known_Releases) %>%
        mutate_all( as.character)
      
      metalworks_tmp = metalworks_clip %>% 
        rename(Name = OPERATIO_E, Owner = OWNER_E) %>%
        mutate(Facility_Type = FAC_TYPE_E, 
               Known_Releases = NA,
               Description = COM_GROU_E) %>% 
        dplyr::select(Facility_Type, Name, Owner, Latitude, Longitude, Description, Known_Releases)%>%
        mutate_all( as.character)
      
      oilgas_tmp = oilgas_clip %>% 
        rename(Name = MINESITE_E) %>%
        mutate(Owner = NA,
               Facility_Type = PRODUCT_E, 
               Known_Releases = NA,
               Description = NA) %>% 
        dplyr::select(Facility_Type, Name, Owner, Latitude, Longitude, Description, Known_Releases)%>%
        mutate_all( as.character)
      
      npri_tmp = npri_clip %>% 
        rename(Name = Facility_Name, Owner = Company_Name) %>%
        mutate(Facility_Type = str_c("National Pollutant Release Inventory Site", Key_Industrial_Sector, sep=" - "),
               Known_Releases = Substances,
               Description = str_c("Most recent reporting year = ", Year_of_last_filed_report )) %>%
        dplyr::select(Facility_Type, Name, Owner, Latitude, Longitude, Description, Known_Releases)%>%
        mutate_all( as.character)
      
      wastewater_tmp = wastewater_clip %>% 
        rename(Name = System_Nam, Owner = Owner_Name) %>%
        mutate(Facility_Type = "Wastewater treatment", 
               Known_Releases = str_c(Average_Daily_Effluent_Volume_m3, " (m3) average daily effluent"),
               Description = Treatment_) %>% 
        dplyr::select(Facility_Type, Name, Owner, Latitude, Longitude, Description, Known_Releases)%>%
        mutate_all( as.character)
      
      contaminated_sites_tmp = contaminated_sites_clip %>% 
        rename(Name = Site_Name_, Owner = DOB_Name_E) %>%
        mutate(Facility_Type = "Federal contaminated site", 
               Known_Releases = Contaminan,
               Description = Contaminat) %>% 
        dplyr::select(Facility_Type, Name, Owner, Latitude, Longitude, Description, Known_Releases)%>%
        mutate_all( as.character)
      
      tmp = bind_rows(
        mines_tmp, metalworks_tmp, oilgas_tmp, npri_tmp, 
        wastewater_tmp, contaminated_sites_tmp,
        mp_points_tmp,
        mp_lines_tmp
      )
      tmp$Latitude <-as.numeric(tmp$Latitude)
      tmp$Longitude<-as.numeric(tmp$Longitude)
      
      tmp %>% arrange(Facility_Type)    
      
      
    })
    
    output$table_ind <- renderDT(
      {
        data_ind()
      },
      rownames = FALSE,
      options = list(pageLength = 20)
    )
    
    
    output$downloadTableInd <- downloadHandler(
      filename = function() {
        paste0("Watershed_", focal_watershed_ID, "_Industrial_Activities.csv")
      },
      content = function(file) {
        write_csv(data_ind(), file)
      }
    )###  
   ##########  
    
	#Critical habitat download
  ###########
    output$downloadTable6 <- downloadHandler(
      filename = function() {
        paste0("Watershed_", focal_watershed_ID, "species_with_critical_habitat.csv")
      },
      content = function(file) {
        write_csv(crithab_clip, file)
      }
    )
  ############

    removeModal() #this is the 'data processing' modal
    
   #Message for missing data layers for a focal watershed
   #################
        
    n_PA = nrow(pa_clip) #pas
    n_fish = nrow(fish_data_sum)
    n_WQ = nrow(wq_clip)
    n_crithab = nrow(crithab_clip)
    n_barrier = nrow(fishways_clip) + nrow(wfs_clip) + nrow(dams_clip)
    n_ind = nrow(mines_clip) + nrow(metalworks_clip) + nrow(oilgas_clip) + nrow(npri_clip) + nrow(wastewater_clip) + nrow(contaminated_sites_clip)
    
    if(any( c(n_PA, n_crithab, n_fish, n_WQ, n_barrier, n_ind) == 0)) { 
    
    messages = c(
      "There are no protected areas in this watershed.",
      "There is no designated critical habitat within this watershed.",
      "CICADA does not have any fish observations for this watershed.",
      "CICADA does not have any water quality data for this watershed.",
      "CICADA does not have any aquatic barrier data for this watershed.",
      "CICADA does not have any industrial activity data for this watershed."
      #"Resolution of land use / land cover raster has been reduced to 100m x 100m."
    ) 
    
    focal_message = str_c(messages[which( c(n_PA, n_crithab, n_fish, n_WQ, n_barrier, n_ind) == 0)], collapse="<br>")
      
      
      showModal(modalDialog(
      title = "Note",
      HTML(focal_message),
      easyClose = TRUE
    ))
    }
   #################
    
  }
  ) # closes the generate map button
  
} # end of server


shinyApp(ui = ui, server = server)


