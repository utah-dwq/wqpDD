
### WQP data dashboard

#setwd('C:\\Users\\jvander\\Documents\\R\\wqpDD')

# Packages
library(shiny)
library(shinyBS)
library(wqTools)
library(leaflet)
library(jsonlite)
library(plotly)

# Helpers
source('helpers/figuresMod.R')

# Query domains
statecodes=unique(fromJSON('https://www.waterqualitydata.us/Codes/statecode?countrycode=US&mimeType=json')$codes)
#countycodes=fromJSON('https://www.waterqualitydata.us/Codes/countycode?statecode=US:01;US:04&mimeType=json')$codes
sitetypes=unique(fromJSON('https://www.waterqualitydata.us/Codes/Sitetype?mimeType=json')$codes$value)
orgids=unique(fromJSON('https://www.waterqualitydata.us/Codes/Organization?mimeType=json')$codes$value)
samplemedia=unique(fromJSON('https://www.waterqualitydata.us/Codes/Samplemedia?mimeType=json')$codes$value)
characteristicnames=unique(fromJSON('https://www.waterqualitydata.us/Codes/Characteristicname?mimeType=json')$codes$value)[order(unique(fromJSON('https://www.waterqualitydata.us/Codes/Characteristicname?mimeType=json')$codes$value))]
aus=unique(as.character(wqTools::au_poly$ASSESS_ID))
huc12s=unique(as.character(wqTools::huc12_poly$HUC12))

ui <-fluidPage(

	# Header
	headerPanel(
		title=tags$a(href='https://deq.utah.gov/division-water-quality/',tags$img(src='deq_dwq_logo.png', height = 75, width = 75*2.85), target="_blank"),
		tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"), windowTitle="Site Review Application")
	),

	mainPanel(width=11,
		bsCollapse(id='collapse_panels', multiple=T, open=1,
			bsCollapsePanel(list(icon('cloud-download-alt'),"Download data"), value=6,
				#tabPanel('Query by WQP URL',
				#	helpText("Use the WQP web query interface to define your data query. Copy/paste the URL from the top of your browser to the input below and click 'Get data!'"),
				#	helpText(a('WQP web query interface', href='https://www.waterqualitydata.us/portal/#mimeType=csv', target="_blank")),
				#	fluidRow(textInput('wqp_url', 'WQP URL:')),
				#	actionButton('get_data_url', 'Get data!', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('cloud-download-alt'))
				#),
				fluidRow(
					column(3, shinyWidgets::multiInput('statecodes', 'State(s):', choices=statecodes$desc, selected='Utah')),
					#column(3, shinyWidgets::multiInput('countycodes', 'Counties:', choices=countycodes)),
					column(3, shinyWidgets::multiInput('sitetypes', 'Site types:', choices=sitetypes, selected=c('Lake, Reservoir, Impoundment','Stream', 'Spring'))),
					#column(3, shinyWidgets::multiInput('orgids', 'Organization IDs:', choices=orgids)),
					column(3, shinyWidgets::multiInput('samplemedia', 'Sample media:', choices=samplemedia, selected='Water')),
					column(3, dateRangeInput('import_date_range', 'Date range:', end=Sys.Date(), start=Sys.Date()-365))
				),
				actionButton('import_data', 'Download data!', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('cloud-download-alt'))
			),
			bsCollapsePanel(list(icon('database'),"Select data"), value=1,
				fluidRow(
					column(2, shinyWidgets::radioGroupButtons('spatial_sel_type','Select by:', choices=c('Asessment unit','HUC 12'), status = "primary", checkIcon = list(yes = icon("check")))),
					column(2, shinyWidgets::radioGroupButtons('sel_type','Selection type:', choices=c('Map','List'), status = "primary", checkIcon = list(yes = icon("check")))),
					actionButton('select_data', 'Select data!', style='margin-top: 25px; color: #fff; background-color: #4ab05b; border-color: #1d6308', icon=icon('check'))
				),
				# Select by AU
				fluidRow(
					## map
					conditionalPanel(condition="input.spatial_sel_type=='Asessment unit' & input.sel_type=='Map'",
						column(5, shinycssloaders::withSpinner(leafletOutput('au_map'),size=2, color="#0080b7"))
					),
					# list
					conditionalPanel(condition="input.spatial_sel_type=='Asessment unit' & input.sel_type=='List'",
						column(3, uiOutput('aus_multiInput'))
					)
				),
				# Select by HUC12
				fluidRow(
					## map
					conditionalPanel(condition="input.spatial_sel_type=='HUC 12' & input.sel_type=='Map'",
						column(5, shinycssloaders::withSpinner(leafletOutput('huc_map'),size=2, color="#0080b7"))
					),
					# list
					conditionalPanel(condition="input.spatial_sel_type=='HUC 12' & input.sel_type=='List'",
						column(3, uiOutput('hucs_multiInput'))
					)
				)
				#fluidRow(
				#	column(3, shinyWidgets::multiInput('sitetypes', 'Site types:', choices=sitetypes, selected=c('Lake, Reservoir, Impoundment','Stream', 'Spring'))),
				#	column(3, shinyWidgets::multiInput('orgids', 'Organization IDs:', choices=orgids)),
				#	column(3, shinyWidgets::multiInput('samplemedia', 'Sample media:', choices=samplemedia, selected='Water')),
				#	column(3, shinyWidgets::multiInput('characteristicnames', 'Characteristic names:', choices=characteristicnames))
				#),
				
			),
			bsCollapsePanel(list(icon('cogs'),"Data processing"), value=2,
				bsCollapse(id='data_process_panels', multiple=T, open=1,
					bsCollapsePanel(list(icon('cog'),"Non-detect values"), value=1, 
						shinyWidgets::radioGroupButtons('fill_nd','Fill non-detect values:', choices=c('NA','Detection limit','1/2 detection limit'), checkIcon = list(yes = icon("check")), 
							status = "primary", selected='1/2 detection limit')
					),
					bsCollapsePanel(list(icon('filter'),"Filter data"), value=2,
						column(3,
							uiOutput('filter_col'),
							uiOutput('filter'),
							actionButton('add_filter', 'Add filter', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('plus-circle'))
						),
						column(3,
							uiOutput('filter_picker')
						)
					),
					bsCollapsePanel(list(icon('object-group'),"Combine parameters"), value=3
					)
				)
			),
			bsCollapsePanel(list(icon('table'),"Data table"), value=3,
				div(DT::DTOutput("datatable"), style = list("font-size:65%"))
			),
			bsCollapsePanel(list(icon('chart-bar'),"Analyze data"), value=4,
				figuresModUI('figures')
			),
			bsCollapsePanel(list(icon('file-export'),"Write report"), value=5,
				helpText('Report writing capabilities in concept stage.')
			)
		)
	)
)

# Server
server <- function(input, output, session){

## Empty reactive values object
reactive_objects=reactiveValues()

## Read data
data_path=paste0(path.package('wqTools'),'/extdata')
load(system.file('extdata', "wqpDD_data.Rdata", package='wqTools'))

if(file.exists(paste0(data_path,"/new_wqpDD.txt"))){
	showModal(modalDialog(easyClose=F, title="Looks like you're new here...", 
		  "This app has been preloaded with a working dataset..."))
	file.remove(paste0(data_path,"/new_wqpDD.txt"))
}

## Import data
observeEvent(input$import_data, {
	req(input$statecodes, input$sitetypes, input$samplemedia, input$import_date_range)
	showModal(modalDialog(easyClose=F, title='Downloading data', 'This may take a while.', footer=NULL))
	statecode=subset(statecodes, desc==input$statecodes)$value
	downloadWQP(outfile_path=data_path, statecode=statecode, sampleMedia=input$samplemedia, siteType=input$sitetypes, 
		start_date=input$import_date_range[1], end_date=input$import_date_range[2], retrieve='result')
	downloadWQP(outfile_path=data_path, statecode=statecode, sampleMedia=input$samplemedia, siteType=input$sitetypes, retrieve='sites')
	result=read.csv(paste0(data_path,"/result-", Sys.Date(), ".csv"))
	showModal(modalDialog(easyClose=F, title='Processing data', 'This may take a while.', footer=NULL))
	sites=read.csv(paste0(data_path,"/sites-", Sys.Date(), ".csv"))
	sites=subset(sites, MonitoringLocationIdentifier %in% result$MonitoringLocationIdentifier)
	sites=assignAUs(sites)
	sites=assignHUCs(sites)
	wqp_data=merge(sites, result, all.y=T)
	save(wqp_data, file=paste0(data_path,"/wqpDD_data.Rdata"))
	file.remove(paste0(data_path,"/result-", Sys.Date(), ".csv"))
	file.remove(paste0(data_path,"/sites-", Sys.Date(), ".csv"))
	rm(wqp_data)
	load(paste0(data_path,"/wqpDD_data.Rdata"))
	removeModal()
})


## AU multiInput
output$aus_multiInput=renderUI({
	shinyWidgets::multiInput('aus_multiInput', 'Assessment units:', choices=aus)
})

## Update selected AUs from AU multiInput
observeEvent(input$aus_multiInput, ignoreNULL=F, {
	reactive_objects$sel_aus_mi=input$aus_multiInput
})

## AU map input
session$onFlushed(once = T, function() {
	output$au_map=renderLeaflet({
		wqTools::buildMap(plot_polys=F) %>%
		addPolygons(data=wqTools::au_poly,group="Assessment units",smoothFactor=2,fillOpacity = 0.1, layerId=wqTools::au_poly$polyID,weight=3,color="orange", options = pathOptions(pane = "au_poly"),
			label=lapply(paste0(
				'<p>',
				"AU name: ", wqTools::au_poly$AU_NAME,
				'<br />', "AU ID: ", wqTools::au_poly$ASSESS_ID,
				'<br />', "AU type: ", wqTools::au_poly$AU_Type
			), HTML)
		) %>% 
		addMapPane("highlight", zIndex = 414) %>%
		leaflet::addLayersControl(
					position ="topleft",
					baseGroups = c("Topo","Satellite"),overlayGroups = c("Assessment units"),
					options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE)) %>%
		showGroup('Assessment units')
	})
})

au_map_proxy=leafletProxy("au_map")


## Select AUs on map click
observeEvent(input$au_map_shape_click,{
	au_click = input$au_map_shape_click$id
	if(!is.null(au_click)){
		au_id=as.character(unique(au_poly$ASSESS_ID[au_poly$polyID==au_click]))
		if(au_id %in% reactive_objects$sel_aus_map){
			reactive_objects$sel_aus_map=reactive_objects$sel_aus_map[!reactive_objects$sel_aus_map %in% au_id]
		}else{
			reactive_objects$sel_aus_map=append(reactive_objects$sel_aus_map, au_id)
		}
	}
})

## Update map AU highlight
observeEvent(reactive_objects$sel_aus_map, ignoreNULL = F, ignoreInit=T, {
	au_map_proxy %>%
		clearGroup(group='highlight') %>%
		addPolygons(data=wqTools::au_poly[wqTools::au_poly$ASSESS_ID %in% reactive_objects$sel_aus_map,],
			group='highlight', options = pathOptions(pane = "highlight"), color='chartreuse', opacity = 0.75, fillOpacity = 0.4, weight = 5)
})


## HUC 12 multiInput
output$hucs_multiInput=renderUI({
	shinyWidgets::multiInput('hucs_multiInput', 'HUC 12s:', choices=huc12s)
})

## Update selected HUC 12s from HUC 12 multiInput
observeEvent(input$hucs_multiInput, ignoreNULL=F, {
	reactive_objects$sel_hucs_mi=input$hucs_multiInput
})

## HUC 12 map input
session$onFlushed(once = T, function() {
	output$huc_map=renderLeaflet({
		wqTools::buildMap(plot_polys=F) %>%
		addPolygons(data=wqTools::huc12_poly, group="HUC 12",smoothFactor=2,fillOpacity = 0.1, layerId=~HUC12,weight=3,color="pink", options = pathOptions(pane = "huc12_poly"),
			label=lapply(paste0(
				'<p>',
				"HUC 12: ", wqTools::huc12_poly$HUC12
			), HTML)
		) %>% 
		addMapPane("highlight", zIndex = 411) %>%
		leaflet::addLayersControl(
					position ="topleft",
					baseGroups = c("Topo","Satellite"),overlayGroups = c("HUC 12"),
					options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex=FALSE)) %>%
		showGroup('HUC 12')
	})
})

huc_map_proxy=leafletProxy("huc_map")


### Select HUCs on map click
observeEvent(input$huc_map_shape_click,{
	huc_id = input$huc_map_shape_click$id
	if(!is.null(huc_id)){
		if(huc_id %in% reactive_objects$sel_hucs_map){
			reactive_objects$sel_hucs_map=reactive_objects$sel_hucs_map[!reactive_objects$sel_hucs_map %in% huc_id]
		}else{
			reactive_objects$sel_hucs_map=append(reactive_objects$sel_hucs_map, huc_id)
		}
	}
})

## Update map HUC highlight
observeEvent(reactive_objects$sel_hucs_map, ignoreNULL = F, ignoreInit=T, {
	huc_map_proxy %>%
		clearGroup(group='highlight') %>%
		addPolygons(data=wqTools::huc12_poly[wqTools::huc12_poly$HUC12 %in% reactive_objects$sel_hucs_map,],
			group='highlight', options = pathOptions(pane = "highlight"), color='chartreuse', opacity = 0.75, fillOpacity = 0.4, weight = 5)
})


## Spatial select data
observeEvent(input$select_data, {
	if(input$spatial_sel_type== 'Assessment unit' & input$sel_type == 'Map'){
		auid=as.vector(reactive_objects$sel_aus_map)
		reactive_objects$spatial_sel_data=subset(wqp_data, ASSESS_ID %in% auid)
	}
	if(input$spatial_sel_type== 'Assessment unit' & input$sel_type == 'List'){
		auid=as.vector(reactive_objects$sel_aus_mi)
		reactive_objects$spatial_sel_data=subset(wqp_data, ASSESS_ID %in% auid)
	}
	if(input$spatial_sel_type== 'HUC 12' & input$sel_type == 'Map'){
		hucs=as.vector(reactive_objects$sel_hucs_map)
		reactive_objects$spatial_sel_data=subset(wqp_data, HUC12 %in% hucs)
	}
	if(input$spatial_sel_type== 'HUC 12' & input$sel_type == 'List'){
		auid=as.vector(reactive_objects$sel_hucs_mi)
		reactive_objects$spatial_sel_data=subset(wqp_data, HUC12 %in% hucs)
	}
})


## Query data by url
#observeEvent(input$get_data_url,{
#	req(input$wqp_url)
#	qurl=input$wqp_url
#	result_qurl=gsub('/portal/#', '/data/Result/search?', qurl)
#	site_qurl=gsub('/portal/#', '/data/Station/search?', qurl)
#	showModal(modalDialog(easyClose=F, 'Reading data...', footer=NULL))
#	data_raw=as.data.frame(data.table::fread(result_qurl))
#	qsites=as.data.frame(data.table::fread(site_qurl))
#	data_raw=merge(data_raw, qsites, all.x=T)
#	reactive_objects$data_raw=data_raw
#	data_raw=wqTools::assignAUs(data_raw)
#	data_raw$ActivityStartDate=as.Date(data_raw$ActivityStartDate)
#	data_raw$Month=lubridate::month(as.Date(data_raw$ActivityStartDate))
#	data_raw$Year=lubridate::year(as.Date(data_raw$ActivityStartDate))
#	reactive_objects$data_raw=data_raw
#	removeModal()
#})


## Filters
### Select column to filter
output$filter_col=renderUI({
	req(reactive_objects$spatial_sel_data)
	selectInput('filter_col', 'Attribute:', choices=names(reactive_objects$spatial_sel_data))
})

### Build filter UI
output$filter=renderUI({
	req(input$filter_col)
	if(class(reactive_objects$spatial_sel_data[,input$filter_col])=='numeric' | class(reactive_objects$spatial_sel_data[,input$filter_col])=='Date'){
		sliderInput('filter', "Filter:", min=min(reactive_objects$spatial_sel_data[,input$filter_col], na.rm=T), max=max(reactive_objects$spatial_sel_data[,input$filter_col], na.rm=T), value=c(min(reactive_objects$spatial_sel_data[,input$filter_col], na.rm=T),max(reactive_objects$spatial_sel_data[,input$filter_col], na.rm=T)))
	}else{
		shinyWidgets::pickerInput("filter", "Filter:", choices=as.character(unique(reactive_objects$spatial_sel_data[,input$filter_col])), multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
	}
})

reactive_objects$filters=vector()

### Add filter
observeEvent(input$add_filter,{
	req(input$filter, input$filter_col)
	if(class(reactive_objects$spatial_sel_data[,input$filter_col])=='numeric' | class(reactive_objects$spatial_sel_data[,input$filter_col])=='Date'){
		if(class(reactive_objects$spatial_sel_data[,input$filter_col])=='numeric'){
			filter_n=paste0(input$filter_col, ' >= ', input$filter[1], ' & ', input$filter_col, ' <= ', input$filter[2])
		}else{
			filter_n=paste0(input$filter_col, ' >= ', "as.Date('",input$filter[1],"')", ' & ', input$filter_col, ' <= ', "as.Date('",input$filter[2],"')")
		}
	}else{
		filter_n=paste0(input$filter_col, ' %in% ', "c('", paste(input$filter, collapse="', '"), "')")
	}
	
	reactive_objects$filters=append(reactive_objects$filters, filter_n)
	
})

### Filter on/off picker
output$filter_picker=renderUI({
	req(reactive_objects$filters)
	shinyWidgets::pickerInput("filter_picker", "Filters:", choices=reactive_objects$filters, multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
})


### Filter data
observe({
	req(reactive_objects$spatial_sel_data)
	data_sub=reactive_objects$spatial_sel_data
	if(!is.null(input$filter_picker)){
		for(n in 1:length(input$filter_picker)){
			data_sub = subset(data_sub, eval(parse(text=input$filter_picker[n])))
		}
	}
	data_sub$ResultMeasureValue=facToNum(data_sub$ResultMeasureValue)
	data_sub$ActivityStartDate=as.Date(data_sub$ActivityStartDate)
	#### Fill non-detects
	data_sub=within(data_sub, {
		ResultMeasureValue=ifelse(input$fill_nd == 'Detection limit' & ResultDetectionConditionText=='Not Detected' & is.na(ResultMeasureValue), DetectionQuantitationLimitMeasure.MeasureValue, ResultMeasureValue)
		ResultMeasureValue=ifelse(input$fill_nd == '1/2 detection limit' & ResultDetectionConditionText=='Not Detected' & is.na(ResultMeasureValue), DetectionQuantitationLimitMeasure.MeasureValue/2, ResultMeasureValue)
		ResultMeasureValue=ifelse(ResultDetectionConditionText=='Present Above Quantification Limit' & is.na(ResultMeasureValue), DetectionQuantitationLimitMeasure.MeasureValue, ResultMeasureValue)
	})
	test<<-data_sub
	reactive_objects$data_sub=data_sub
	print(dim(reactive_objects$data_sub))
})


### Data table
output$datatable=DT::renderDT({
	req(reactive_objects$data_sub)
	DT::datatable(data.frame(reactive_objects$data_sub),
		selection='none', rownames=FALSE,
		options = list(scrollY = '600px', paging = TRUE, scrollX=TRUE)
	)
})

### Analyses
sel_data=reactive(reactive_objects$data_sub)

figures=callModule(module=figuresMod, id='figures', sel_data)



}


shinyApp(ui = ui, server = server)


