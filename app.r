
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
					shinyWidgets::radioGroupButtons('select_au_type','Select AUs by:', choices=c('Map','List'), checkIcon = list(yes = icon("check"))),
					actionButton('select_data_au', 'Select data!', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('check'))
				),
				fluidRow(
					conditionalPanel(condition="input.select_au_type=='Map'",
						column(5, shinycssloaders::withSpinner(leafletOutput('au_map'),size=2, color="#0080b7"))
					),
					conditionalPanel(condition="input.select_au_type=='List'",
						column(3, uiOutput('aus_multiInput'))
					)
				)
				#fluidRow(
				#	column(3, shinyWidgets::multiInput('sitetypes', 'Site types:', choices=sitetypes, selected=c('Lake, Reservoir, Impoundment','Stream', 'Spring'))),
				#	column(3, shinyWidgets::multiInput('orgids', 'Organization IDs:', choices=orgids)),
				#	column(3, shinyWidgets::multiInput('samplemedia', 'Sample media:', choices=samplemedia, selected='Water')),
				#	column(3, shinyWidgets::multiInput('characteristicnames', 'Characteristic names:', choices=characteristicnames))
				#),
				
			),
			bsCollapsePanel(list(icon('filter'),"Additional filters"), value=2,
				column(6,
					uiOutput('filter_col'),
					uiOutput('filter'),
					actionButton('add_filter', 'Add filter', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('plus-circle'))
				),
				column(6,
					uiOutput('filter_picker')
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
if(file.exists(paste0(data_path,"/wqpDD_data.Rdata"))){
	load(paste0(data_path,"/wqpDD_data.Rdata"))
}else{
	showModal(modalDialog(easyClose=F, title="Looks like you're new here...", 
			  "You'll need to bulk download data for the application using the query tools in the 'Import WQP data' box. These tools will download data from WQP 
			  to the wqTools package path for the application to access in the future.", br(), br(), "Once you've downloaded data, you can proceed with data selections and analyses.
			  You only need to download data the first time you run the application on any R installation. You can also use the query tools to re-download data any time you'd like to update
			  the data included in the application.", br(), br(), "This query tool is purposely broad - you can further select data to analyze using the tools in the 'Filter data' box."))
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
		wqTools::buildMap() %>%
		clearGroup('Assessment units') %>% 
		addPolygons(data=wqTools::au_poly,group="Assessment units",smoothFactor=2,fillOpacity = 0.1, layerId=wqTools::au_poly$polyID,weight=3,color="orange", options = pathOptions(pane = "au_poly"),
			label=lapply(paste0(
				'<p>',
				"AU name: ", wqTools::au_poly$AU_NAME,
				'<br />', "AU ID: ", wqTools::au_poly$ASSESS_ID,
				'<br />', "AU type: ", wqTools::au_poly$AU_Type
			), HTML)
		) %>% 
		addMapPane("highlight", zIndex = 414) %>%
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


## Query data by AU
observeEvent(input$select_data_au, {
	if(input$select_au_type=='Map'){
		auid=as.vector(reactive_objects$sel_aus_map)
	}else{
		auid=as.vector(reactive_objects$sel_aus_mi)
	}
	reactive_objects$sel_data_au=subset(wqp_data, ASSESS_ID %in% auid)
	print(dim(reactive_objects$sel_data_au))

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


## Additional filters
### Select column to filter
output$filter_col=renderUI({
	req(reactive_objects$sel_data_au)
	selectInput('filter_col', 'Attribute:', choices=names(reactive_objects$sel_data_au))
})

### Build filter UI
output$filter=renderUI({
	req(input$filter_col)
	if(class(reactive_objects$sel_data_au[,input$filter_col])=='numeric' | class(reactive_objects$sel_data_au[,input$filter_col])=='Date'){
		sliderInput('filter', "Filter:", min=min(reactive_objects$sel_data_au[,input$filter_col], na.rm=T), max=max(reactive_objects$sel_data_au[,input$filter_col], na.rm=T), value=c(min(reactive_objects$sel_data_au[,input$filter_col], na.rm=T),max(reactive_objects$sel_data_au[,input$filter_col], na.rm=T)))
	}else{
		shinyWidgets::pickerInput("filter", "Filter:", choices=as.character(unique(reactive_objects$sel_data_au[,input$filter_col])), multiple=T, options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3", 'live-search'=TRUE))
	}
})

reactive_objects$filters=vector()

### Add filter
observeEvent(input$add_filter,{
	req(input$filter, input$filter_col)
	if(class(reactive_objects$sel_data_au[,input$filter_col])=='numeric' | class(reactive_objects$sel_data_au[,input$filter_col])=='Date'){
		if(class(reactive_objects$sel_data_au[,input$filter_col])=='numeric'){
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
	req(reactive_objects$sel_data_au)
	data_sub=reactive_objects$sel_data_au
	if(!is.null(input$filter_picker)){
		for(n in 1:length(input$filter_picker)){
			data_sub = subset(data_sub, eval(parse(text=input$filter_picker[n])))
		}
	}
	data_sub$ResultMeasureValue=facToNum(data_sub$ResultMeasureValue)
	data_sub$ActivityStartDate=as.Date(data_sub$ActivityStartDate)
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


