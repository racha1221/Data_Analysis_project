# ============================================================
# APPLICATION SHINY - EXPLORATION ET CLASSIFICATION
# Deux espaces : Fonctionnalites et Etude de cas
# ============================================================

packages <- c('shiny','bslib','ggplot2','dplyr','DT','readxl','recipes','caret','ranger','pROC')
absents <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if(length(absents)) stop('Installer : install.packages(c(',paste(sprintf('"%s"',absents),collapse=','),'))')
library(shiny)
library(ggplot2)
library(dplyr)

safe_mode <- function(x) {
  z <- x[!is.na(x)]
  if(!length(z)) return(NA_character_)
  names(sort(table(as.character(z)), decreasing=TRUE))[1]
}

# Import CSV robuste aux separateurs et decimales usuels
read_uploaded <- function(path, name, sep, decimal, header) {
  ext <- tolower(tools::file_ext(name))
  if(ext == 'csv') {
    d <- utils::read.table(path, header=header, sep=sep, dec=decimal,
                           stringsAsFactors=FALSE, check.names=FALSE,
                           fileEncoding='UTF-8-BOM', na.strings=c('', 'NA', 'N/A', 'NULL'))
  } else if(ext %in% c('xlsx','xls')) {
    d <- as.data.frame(readxl::read_excel(path))
  } else if(ext == 'rds') {
    d <- readRDS(path)
  } else stop('Format non pris en charge')
  if(!is.data.frame(d)) stop('Le fichier ne contient pas un tableau')
  names(d) <- make.unique(make.names(names(d)))
  d
}

# ---------- Identite visuelle et navigation ----------
observatory_css <- "
/* Quatre palettes explicites : espace × mode. */
body.obs-data:not(.obs-night) {
 --obs-bg:#edf5fa;--obs-panel:#ffffff;--obs-ink:#172b3a;--obs-muted:#43576a;
 --obs-border:#c3d6e2;--obs-field:#ffffff;--obs-accent:#086c79;
 --obs-nav-a:#087f91;--obs-nav-b:#0c9d91;--obs-active:#075d70;
 --obs-soft:#e3f2f5;--obs-link:#075e78;
}
body.obs-data.obs-night {
 --obs-bg:#0b1524;--obs-panel:#16283a;--obs-ink:#edf7ff;--obs-muted:#c2d3e3;
 --obs-border:#476174;--obs-field:#20394c;--obs-accent:#7ee4dc;
 --obs-nav-a:#092d41;--obs-nav-b:#12505a;--obs-active:#1c7780;
 --obs-soft:#1b394a;--obs-link:#8debe2;
}
body.obs-cinema:not(.obs-night) {
 --obs-bg:#fbf3eb;--obs-panel:#fffdf9;--obs-ink:#35212c;--obs-muted:#674e58;
 --obs-border:#d9c2b7;--obs-field:#ffffff;--obs-accent:#873d4e;
 --obs-nav-a:#63243c;--obs-nav-b:#92374c;--obs-active:#eac47d;
 --obs-soft:#f5e6dd;--obs-link:#81324a;
}
body.obs-cinema.obs-night {
 --obs-bg:#130f17;--obs-panel:#281e2d;--obs-ink:#fff1e5;--obs-muted:#dec7ce;
 --obs-border:#735564;--obs-field:#362839;--obs-accent:#f2cb88;
 --obs-nav-a:#211420;--obs-nav-b:#50243a;--obs-active:#eac47d;
 --obs-soft:#3b2836;--obs-link:#f3d394;
}
body.obs-data,body.obs-cinema {
 background:var(--obs-bg)!important;color:var(--obs-ink)!important;
 --bs-body-bg:var(--obs-bg);--bs-body-color:var(--obs-ink);
 --bs-secondary-color:var(--obs-muted);--bs-border-color:var(--obs-border);
 --bs-link-color:var(--obs-link);
}
body.obs-data nav.navbar,body.obs-cinema nav.navbar,
body.obs-data .navbar,body.obs-cinema .navbar {
 background:linear-gradient(110deg,var(--obs-nav-a),var(--obs-nav-b))!important;
 border-bottom:2px solid var(--obs-border)!important;
}
body.obs-data .navbar .navbar-brand,body.obs-cinema .navbar .navbar-brand,
body.obs-data .navbar .nav-link,body.obs-cinema .navbar .nav-link{
 color:#fffaf3!important;
}
body.obs-data .navbar .nav-link.active {
 background:var(--obs-active)!important;color:#ffffff!important;
 border-radius:10px;font-weight:750;
}
body.obs-cinema .navbar .nav-link.active {
 background:var(--obs-active)!important;color:#321c29!important;
 border-radius:10px;font-weight:750;
}
body.obs-data .navbar .nav-link:not(.active):hover,
body.obs-cinema .navbar .nav-link:not(.active):hover {
 background:#ffffff24!important;border-radius:10px;
}
body.obs-data .bslib-sidebar-layout,body.obs-cinema .bslib-sidebar-layout,
body.obs-data .bslib-sidebar-layout>.sidebar,body.obs-cinema .bslib-sidebar-layout>.sidebar,
body.obs-data .card,body.obs-cinema .card,
body.obs-data .bslib-card,body.obs-cinema .bslib-card,
body.obs-data .container-fluid,body.obs-cinema .container-fluid,
body.obs-data .tab-content,body.obs-cinema .tab-content {
 background-color:var(--obs-panel)!important;color:var(--obs-ink)!important;
 border-color:var(--obs-border)!important;
}
body.obs-data .bslib-sidebar-layout,body.obs-cinema .bslib-sidebar-layout{min-height:75vh;}
body.obs-data main,body.obs-cinema main{min-width:0;}
body.obs-data .card,body.obs-cinema .card{border-radius:15px;}
/* Texte courant : ne jamais imposer une couleur nocturne en mode jour. */
body.obs-data :is(h1,h2,h3,h4,h5,h6,p,label,legend,li,dt,dd,small,strong,span),
body.obs-cinema :is(h1,h2,h3,h4,h5,h6,p,label,legend,li,dt,dd,small,strong,span){
 color:inherit;
}
body.obs-data :is(h1,h2,h3,h4,h5,h6),body.obs-cinema :is(h1,h2,h3,h4,h5,h6){
 color:var(--obs-ink)!important;
}
body.obs-data :is(.text-muted,.help-block,.form-text,.text-secondary),
body.obs-cinema :is(.text-muted,.help-block,.form-text,.text-secondary){
 color:var(--obs-muted)!important;
}
body.obs-data :is(.form-control,.form-select,select,input,textarea,.selectize-input),
body.obs-cinema :is(.form-control,.form-select,select,input,textarea,.selectize-input){
 color:var(--obs-ink)!important;background-color:var(--obs-field)!important;
 border-color:var(--obs-border)!important;
}
body.obs-data input::placeholder,body.obs-cinema input::placeholder,
body.obs-data textarea::placeholder,body.obs-cinema textarea::placeholder {
 color:var(--obs-muted)!important;opacity:1;
}
body.obs-data .selectize-dropdown,body.obs-cinema .selectize-dropdown,
body.obs-data .selectize-dropdown-content,body.obs-cinema .selectize-dropdown-content{
 color:var(--obs-ink)!important;background:var(--obs-panel)!important;
}
body.obs-data .selectize-dropdown .active,body.obs-cinema .selectize-dropdown .active{
 color:var(--obs-ink)!important;background:var(--obs-soft)!important;
}
body.obs-data :is(.btn-outline-secondary,.btn-outline-primary),
body.obs-cinema :is(.btn-outline-secondary,.btn-outline-primary){
 color:var(--obs-accent)!important;background:var(--obs-panel)!important;
 border-color:var(--obs-accent)!important;
}
body.obs-data :is(.btn-outline-secondary,.btn-outline-primary):hover,
body.obs-cinema :is(.btn-outline-secondary,.btn-outline-primary):hover{
 color:var(--obs-panel)!important;background:var(--obs-accent)!important;
}
body.obs-data .btn-primary,body.obs-cinema .btn-primary{
 background:var(--obs-nav-a)!important;border-color:var(--obs-nav-a)!important;
 color:#ffffff!important;
}
body.obs-data .btn-primary *,body.obs-cinema .btn-primary *{color:inherit!important;}
body.obs-data :is(.table,.table th,.table td,.dataTables_wrapper,.dataTables_info,.dataTables_paginate),
body.obs-cinema :is(.table,.table th,.table td,.dataTables_wrapper,.dataTables_info,.dataTables_paginate){
 color:var(--obs-ink)!important;background:var(--obs-panel)!important;
}
body.obs-data .table-striped>tbody>tr:nth-of-type(odd)>*,
body.obs-cinema .table-striped>tbody>tr:nth-of-type(odd)>*{
 background:var(--obs-soft)!important;color:var(--obs-ink)!important;
}
body.obs-data a:not(.btn):not(.nav-link),body.obs-cinema a:not(.btn):not(.nav-link){
 color:var(--obs-link);
}
body.obs-data pre,body.obs-cinema pre,
body.obs-data .shiny-text-output,body.obs-cinema .shiny-text-output{
 color:var(--obs-ink)!important;background:var(--obs-panel)!important;
}
.obs-upload,.obs-save-panel {
 background:var(--obs-soft)!important;border:1px solid var(--obs-border)!important;
 border-radius:14px;padding:18px;margin:12px 0 22px;
 color:var(--obs-ink)!important;
}
.obs-upload{max-width:820px;}
.obs-upload .shiny-input-container,.obs-save-panel .shiny-input-container{width:100%!important;}
.obs-upload strong,.obs-save-panel strong{display:block;margin-bottom:10px;}
.obs-upload .form-group{margin-bottom:0;}
.obs-heading{color:var(--obs-accent)!important;font-weight:800;letter-spacing:.09em;font-size:12px;}
.obs-empty{margin:40px auto;padding:35px;max-width:640px;text-align:center;
 border:1px dashed var(--obs-border);border-radius:20px;}
/* Identité DATA : trame statistique, indicateurs et axes. */
body.obs-data .bslib-sidebar-layout>.sidebar{
 background:var(--obs-soft)!important;
 border-right:1px solid var(--obs-border)!important;
}
body.obs-data main{
 background-image:linear-gradient(to right,transparent 23px,#5b9db00d 24px),
 linear-gradient(to bottom,transparent 23px,#5b9db00d 24px)!important;
 background-size:24px 24px!important;
}
body.obs-data .obs-heading::before{content:'▥  ';color:var(--obs-accent);}
body.obs-data .card,body.obs-data .bslib-card{
 border:1px solid var(--obs-border)!important;box-shadow:0 5px 20px #0b506015;
}
/* Identité CINÉMA : affiches, dorures et contrastes propres au jour et à la nuit. */
body.obs-cinema .bslib-sidebar-layout>.sidebar{
 background:var(--obs-soft)!important;border-right:1px solid var(--obs-border)!important;
}
.cinema-hero {
 padding:30px;border-radius:18px;
 background:radial-gradient(circle at 85% 0%,#9a4a53 0%,transparent 43%),
 linear-gradient(115deg,#2b1828,#4e202c 70%,#1c1823);
 border:1px solid #c29a68;box-shadow:0 12px 30px #08060a35;margin-bottom:22px;
}
body.obs-cinema .cinema-hero h2,body.obs-cinema .cinema-hero p,
body.obs-cinema .cinema-hero .cinema-kicker{color:#fff0d8!important;}
body.obs-cinema .cinema-hero h2{font-family:Georgia,serif;font-size:clamp(26px,3vw,42px);}
body.obs-cinema .cinema-hero .cinema-kicker{
 font-weight:800;letter-spacing:.16em;color:#f7d58f!important;font-size:12px;margin-bottom:15px;
}
@media(max-width:750px){.cinema-hero{padding:18px;}}

/* Correctif prioritaire de navigation : CSS natif, sans dépendance JavaScript.
   :has() lit l'onglet réellement actif dans la barre Bootstrap. */
html body .navbar,
html body nav.navbar,
html body .bslib-page-navbar > .navbar,
html body .bslib-page-navbar > nav,
html body .navbar.navbar-default {
  --bs-navbar-color:#ffffff;
  --bs-navbar-active-color:#ffffff;
  --bs-navbar-brand-color:#ffffff;
  --bs-navbar-hover-color:#ffffff;
  --bs-navbar-brand-hover-color:#ffffff;
  --bs-bg-opacity:1;
  background:#12a895!important;
  background-color:#12a895!important;
  background-image:none!important;
  color:#ffffff!important;
}
html body:has(.navbar [data-value='etude'].active) .navbar,
html body:has(.navbar [data-value='etude'].active) nav.navbar {
  background:#70263e!important;background-color:#70263e!important;
}
html body.obs-night .navbar,html body.obs-night nav.navbar {
  background:#0d3549!important;background-color:#0d3549!important;
}
html body.obs-night:has(.navbar [data-value='etude'].active) .navbar,
html body.obs-night:has(.navbar [data-value='etude'].active) nav.navbar {
  background:#241522!important;background-color:#241522!important;
}
html body .navbar :is(.navbar-brand,.navbar-brand span,.nav-link,.nav-link span,.navbar-text,.nav-item) {
  color:#ffffff!important;text-shadow:none!important;opacity:1!important;
}
html body .navbar .nav-link:not(.active) {background:transparent!important;}
html body .navbar .nav-link.active {
  background:#075b70!important;color:#ffffff!important;
  border-radius:10px;
}
html body:has(.navbar [data-value='etude'].active) .navbar .nav-link.active {
  background:#e9bf78!important;color:#2d1b25!important;
}
html body.obs-night:not(:has(.navbar [data-value='etude'].active)) .navbar .nav-link.active {
  background:#126a7b!important;color:#ffffff!important;
}
html body .navbar .nav-link.active * {color:inherit!important;}

"


# ---------- Interface ----------
ui <- bslib::page_navbar(
  title='◉ Data Observatory | Classification',
  theme=bslib::bs_theme(bootswatch='flatly', version=5, primary='#087f8c'),
  header=tagList(tags$head(tags$style(HTML(observatory_css)), tags$script(HTML("(function(){\n  function paintNavbar(){ /* Style gere par le CSS. */ }
  function applySection(section){\n    var cinema = section === 'etude';\n    document.documentElement.classList.toggle('obs-cinema',cinema);\n    document.documentElement.classList.toggle('obs-data',!cinema);\n    document.body.classList.toggle('obs-cinema',cinema);\n    document.body.classList.toggle('obs-data',!cinema);\n    paintNavbar();\n  }\n  function register(){\n    if(!window.Shiny || !Shiny.addCustomMessageHandler) return;\n    Shiny.addCustomMessageHandler('obs-section',applySection);\n  }\n  // Enregistrer sans attendre shiny:connected : cet evenement peut deja avoir eu lieu.\n  register();\n  document.addEventListener('shiny:connected',register);\n  document.documentElement.classList.add('obs-night');\n  document.addEventListener('DOMContentLoaded',function(){\n    document.documentElement.classList.add('obs-night');\n    document.body.classList.add('obs-night');\n    applySection('fonctionnalites');\n    paintNavbar();\n    // La navigation Bootstrap change le panneau actif : le theme suit directement l'onglet.\n    document.addEventListener('shown.bs.tab',function(ev){\n      var target=ev.target;\n      if(!target) return;\n      var value=target.getAttribute('data-value') || '';\n      if(value==='etude' || value==='fonctionnalites') applySection(value);\n    });\n    document.addEventListener('click',function(ev){\n      var tab=ev.target.closest('[data-value=\"etude\"], [data-value=\"fonctionnalites\"]');\n      if(tab) applySection(tab.getAttribute('data-value'));\n    });\n  });\n})();"))),
    div(class='container-fluid pt-3',
      div(class='d-flex justify-content-between align-items-center flex-wrap gap-2',
        div(tags$span(class='obs-heading','EXPLORE · MODEL · UNDERSTAND'),
            p('Explorer, preparer, modeliser et documenter une classification binaire.')),
        tags$span(class='obs-heading', style='font-size:19px', icon('chart-line'))
      )
    )
  ),
  id='espace', selected='fonctionnalites',


  bslib::nav_panel('◈ Fonctionnalites', value='fonctionnalites',
    bslib::layout_sidebar(
      sidebar=bslib::sidebar(width=280, title='Outils interactifs',
        radioButtons('page_f', NULL, choices=c(
          'Importation et types'='import',
          'Qualite des donnees'='quality',
          'Analyse univariee'='uni',
          'Analyse bivariee'='bi',
          'Preparation et modelisation'='model',
          'Evaluation et importance'='eval'
        ), selected='import'),
        hr(),
        div(class='obs-save-panel',
          tags$strong('Sessions de travail'),
          downloadButton('save_session','Sauvegarder cette analyse',class='btn btn-outline-primary w-100'),
          tags$div(style='height:9px'),
          fileInput('restore_session','Reprendre une sauvegarde (.rds)',
                    accept='.rds',buttonLabel='Choisir une sauvegarde…',
                    placeholder='Aucune sauvegarde',width='100%'),
          actionButton('new_session','Nouvelle analyse',class='btn btn-outline-secondary w-100')
        )
      ),
      tagList(
        shiny::conditionalPanel(
          condition = "input.page_f === 'import'",
          div(class='obs-upload',
            tags$strong('Importer un jeu de donnees'),
            p(class='text-muted', 'Selectionnez un fichier directement dans vos dossiers Windows.'),
            fileInput('file', 'Fichier CSV, Excel ou RDS',
              accept=c('.csv','.xlsx','.xls','.rds'),
              buttonLabel='Parcourir mes dossiers…',
              placeholder='Aucun fichier selectionne', width='100%'),
            textOutput('upload_status')
          )
        ),
        uiOutput('page_f_content')
      )
    )
  ),
  bslib::nav_panel('🎬 Etude de cas', value='etude',
    bslib::layout_sidebar(
      sidebar=bslib::sidebar(width=280, title='🎞 Studio / Etude de cas',
        radioButtons('page_c', NULL, choices=c(
          'Presentation'='intro',
          'Diagnostic initial'='diagnostic',
          'Exploration'='explore',
          'Choix methodologiques'='methods',
          'Modeles et performances'='results',
          'Interpretation et conclusion'='conclusion'
        ), selected='intro'),
        hr(),
        tags$p('Parcours de notre étude de cas.')

      ),
      uiOutput('page_c_content')
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$espace, {
    session$sendCustomMessage('obs-section',input$espace)
  },ignoreInit=FALSE)
  observeEvent(input$file, {
    req(input$file)
    showNotification(paste('Fichier selectionne :',input$file$name),type='message')
  })

  output$upload_status <- renderText({
    if(isTRUE(blank_session()) || (is.null(input$file) && is.null(restored_data()))) 'Aucun fichier importe. Choisissez un jeu de donnees pour commencer.'
    else if(!is.null(restored_data())) 'Analyse restauree depuis une sauvegarde.'
    else paste('Fichier selectionne :', input$file$name)
  })

  restored_data <- reactiveVal(NULL)
  blank_session <- reactiveVal(FALSE)
  restored_types <- reactiveVal(NULL)
  restored_models <- reactiveVal(NULL)
  restored_settings <- reactiveVal(NULL)
  uploaded <- reactive({
    req(input$file)
    tryCatch(read_uploaded(input$file$datapath,input$file$name,
      if(is.null(input$sep)) ',' else input$sep,
      if(is.null(input$decimal)) '.' else input$decimal,
      if(is.null(input$header)) TRUE else input$header),
      error=function(e) validate(need(FALSE,paste('Erreur de lecture :',conditionMessage(e)))))
  })
  original <- reactive({
    if(isTRUE(blank_session())) req(FALSE)
    if(!is.null(restored_data())) return(restored_data())
    req(input$file, message='Importez un fichier dans la page Importation pour commencer.')
    uploaded()
  })
  observeEvent(input$file, {
    blank_session(FALSE)
    restored_data(NULL)
    restored_types(NULL)
    restored_models(NULL)
    restored_settings(NULL)
  }, ignoreInit=TRUE)
  observeEvent(input$restore_session, {
    req(input$restore_session)
    tryCatch({
      z <- readRDS(input$restore_session$datapath)
      if(!is.list(z) || !identical(z$format,'data-observatory-session-v1') ||
         !is.data.frame(z$data)) stop('Fichier de sauvegarde non reconnu.')
      blank_session(FALSE)
      restored_data(z$data)
      restored_types(z$types)
      restored_models(z$models)
      restored_settings(z$settings)
      updateRadioButtons(session,'page_f',selected='import')
      showNotification('Sauvegarde restauree. Vous pouvez reprendre votre analyse.',type='message')
    },error=function(e) showNotification(conditionMessage(e),type='error',duration=10))
  })
  observeEvent(input$new_session, {
    restored_data(NULL)
    restored_types(NULL)
    restored_models(NULL)
    restored_settings(NULL)
    # Nouvelle session : l ancien fichier n est plus utilise, meme si le navigateur le conserve
    blank_session(TRUE)
    updateRadioButtons(session,'page_f',selected='import')
    showNotification('Nouvelle analyse : importez un fichier.',type='message')
  })
  output$save_session <- downloadHandler(
    filename=function() paste0('data_observatory_',format(Sys.time(),'%Y%m%d_%H%M%S'),'.rds'),
    content=function(file) {
      req(original())
      d <- original()
      settings <- list(target=input$target,positive=input$positive,
        split=input$split,seed=input$seed,folds=input$folds,
        missing=input$missing,outliers=input$outliers,
        normalize=input$normalize,balance=input$balance,
        glm=input$glm,rf=input$rf,sep=input$sep,decimal=input$decimal,header=input$header)
      types <- setNames(lapply(names(d),function(n) input[[paste0('type_',n)]]),names(d))
      result <- tryCatch(model_result(),error=function(e) NULL)
      saveRDS(list(format='data-observatory-session-v1',
        saved_at=Sys.time(),data=d,types=types,settings=settings,models=result),file)
    }
  )
  observeEvent(original(), {
    n <- names(original())
    target_default <- if(!is.null(restored_settings()) &&
      !is.null(restored_settings()$target) &&
      restored_settings()$target %in% n) restored_settings()$target else tail(n,1)
    updateSelectInput(session,'target',choices=n,selected=target_default)
    updateSelectInput(session,'var_uni',choices=n,selected=n[1])
    updateSelectInput(session,'var_x',choices=n,selected=n[1])
    updateSelectInput(session,'var_y',choices=n,selected=n[min(2,length(n))])
    st <- restored_settings()
    if(!is.null(st)) {
      if(!is.null(st$split)) updateSliderInput(session,'split',value=st$split)
      if(!is.null(st$seed)) updateNumericInput(session,'seed',value=st$seed)
      if(!is.null(st$folds)) updateNumericInput(session,'folds',value=st$folds)
      if(!is.null(st$missing)) updateSelectInput(session,'missing',selected=st$missing)
      if(!is.null(st$normalize)) updateCheckboxInput(session,'normalize',value=st$normalize)
      if(!is.null(st$glm)) updateCheckboxInput(session,'glm',value=st$glm)
      if(!is.null(st$rf)) updateCheckboxInput(session,'rf',value=st$rf)
    }
  })
  typed <- reactive({
    d <- original()
    for(n in names(d)) {
      kind <- input[[paste0('type_',n)]]
      if(identical(kind,'quant')) d[[n]] <- suppressWarnings(as.numeric(as.character(d[[n]])))
      if(identical(kind,'qual')) d[[n]] <- factor(d[[n]])
      if(is.character(d[[n]])) d[[n]] <- factor(d[[n]])
      if(is.logical(d[[n]])) d[[n]] <- factor(d[[n]])
    }
    d
  })
  observeEvent(typed(), {
    req(input$target)
    if(input$target %in% names(typed())) {
      z <- typed()[[input$target]]
      opts <- sort(unique(as.character(z[!is.na(z)])))
      st <- restored_settings()
      updateSelectInput(session,'positive',choices=opts,
        selected=if(!is.null(st) && st$positive %in% opts) st$positive else opts[1])
    }
  })
  quality <- reactive({
    d <- typed()
    data.frame(
      Variable=names(d),
      Type=vapply(d,function(x) if(is.numeric(x)) 'Quantitative' else 'Qualitative',character(1)),
      Modalites_ou_valeurs=vapply(d,function(x) length(unique(x[!is.na(x)])),integer(1)),
      Manquantes=vapply(d,function(x) sum(is.na(x)),integer(1)),
      Pourcentage_NA=round(vapply(d,function(x) 100*mean(is.na(x)),numeric(1)),2),
      Outliers_IQR=vapply(d,function(x) {
        if(!is.numeric(x) || sum(!is.na(x))<4) return(NA_integer_)
        q <- quantile(x,c(.25,.75),na.rm=TRUE)
        i <- q[2]-q[1]
        as.integer(sum(x<q[1]-1.5*i | x>q[2]+1.5*i,na.rm=TRUE))
      },integer(1))
    )
  })
  output$quality_table <- DT::renderDT(DT::datatable(quality(),options=list(scrollX=TRUE,pageLength=12)))
  output$preview <- DT::renderDT(DT::datatable(head(typed(),100),options=list(scrollX=TRUE,pageLength=10)))
  output$summary_data <- renderPrint(summary(typed()))
  output$types <- renderUI({
    d <- original()
    tagList(lapply(names(d),function(n) {
      selectInput(paste0('type_',n),n,
                  choices=c('Automatique'='auto','Quantitative'='quant','Qualitative'='qual'),
                  selected=if(!is.null(restored_types()) && !is.null(restored_types()[[n]])) restored_types()[[n]] else 'auto')
    }))
  })
  output$has_data <- reactive({
    if(isTRUE(blank_session())) return(FALSE)
    !is.null(restored_data()) || !is.null(input$file)
  })
  outputOptions(output,'has_data',suspendWhenHidden=FALSE)
  output$dataset_info <- renderUI({
    d <- typed()
    div(class='row',
      div(class='col-md-4',bslib::value_box('Observations',nrow(d),theme='primary')),
      div(class='col-md-4',bslib::value_box('Variables',ncol(d),theme='info')),
      div(class='col-md-4',bslib::value_box('Cellules manquantes',sum(is.na(d)),theme='warning'))
    )
  })
  output$uni_plot <- renderPlot({
    req(input$var_uni)
    d <- typed(); req(input$var_uni %in% names(d)); x <- input$var_uni
    if(is.numeric(d[[x]])) {
      ggplot(d,aes(x=.data[[x]]))+geom_histogram(bins=25,fill='#2677a5',color='white')+
        geom_vline(xintercept=median(d[[x]],na.rm=TRUE),linetype=2,color='red')+
        labs(title=paste('Distribution de',x),x=x,y='Effectif')+theme_minimal()
    } else {
      ggplot(d,aes(x=.data[[x]]))+geom_bar(fill='#2677a5')+
        labs(title=paste('Repartition de',x),x=x,y='Effectif')+
        theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
    }
  })
  output$uni_stats <- renderPrint({
    req(input$var_uni)
    x <- typed()[[input$var_uni]]
    print(summary(x))
    cat('\nValeurs manquantes :',sum(is.na(x)),'\n')
    if(is.numeric(x)) cat('Ecart-type :',sd(x,na.rm=TRUE),'\n')
    else print(sort(table(x,useNA='ifany'),decreasing=TRUE))
  })
  output$bi_plot <- renderPlot({
    req(input$var_x,input$var_y)
    validate(need(input$var_x != input$var_y,'Selectionner deux variables differentes.'))
    d <- typed(); x <- input$var_x; y <- input$var_y
    if(is.numeric(d[[x]]) && is.numeric(d[[y]])) {
      ggplot(d,aes(x=.data[[x]],y=.data[[y]]))+geom_point(alpha=.65)+
        geom_smooth(method='lm',se=FALSE)+theme_minimal()
    } else if(is.numeric(d[[x]]) || is.numeric(d[[y]])) {
      q <- if(is.numeric(d[[x]])) x else y
      f <- if(identical(q,x)) y else x
      ggplot(d,aes(x=.data[[f]],y=.data[[q]]))+geom_boxplot(fill='#a4cce5')+
        theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
    } else {
      ggplot(d,aes(x=.data[[x]],fill=.data[[y]]))+geom_bar(position='fill')+
        labs(y='Proportion',fill=y)+theme_minimal()+
        theme(axis.text.x=element_text(angle=45,hjust=1))
    }
  })
  output$bi_stats <- renderPrint({
    req(input$var_x,input$var_y)
    validate(need(input$var_x != input$var_y,'Choisir deux variables differentes.'))
    d <- typed(); a <- d[[input$var_x]]; b <- d[[input$var_y]]
    ok <- complete.cases(a,b); a <- a[ok]; b <- b[ok]
    validate(need(length(a)>3,'Pas assez de paires completes.'))
    if(is.numeric(a) && is.numeric(b)) {
      cat('Pearson :',cor(a,b),'\nSpearman :',cor(a,b,method='spearman'))
    } else if(is.numeric(a) || is.numeric(b)) {
      q <- if(is.numeric(a)) a else b
      f <- factor(if(is.numeric(a)) b else a)
      validate(need(nlevels(f)>1,'Une seule modalite disponible.'))
      print(summary(aov(q~f)))
    } else {
      tab <- table(a,b)
      validate(need(min(dim(tab))>1,'Table de contingence insuffisante.'))
      test <- suppressWarnings(chisq.test(tab))
      print(test)
      cat('\nV de Cramer :',sqrt(unname(test$statistic)/(sum(tab)*(min(dim(tab))-1))))
      if(any(test$expected<5)) cat('\nAttention : effectifs theoriques faibles ; chi-deux a interpreter avec prudence.')
    }
  })

  # ---------- Fonction de modelisation reutilisable ----------
  # Les recettes sont re-estimees dans chaque pli de validation croisee.
  fit_models <- function(d,target,positive,split=.8,seed=42,folds=5,
                         missing='impute',outliers='keep',normalize=TRUE,
                         balance='none',do_glm=TRUE,do_rf=TRUE) {
    if(!target %in% names(d)) stop('Cible inconnue.')
    if(anyNA(d[[target]])) stop('La cible contient des NA : nettoyer la cible avant de modeliser.')
    if(length(unique(d[[target]]))!=2) stop('La classification exige exactement deux classes.')
    if(!positive %in% as.character(unique(d[[target]]))) stop('Classe positive inconnue.')
    if(!do_glm && !do_rf) stop('Selectionner au moins un modele.')
    d[[target]] <- factor(as.character(d[[target]]),levels=c(positive,setdiff(as.character(unique(d[[target]])),positive)))
    # Eliminer les identifiants evidents et colonnes constantes
    pred <- setdiff(names(d),target)
    pred <- pred[vapply(d[pred],function(z) length(unique(z[!is.na(z)]))>1,logical(1))]
    pred <- pred[!grepl('^(id|identifiant|index|modele)$',pred,ignore.case=TRUE)]
    if(!length(pred)) stop('Aucune variable explicative exploitable.')
    d <- d[,c(target,pred),drop=FALSE]
    if(missing=='drop') d <- d[complete.cases(d),,drop=FALSE]
    if(nrow(d)<20) stop('Au moins 20 observations completes sont requises pour cette configuration.')
    set.seed(seed)
    ix <- caret::createDataPartition(d[[target]],p=split,list=FALSE)
    tr <- d[ix,,drop=FALSE]; te <- d[-ix,,drop=FALSE]
    if(length(unique(tr[[target]]))<2 || length(unique(te[[target]]))<2)
      stop('Chaque partition doit contenir les deux classes.')
    if(min(table(tr[[target]]))<folds) stop('La classe minoritaire est trop petite pour ce nombre de plis.')
    form <- reformulate(pred,response=target)
    rec <- recipes::recipe(form,data=tr)
    if(missing=='impute') rec <- rec |>
      recipes::step_impute_median(recipes::all_numeric_predictors()) |>
      recipes::step_impute_mode(recipes::all_nominal_predictors())
    if(outliers=='cap') {
      # Winsorisation via une fonction personnalisee apprise dans chaque pli :
      # ici on utilise une transformation IQR preparee en amont sur le train uniquement.
      # Pour la CV, l'option est desactivee avec avertissement explicite.
      warning('Winsorisation IQR non appliquee : necessite une etape de recette personnalisee par pli.')
    }
    rec <- rec |>
      recipes::step_novel(recipes::all_nominal_predictors()) |>
      recipes::step_unknown(recipes::all_nominal_predictors()) |>
      recipes::step_dummy(recipes::all_nominal_predictors()) |>
      recipes::step_zv(recipes::all_predictors())
    if(normalize) rec <- rec |> recipes::step_normalize(recipes::all_numeric_predictors())
    if(balance!='none') warning('Reequilibrage non applique dans cette version : necessite themis pour rester interne aux plis.')
    # train() avec recipe garantit le pretraitement dans chaque pli
    ctrl <- caret::trainControl(method='cv',number=folds,classProbs=TRUE,
      summaryFunction=caret::twoClassSummary,savePredictions='final')
    set.seed(seed)
    fits <- list()
    if(do_glm) fits[['Logistique']] <- caret::train(rec,data=tr,method='glm',family=binomial(),
      trControl=ctrl,metric='ROC')
    if(do_rf) fits[['Random Forest']] <- caret::train(rec,data=tr,method='ranger',
      importance='permutation',tuneLength=3,trControl=ctrl,metric='ROC')
    out <- lapply(names(fits),function(n) {
      fit <- fits[[n]]
      prob <- predict(fit,newdata=te,type='prob')[[positive]]
      negative <- setdiff(levels(te[[target]]),positive)
      pred <- factor(ifelse(prob>=.5,positive,negative),levels=levels(te[[target]]))
      cm <- caret::confusionMatrix(pred,te[[target]],positive=positive)
      roc <- pROC::roc(response=te[[target]],predictor=prob,
                       levels=c(negative,positive),direction='<',quiet=TRUE)
      pr <- unname(cm$byClass['Precision']); re <- unname(cm$byClass['Recall'])
      f1 <- if(is.finite(pr+re) && pr+re>0) 2*pr*re/(pr+re) else NA_real_
      list(name=n,fit=fit,cm=cm,roc=roc,
        metrics=data.frame(Modele=n,Precision=pr,Recall=re,F1=f1,
          AUC=as.numeric(pROC::auc(roc)),Accuracy=unname(cm$overall['Accuracy'])),
        roc_df=data.frame(FPR=1-roc$specificities,TPR=roc$sensitivities,Modele=n),
        importance=tryCatch({
          v <- caret::varImp(fit)$importance
          data.frame(Variable=rownames(v),Importance=v[[1]],Modele=n)
        },error=function(e) data.frame()))
    })
    names(out) <- names(fits)
    list(models=out,metrics=dplyr::bind_rows(lapply(out,`[[`,'metrics')),
         rocs=dplyr::bind_rows(lapply(out,`[[`,'roc_df')),
         importance=dplyr::bind_rows(lapply(out,`[[`,'importance')),
         ntrain=nrow(tr),ntest=nrow(te),train_distribution=table(tr[[target]]),
         test_distribution=table(te[[target]]))
  }
  trained_result <- eventReactive(input$train, {
    req(input$target,input$positive)
    withProgress(message='Entrainement et validation croisee...',value=.2,{
      r <- fit_models(typed(),input$target,input$positive,
        split=input$split,seed=input$seed,folds=input$folds,
        missing=input$missing,outliers=input$outliers,normalize=input$normalize,
        balance=input$balance,do_glm=input$glm,do_rf=input$rf)
      incProgress(.8)
      r
    })
  })
  model_result <- reactive({
    if(!is.null(restored_models())) return(restored_models())
    trained_result()
  })
  observeEvent(input$train, {restored_models(NULL)},ignoreInit=TRUE)
  output$train_status <- renderPrint({
    req(model_result())
    cat('Entrainement termine\nTrain :',model_result()$ntrain,
        '\nTest :',model_result()$ntest,'\n')
    print(model_result()$train_distribution)
  })
  output$metrics <- DT::renderDT({
    req(model_result())
    DT::datatable(model_result()$metrics,rownames=FALSE,options=list(dom='t')) |>
      DT::formatRound(c('Precision','Recall','F1','AUC','Accuracy'),3)
  })
  plot_rocs <- function(r) {
    ggplot(r$rocs,aes(FPR,TPR,color=Modele))+geom_line(linewidth=1.1)+
      geom_abline(slope=1,intercept=0,linetype=2,color='grey50')+
      coord_equal()+theme_minimal()+labs(title='Courbes ROC sur le jeu de test',
        x='1 - Specificite',y='Sensibilite')
  }
  plot_importance <- function(r) {
    z <- r$importance
    if(!nrow(z)) return(ggplot()+theme_void()+labs(title='Importance non disponible'))
    z <- z |> group_by(Modele) |> slice_max(Importance,n=12,with_ties=FALSE) |> ungroup()
    ggplot(z,aes(reorder(Variable,Importance),Importance))+geom_col(fill='#2677a5')+
      coord_flip()+facet_wrap(~Modele,scales='free_y')+theme_minimal()+
      labs(x=NULL,title='Importance des variables apres encodage')
  }
  output$roc <- renderPlot({req(model_result());print(plot_rocs(model_result()))})
  output$importance <- renderPlot({req(model_result());print(plot_importance(model_result()))})
  output$confusions <- renderPrint({
    req(model_result())
    lapply(model_result()$models,function(x) x$cm$table)
  })

  # ÉTUDE DE CAS : aucune importation, aucun calcul déclenché par le visiteur.
  # Les membres du groupe complètent directement les pages ci-dessous.

  # ---------- Navigation dynamique ----------
  output$page_f_content <- renderUI({
    if(!identical(input$page_f,'import') && isTRUE(blank_session()))
      return(div(class='obs-empty',h3('📊 Aucune analyse ouverte'),
        p('Importez un jeu de donnees ou restaurez une sauvegarde pour acceder aux analyses.')))
    if(!identical(input$page_f,'import') && is.null(restored_data()) && is.null(input$file))
      return(div(class='obs-empty',h3('📊 Aucun jeu de donnees'),
        p('Rendez-vous dans Importation pour choisir un fichier.')))
    switch(input$page_f,
      import=tagList(
        h2('Importer et comprendre les donnees'),
        p('Aucun jeu de donnees n est charge par defaut. Importez votre fichier ; les types peuvent ensuite etre ajustes.'),
        fluidRow(column(4,selectInput('sep','Separateur CSV',choices=c('Virgule'= ',', 'Point-virgule'=';', 'Tabulation'='\t'),selected=',')),
                 column(4,selectInput('decimal','Separateur decimal',choices=c('Point'='.', 'Virgule'=','))),
                 column(4,checkboxInput('header','En-tetes dans le CSV',TRUE))),
        conditionalPanel(condition="output.has_data",
          h4('Vue d ensemble'),uiOutput('dataset_info'),
        h4('Apercu des observations'),DT::DTOutput('preview'),
        h4('Choix de la cible'),selectInput('target','Variable a predire',choices=NULL),
        h4('Types des variables'),uiOutput('types'))
      ),
      quality=tagList(
        h2('Diagnostic de qualite'),
        p('Types, nombre de modalites, taux de valeurs manquantes et valeurs atypiques selon la regle 1,5 x IQR.'),
        DT::DTOutput('quality_table'),
        h4('Resume statistique'),verbatimTextOutput('summary_data'),
        p('Une valeur atypique statistique n est pas necessairement une erreur : son traitement doit etre justifie.')
      ),
      uni=tagList(
        h2('Analyse univariee'),
        selectInput('var_uni','Variable',choices=NULL),
        plotOutput('uni_plot',height='400px'),
        h4('Statistiques descriptives'),verbatimTextOutput('uni_stats')
      ),
      bi=tagList(
        h2('Analyse bivariee'),
        fluidRow(column(6,selectInput('var_x','Variable X',choices=NULL)),
                 column(6,selectInput('var_y','Variable Y',choices=NULL))),
        plotOutput('bi_plot',height='430px'),
        h4('Mesure de relation'),verbatimTextOutput('bi_stats'),
        p('Quantitative/quantitative : Pearson et Spearman. Mixte : ANOVA. Qualitative/qualitative : Chi-deux et V de Cramer.')
      ),
      model=tagList(
        h2('Preparation et entrainement'),
        p('Les transformations sont estimees dans chaque pli de validation croisee par recipes/caret.'),
        selectInput('positive','Classe positive',choices=NULL),
        fluidRow(
          column(6,
            selectInput('missing','Valeurs manquantes',c('Imputation mediane / mode'='impute','Supprimer lignes incompletes'='drop')),
            selectInput('outliers','Outliers',c('Conserver'='keep')),
            checkboxInput('normalize','Centrer-reduire',TRUE),
            selectInput('balance','Reequilibrage',c('Aucun (autres methodes a integrer)'='none'))
          ),
          column(6,
            sliderInput('split','Part apprentissage',min=.6,max=.9,value=.8,step=.05),
            numericInput('seed','Graine',42,min=1),
            numericInput('folds','Plis de validation croisee',5,min=2,max=10),
            checkboxInput('glm','Regression logistique',TRUE),
            checkboxInput('rf','Random Forest',TRUE)
          )
        ),
        p('Les variables qualitatives sont automatiquement dummifiees. Les colonnes constantes sont retirees.'),
        actionButton('train','Entrainer et evaluer',class='btn-primary'),
        verbatimTextOutput('train_status')
      ),
      eval=tagList(
        h2('Evaluation comparative sur le jeu de test'),
        p('Precision, rappel, F1, AUC et accuracy. Le seuil de classification est fixe a 0,5.'),
        DT::DTOutput('metrics'),
        plotOutput('roc',height='430px'),
        plotOutput('importance',height='500px'),
        h4('Matrices de confusion'),verbatimTextOutput('confusions'),
        p('Les importances de la regression logistique et de la foret ne sont pas directement comparables en valeur absolue.')
      )
    )
  })
  # ================================================================
  # ÉTUDE DE CAS — ZONE À COMPLÉTER PAR LE GROUPE
  # Chaque onglet est indépendant. Ajoutez directement ici vos textes,
  # graphiques (plotOutput + renderPlot), tableaux et interprétations.
  # Aucun fichier n'est demandé à la personne qui consulte l'application.
  # ================================================================
  output$page_c_content <- renderUI({
    switch(input$page_c,
      intro=tagList(
        div(class='cinema-hero',
          div(class='cinema-kicker','🎬  ÉTUDE DE CAS  ·  CINÉMA'),
          h2('Notre étude de cas'),
          p('Présentation du projet et de ses objectifs.')
        ),
        h3('Présentation'),
        p('Section à compléter par le groupe : contexte, problématique et données étudiées.')
      ),
      diagnostic=tagList(
        h2('Diagnostic initial'),
        p('Section à compléter : description des variables, qualité des données et premiers constats.')
      ),
      explore=tagList(
        h2('Exploration statistique'),
        p('Section à compléter : graphiques, tableaux et analyses exploratoires.')
      ),
      methods=tagList(
        h2('Choix méthodologiques'),
        p('Section à compléter : préparation des données, méthodes retenues et justifications.')
      ),
      results=tagList(
        h2('Modèles et performances'),
        p('Section à compléter : résultats de vos modèles, graphiques et comparaison des performances.')
      ),
      conclusion=tagList(
        h2('Interprétation et conclusion'),
        p('Section à compléter : interprétation des résultats, limites et conclusion.')
      )
    )
  })

}

shinyApp(ui,server)
