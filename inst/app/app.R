


#' CardioRVARapp
#'
#' Shiny app interface to analyze closed-loop and open-loop cardiovascular
#' interactions
#'
#' @author Alvaro Chao-Ecija, Marc Stefan Dawid-Milner
#'
#' @details
#' This script contains the commands for the building and functionality of
#' CardioRVAR's Shiny interface. This project has been developed by the
#' department of Physiology of the University of Malaga, and was supervised
#' and mentored by PhD. MS Dawid Milner.
#'
#' The purpose of this interface is to facilitate the use of CardioRVAR's
#' commands in a user-friendly way. With the interface, the user is able to:
#'
#' 1. Upload cardiovascular signals and select a specific time window
#'
#' 2. Model and evaluate closed-loop interactions between those signals
#'
#' 3. Baroreflex sensitivity and causal coherence estimation*
#'
#' 4. Cuantification of immediate transfer paths*
#'
#' 5. Estimation of noise source contribution
#'
#' *See the references section for further information.
#'
#'
#' @export
#'


library(shiny)
library(ggplot2)
library(tools)
library(CardioRVAR)


# CardioRVAR's interface
ui <- fluidPage(
  tags$head(tags$style(
    HTML("
                  body {
                  background-color: moccasin;
                  color: black;
                  }
                  ")
  )),
  
  # Application title
  titlePanel("CardioRVAR"),
  
  wellPanel(# Uploading section: the user can upload data, and select a specific time
    # window
    fluidRow(
      column(
        3,
        wellPanel(
          h2("Upload data"),
          p("Upload cardiovascular data",
            style = "text-align:justify;color:black;background-color:grey80"),
          uiOutput("Ex_mode_mesg"),
          uiOutput("data_file"),
          tags$hr(),
          radioButtons(
            "separator",
            "Choose separator",
            choices = c(
              Semicolon = ";",
              Comma = ",",
              Tab =
                "\t"
            ),
            selected = ";"
          ),
          tags$hr(),
          checkboxInput("Interpolate", "Interpolate Data", TRUE),
          numericInput("int_freq", "Frequency", value = 4, width = 70),
          actionButton("upload", "Confirm Upload")
        )
        
      ),
      column(
        9,
        wellPanel(
          style = "background:white",
          plotOutput("Raw", brush = "brush_raw", dblclick = "dbc_raw")
        ),
        br(),
        h5(textOutput("hr_stats")),
        h5(textOutput("sbp_stats"))
        
      )
      
    )),
  fluidRow(
    column(
      4,
      wellPanel(
        # Settings panel: manage particular settings regarding the analysis
        h3("Detrending settings:"),
        tags$hr(),
        numericInput("f_cutoff", "Cutoff", value = 0.035, width = 70),
        br(),
        h3("VAR model order:"),
        tags$hr(),
        checkboxInput(
          "Use_order",
          "Use customed model order (otherwise the best model order
                  according to the AIC will be used)",
          value = FALSE
        ),
        numericInput(
          "order",
          "Customed Model Order",
          value = 16,
          min = 1,
          max = 30,
          width = 70,
          step = 1
        ),
        br(),
        h3("Multiple comparisons settings:"),
        tags$hr(),
        selectInput(
          "mc_correction",
          "Select a correction method for multiple comparisons",
          choices = rev(p.adjust.methods),
          "bonferroni"
        ),
        br(),
        h3("Coherence settings:"),
        tags$hr(),
        checkboxInput(
          "Use_coh",
          "Use coherence threshold to calculate estimates",
          value = FALSE
        ),
        numericInput(
          "coh_thr",
          "Coherence threshold",
          value = 0.5,
          min = 0,
          max = 1,
          width = 70,
          step = 0.05
        ),
        br(),
        h3("Weighting settings:"),
        tags$hr(),
        checkboxInput(
          "Use_weight",
          "Weight estimates using a Gaussian function",
          value = FALSE
        ),
        br(),
        h3("Plot settings:"),
        tags$hr(),
        checkboxInput("Plot_phase", "Plot transfer phase", value = FALSE),
      )
    ),
    column(
      4,
      wellPanel(
        # Criteria panel: this panel displays the results for the MVAR validation criteria
        h3("VAR model details from chosen segment:"),
        tags$hr(),
        h4(textOutput("text_timesegment")),
        h4(textOutput("text_stationary")),
        h4(textOutput("text_model")),
        h4(textOutput("text_model2")),
        h4(textOutput("text_stability")),
        h4(textOutput("text_whitenoise")),
        h4(textOutput("text_validity")),
        br(),
        fluidRow(column(4),
                 column(
                   4,  actionButton("calculate", "Calculate Results")
                 ),
                 column(4),)
      ),
      wellPanel(
        h3("Download Results:"),
        tags$hr(),
        br(),
        textInput("name", "File name:", width = 300),
        br(),
        fluidRow(column(4),
                 column(
                   4, downloadButton("download", "Download Calculated Results")
                 ),
                 column(4),),
        br(),
        fluidRow(column(4),
                 column(
                   4, downloadButton("download2", "Download Model Structure")
                 ),
                 column(4),)
      )
    ),
    column(
      4,
      wellPanel(
        h3("Path settings:"),
        tags$hr(),
        br(),
        selectInput("input", "Select Input", choices = c("SBP", "RR"), "SBP"),
        selectInput("output", "Select Output", choices = c("SBP", "RR"), "RR"),
        br(),
        br(),
        h3("Immediate effects path settings:"),
        tags$hr(),
        selectInput(
          "path_origin",
          "Select Origin for 0-Delay Transfer Path",
          choices = c("SBP", "RR"),
          "SBP"
        ),
        br(),
        p(
          "Specify the path that you want to isolate from the closed-loop model by selecting the specific input and output
      variables from the path. You may also select an immediate and unidirectional transfer path between those variables.",
          style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
        ),
        br(),
        p(
          "Transfer functions will then be calculated from the selected input to the chosen output, and will reflect the variability
      transfer. These functions will also take into account the selected zero delay effects, that will be reported separately.",
          style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
        )
      )
    )
  ),
  wellPanel(
    h3("Spectral Coherence:"),
    wellPanel(style = "background:white",
              plotOutput("coherence_plot")),
    br(),
    h3("Estimated transfer functions:"),
    tags$hr(),
    fluidRow(
      column(
        4,
        h4("Closed-Loop Transfer Function:"),
        wellPanel(style = "background:white",
                  plotOutput("noDEL")),
        h4(textOutput("HF1")),
        h4(textOutput("LF1")),
        h4(textOutput("HFpeak1")),
        h4(textOutput("LFpeak1")),
        h4(textOutput("HFcohMax1")),
        h4(textOutput("LFcohMax1")),
        #br(),
      ),
      column(
        4,
        h4("Closed-Loop Transfer Function with 0-delay effects:"),
        wellPanel(style = "background:white",
                  plotOutput("yesDEL")),
        h4(textOutput("HF2")),
        h4(textOutput("LF2")),
        h4(textOutput("HFpeak2")),
        h4(textOutput("LFpeak2")),
        h4(textOutput("HFcohMax2")),
        h4(textOutput("LFcohMax2")),
        #br(),
      ),
      column(
        4,
        h4("Open-Loop Transfer Function:"),
        wellPanel(style = "background:white",
                  plotOutput("open")),
        h4(textOutput("HF3")),
        h4(textOutput("LF3")),
        h4(textOutput("HFpeak3")),
        h4(textOutput("LFpeak3")),
        h4(textOutput("HFcohMax3")),
        h4(textOutput("LFcohMax3"))
      )
    )
  ),
  wellPanel(
    fluidRow(column(
      6,
      h4("Comparison of Gains:"),
      wellPanel(style = "background:white",
                plotOutput("barplot"))
    ),
    column(
      6,
      h4("LF/HF ratio of Gains:"),
      wellPanel(style = "background:white",
                plotOutput("barplot2"))
    )),
    br(),
    p(
      "Color legend: closed-loop estimates from model without modification for immediate effects (black),
      closed-loop estimates from modified model (dark grey), open-loop estimates (light grey). A horizontal
      red line may be used to highlight the unity threshold for the ratios.",
      style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
    ),
  ),
  wellPanel(
    # Immediate effects panel
    h3("Estimated 0-lag effects:"),
    tags$hr(),
    wellPanel(style = "background:white",
              plotOutput("barplot3")),
    h4(textOutput("estimated_a0"))
  ),
  
  
  wellPanel(
    h3("Noise transfer functions (without 0-lag effects):"),
    tags$hr(),
    fluidRow(column(
      6,
      h4("Transfer of Output Noise to Output:"),
      wellPanel(style = "background:white",
                plotOutput("noise1_noA0"))
    ),
    column(
      6,
      h4("Transfer of Input Noise to Output:"),
      wellPanel(style = "background:white",
                plotOutput("noise2_noA0"))
    )),
    br(),
    h3("Noise transfer functions (with 0-lag effects):"),
    tags$hr(),
    fluidRow(column(
      6,
      h4("Transfer of Output Noise to Output:"),
      wellPanel(style = "background:white",
                plotOutput("noise1_A0"))
    ),
    column(
      6,
      h4("Transfer of Input Noise to Output:"),
      wellPanel(style = "background:white",
                plotOutput("noise2_A0"))
    ))
  ),
  wellPanel(
    h3("Noise contributions for RR PSD:"),
    tags$hr(),
    fluidRow(column(
      12,
      wellPanel(style = "background:white",
                plotOutput("noiseCon1"))
    )),
    br(),
    h3("Noise contributions for SBP PSD:"),
    tags$hr(),
    fluidRow(column(
      12,
      wellPanel(style = "background:white",
                plotOutput("noiseCon2"))
    )),
    br(),
    h3("Causal flow:"),
    tags$hr(),
    fluidRow(column(
      12,
      wellPanel(style = "background:white",
                plotOutput("causal"))
    )),
    h4(textOutput("CausalCoh_HF")),
    h4(textOutput("CausalCoh_LF")),
    br(),
    fluidRow(column(
      12,
      wellPanel(style = "background:white",
                plotOutput("causal2"))
    ))
  ),
  wellPanel(
    fluidRow(
      column(
        4,
        numericInput("RRnoiseVar", "RR noise variance:", 0),
        h4(textOutput("RRnoiseVarT"))
      ),
      column(
        4,
        numericInput("SBPnoiseVar", "SBP noise variance:", 0),
        h4(textOutput("SBPnoiseVarT"))
      ),
      column(4,
             numericInput("A0_effect", "A0 effect:", 0),
             h4(textOutput("estimated_a0_2")))
      
    ),
    fluidRow(column(
      6,
      h5("RR PSD"),
      wellPanel(style = "background:white",
                plotOutput("psd1"))
    ),
    column(
      6,
      h5("SBP PSD"),
      wellPanel(style = "background:white",
                plotOutput("psd2"))
    )),
    actionButton("simulate", "Calculate Simulation")
  ),
  wellPanel(
    h3("PSDs from individual AR(p) models:"),
    tags$hr(),
    fluidRow(
      column(
        6,
        h4("RR PSD:"),
        wellPanel(style = "background:white",
                  plotOutput("ar1")),
        h4(textOutput("arHF1")),
        h4(textOutput("arHF11")),
        h4(textOutput("arHFpeak1")),
        h4(textOutput("arLF1")),
        h4(textOutput("arLF11")),
        h4(textOutput("arLFpeak1"))
      ),
      column(
        6,
        h4("SBP PSD:"),
        wellPanel(style = "background:white",
                  plotOutput("ar2")),
        h4(textOutput("arHF2")),
        h4(textOutput("arHF22")),
        h4(textOutput("arHFpeak2")),
        h4(textOutput("arLF2")),
        h4(textOutput("arLF22")),
        h4(textOutput("arLFpeak2"))
      )
    )
  ),
  
  # Background section: provides brief theoretical background for the algorithms
  # that this software works with.
  fluidRow(column(
    12,
    wellPanel(
      h2("About this software"),
      br(),
      p(
        "CardioRVAR is a software developed by the Department of Physiology of the University of Malaga. With this
                 software, interactions between cardiovascular variables can be modelled as a closed loop, and certain parameters
                 such as the baroreflex sensitivity can be analyzed.

               In the following sections, we will discuss several features regarding these analyses. Several references have been
               provided for further information. For more information, please contact Alvaro Chao-Ecija (alvarochaoecija.rprojects@gmail.com).",
        style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
      ),
      br(),
      tags$hr(),
      h3("Model estimation and validation"),
      p(
        "Each closed-loop model is estimated by calculating a Vector Autoregressive (VAR) model from the data. Additionally, the
               estimated model according to Korhonen et al. (1) and Faes et al. (2). The modified allows immediate effects from the variables, which can be estimated from the covariance
               matrix of the model, therefore eliminating possible cross-correlations between the noise sources due to the presence of
               these effects. The immediate effects from systollic blood pressure to the inter-beat intervals is reported by the software
               separately to allow further analyses. Further information about this modification of the VAR can be obtained from the tutorial
               written by Hytti et al. (3). In this software, immediate effects are chosen by a priori assumptions: the software lets
               users choose the appropriate immediate transfer path they want to include in the analyses.",
        style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
      ),
      br(),
      p(
        "This software uses certain tests for model validation according to the strategy proposed by Seth (4,5) and Ding et al. (6), which was used in his
               multivariate VAR modelling brain connectivity toolbox. Following their strategy, the signals are tested for stationarity using the ADF and
               KPSS tests, the stability of the model is assessed, and the white-noise nature of the model residuals is evaluated using the Durbin-Watson test. For non-stationary signals, and in
               order to eliminate very low frequency components of each signal, a wavelet-detrending algorithm is provided by the software. More on
               wavelet-detrending can be learned from the work of Li et al. (7), which also discusses other detrending methods.",
        style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
      ),
      br(),
      tags$hr(),
      h3("Baroreflex sensitivity estimation"),
      br(),
      p(
        "Once the VAR model for a recording has been estimated and validated, the baroreflex sensitivity is calculated as a
               transfer function according to the work of Barbieri et al. (8). Several transfer functions are provided: the open-loop
               transfer function, the one obtained from the unmodified closed-loop model, and the one obtained by the modified closed-loop model.
               Coherence values are also estimated in order to choose an appropriate threshold to ensure the reliability of the estimates, as it is
               usually done when estimating the baroreflex (the usual coherence threshold used in literature is 0.5, and thus this is the default
               threshold. Sometimes thresholding makes it impossible to give an estimate for certain subjects, therefore the software provides other
               estimation methods. We would like to hightligh the method proposed by McLoone & Ringwood (9), which consists of applying Gaussian weights
               to the transfer function and thus calculating the weighted average at each band, which enables the analysis of the baroreflex at the center
               of each band.",
        style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
      ),
      br(),
      tags$hr(),
      h3("Causal coherence"),
      br(),
      p(
        "Causal coherence, as introduced by Faes et al. (10, 11) can be calculated using this software. The software uses the modified
               VAR model to calculate the causal coherence. As explained in the cited works, the causal coherence corresponds to the coherence
               value when one of the interactions present at the model is completely supressed.",
        style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
      ),
    )
  )),
  # References section: gives proper credit for the algorithms and background.
  fluidRow(column(
    12,
    wellPanel(
      h2("References"),
      tags$hr(),
      h5(
        "1. Korhonen I, Mainardi L, Baselli G, Bianchi A, Loula P,
              Carrault. Linear multivariate models for physiological signal analysis: applications.
              Comput Methods Programs Biomed. 1996;51(1-2):121-30"
      ),
      br(),
      h5(
        "2. Faes L, Nollo G. Multivariate Frequency Domain Analysis of Causal Interactions
             in Physiological Time Series. In Biomedical Engineering, Trends in Electronics,
                Communications and Software. 2011"
      ),
      br(),
      h5(
        "3.Hytti H, Takalo R, Ihalainen H. Tutorial on Multivariate Autoregressive Modelling.
              J Clin Monit Comput. 2006;20(1):101-8"
      ),
      br(),
      h5(
        "4. Seth AK. A MATLAB toolbox for Granger causal connectivity analysis. J
              Neurosci Methods. 2010;186(2):262-73"
      ),
      br(),
      h5(
        "5. Seth AK. Granger Causal Conectivity Analysis: A MATLAB Toolbox. 2011."
      ),
      br(),
      h5(
        "6. Ding M, Bressler S, Yang W, Liang H. Short-window spectral analysis of cortical
                event-related potentials by adaptative multivariate autoregressive modelling: data
                preprocessing, model validation, and variability assessment. Biol Cybern. 2000;83:35-45."
      ),
      br(),
      h5(
        "7. Li L, Liu C, Li K, Liu C. Comparison of detrending methods in spectral analysis of
                heart rate variability. World Academy of Science, Engineering and Technology, Open Science
                Index 57, International Journal of Biomedical and Biological Engineering. 2011;5(9):398-402."
      ),
      br(),
      h5(
        "8. Barbieri R, Parati G, Saul JP. Closed- versus Open-Loop Assessment of Heart Rate
             Baroreflex. IEEE Eng Med Biol Mag. 2001;20(2):33-42."
      ),
      br(),
      h5(
        "9. McLoone V, Ringwood JV. A system identification approach to baroreflex sensitivity
              estimation."
      ),
      br(),
      h5(
        "10. Faes L, Porta A, Antolini R, Nollo G. Role of causality in the evaluation of coherence and
                transfer function between heart period and systolic pressure in humans. Comput Cardiol.
                2004;277-80."
      ),
      br(),
      h5(
        "11. Faes L, Mase M, Nollo G, Chon KH, Florian JP. Measuring postural-related changes of spontaneous
                baroreflex sensitivity after repeated long-duration diving: frequency domain approaches. Auton Neurosci.
                2013 Nov;178(1-2):96-102."
      ),
      tags$hr()
      
    )
  )),
  fluidRow(column(12,
                  wellPanel(
                    h2("Example mode"),
                    tags$hr(),
                    fluidRow(column(
                      9,
                      p(
                        "When the following checkbox is activated, the app will be on example mode.
        Without uploading any files, you can now click the Confirm Upload button,
        and an example file will be loaded.",
                        style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
                      ),
                    ),
                    column(
                      3,
                      checkboxInput("ex_mode", "Example mode", value = TRUE)
                    ))
                  )))
  
)


# Code for the server
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024 ^ 2) # To change default size for uploaded data
  output$data_file <- renderUI({
    fileInput(
      "data_file",
      "Upload data file",
      multiple = FALSE,
      accept = c(
        "text/csv",
        "text/comma-separated-values",
        "text/plain",
        ".csv",
        ".txt"
      )
    )
    
  })
  output$Ex_mode_mesg <- renderUI({
    if(input$ex_mode) p(
    "Example mode is ON: if you click the Confirm Upload button without uploading any file,
     an example file will be loaded.",
     style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")
  })
  Data <- reactiveValues()
  Data$Results <- FALSE
  Data$Signals <- NA
  Data$Det_Signals <- NA
  Data$Time_model1 <- NA
  Data$Time_model2 <- NA
  Data$freq <- NA
  Data$Freq_model <- NA
  Data$Steps <- NA
  Data$Evaluated <- FALSE
  Data$Form <- list()
  Data$Structure <- list()
  Data$Validity <- FALSE
  output$text_timesegment <-
    renderText({
      return("Time segment: no time segment has been chosen")
    })
  output$text_stationary <- renderText({
    return("Stationarity:")
  })
  output$text_model <-
    renderText({
      return("Model according to AIC:")
    })
  output$text_model2 <- renderText({
    return("Chosen model:")
  })
  output$text_stability <- renderText({
    return("Stability:")
  })
  output$text_whitenoise <- renderText({
    return("Residuals:")
  })
  output$text_validity <- renderText({
    return("Model Evaluation:")
  })
  
  observeEvent(input$upload, {
    tryCatch({
      if(is.null(input$data_file)) {
        if(input$ex_mode) {
          showNotification("Loading example file",
                           duration = NULL,
                           closeButton = TRUE)
          data("Cardiovascular")
          data <- Cardiovascular
        }
      } else {
      req(input$data_file)
      if (file_ext(input$data_file$datapath) == "csv") {
        data <- read.csv(input$data_file$datapath,
                         header = TRUE,
                         sep = input$separator)
      } else if (file_ext(input$data_file$datapath) == "txt") {
        data <- read.table(input$data_file$datapath, header = TRUE)
        if (!("RR" %in% names(data))) {
          data$RR <- data$Time * 1000
          data$Time <- cumsum(data$Time)
        } else if (max(data$RR) < 100) {
          data$RR <- data$RR * 1000
        }
      }
      }
      R_data <- data
      Data$Raw <- R_data
      freq <- isolate(Data$freq)
      if (input$Interpolate) {
        data <- ResampleData(data, input$int_freq)
        Data$freq <- input$int_freq
      } else {
        Data$freq <- 1 / diff(Time)[1]
      }
      List <- isolate(Data$Signals)
      validity <- isolate(Data$Validity)
      Time <- data$Time
      SBP <- data$SBP
      RR <- data$RR
      N <- NROW(Time)
      saveData <- list(Time = Time,
                       RR = RR,
                       SBP = SBP)
      Data$Signals <- saveData
      results <- isolate(Data$Results)
      Data$Results <- FALSE
      Data$Validity <- FALSE
      saveData$HR <- 60000 / saveData$RR
      output$Raw <- renderPlot({
        ggplot(data = data.frame(saveData), aes(Time)) +
          geom_line(aes(y = HR, colour = "HR")) + geom_line(aes(y = SBP, colour = "SBP")) +
          scale_y_continuous(name = "bpm",
                                      sec.axis = sec_axis(trans =  ~ . * 1, name = "mmHg"))
      })
      output$data_file <- renderUI({
        fileInput(
          "data_file",
          "Upload data file",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/plain",
            ".csv",
            ".txt"
          )
        )
      })
    }, error = function(CardioRVAR_error) {
      showNotification(as.character(CardioRVAR_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  
  output$hr_stats <- renderText({
    check_brush <- !is.null(input$brush_raw)
    if (check_brush) {
      data <- isolate(Data$Raw)
      select_time <-
        (data$Time >= input$brush_raw$xmin &
           data$Time <= input$brush_raw$xmax)
      select_time <- c(1:NROW(data$Time))[select_time]
      RR <- data$RR[select_time]
      HR <- 60000 / RR
      text <-
        paste("Heart Rate: ",
              round(mean(HR), 4),
              " bpm (SD ",
              round(sd(HR), 4),
              " bpm)",
              sep = "")
      return(text)
    }
  })
  output$sbp_stats <- renderText({
    check_brush <- !is.null(input$brush_raw)
    if (check_brush) {
      data <- isolate(Data$Raw)
      select_time <-
        (data$Time >= input$brush_raw$xmin &
           data$Time <= input$brush_raw$xmax)
      select_time <- c(1:NROW(data$Time))[select_time]
      SBP <- data$SBP[select_time]
      text <-
        paste(
          "Systolic Blood Pressure: ",
          round(mean(SBP), 4),
          " mmHg (SD ",
          round(sd(SBP), 4),
          " mmHg)",
          sep = ""
        )
      return(text)
    }
  })
  
  
  observeEvent(input$brush_raw,
               {
                 tryCatch({
                   evaluated <- isolate(Data$Evaluated)
                   validity <- isolate(Data$Validity)
                   data <- isolate(Data$Signals)
                   data2 <- isolate(Data$Det_Signals)
                   freq <- isolate(Data$freq)
                   time_model <- isolate(Data$Time_model2)
                   minT <- min(data$Time)
                   maxT <- max(data$Time)
                   t1 <- input$brush_raw$xmin
                   t2 <- input$brush_raw$xmax
                   if (t1 <= minT)
                     t1 <- minT
                   if (t2 >= maxT)
                     t2 <- maxT
                   timesegment <-
                     paste(
                       "Time segment: from",
                       t1 %/% 60,
                       "min",
                       round(t1 %% 60, 3) ,
                       " sec to",
                       t2 %/% 60,
                       "min",
                       round(t2 %% 60, 3),
                       "sec."
                     )
                   output$text_timesegment <-
                     renderText({
                       return(timesegment)
                     })
                   select_time <-
                     (data$Time >= input$brush_raw$xmin &
                        data$Time <= input$brush_raw$xmax)
                   select_time <- c(1:NROW(data$Time))[select_time]
                   if (NROW(select_time) < 64) {
                     message <- "Segment is too short. Cannot be evaluated"
                     output$text_stationary <-
                       renderText({
                         return(paste("Stationarity:", message))
                       })
                     output$text_model <-
                       renderText({
                         return(paste("Model according to AIC:", message))
                       })
                     output$text_model2 <-
                       renderText({
                         return(paste("Chosen model:", message))
                       })
                     output$text_stability <-
                       renderText({
                         return(paste("Stability:", message))
                       })
                     output$text_whitenoise <-
                       renderText({
                         return(paste("Residuals:", message))
                       })
                     output$text_validity <-
                       renderText({
                         return(paste("Model Evaluation:", message))
                       })
                     Data$Validity <- FALSE
                   } else{
                     RR <- DetrendWithCutoff(data$RR[select_time],  f = freq,
                                             cutoff = input$f_cutoff)
                     SBP <-
                       DetrendWithCutoff(data$SBP[select_time],  f = freq,
                                         cutoff = input$f_cutoff)
                     stationarity <-
                       CheckStationarity(
                         cbind(SBP = SBP, RR = RR),
                         warnings = FALSE,
                         verbose = FALSE,
                         correction = input$mc_correction
                       )
                     if (input$path_origin == "RR") {
                       data <- cbind(RR = RR, SBP = SBP)
                     } else {
                       data <- cbind(SBP = SBP, RR = RR)
                     }
                     model1 <- EstimateVAR(data,
                                           warnings = FALSE,
                                           correction = input$mc_correction)
                     if (input$Use_order) {
                       model2 <-
                         EstimateVAR(
                           data,
                           p = input$order,
                           pmax = NULL,
                           warnings = FALSE,
                           correction = input$mc_correction
                         )
                     } else {
                       model2 <- model1
                     }
                     stable = ifelse(stationarity, DiagnoseStability(model2), FALSE)
                     whitenoise <-
                       ifelse(
                         stationarity,
                         DiagnoseResiduals(model2,
                                           correction = input$mc_correction),
                         FALSE
                       )
                     validity <- stationarity & stable & whitenoise
                     Data$Validity <- validity
                     Data$Det_Signals <- data
                     Data$Time_model2 <- model2
                     output$text_stationary <- renderText({
                       stationary <- ifelse(
                         stationarity,
                         "Stationarity: The segment is stationary",
                         "Stationarity: The segment is not stationary"
                       )
                       return(stationary)
                     })
                     output$text_model <- renderText({
                       model1 <- ifelse(
                         stationarity,
                         paste("Model according to AIC: VAR(", model1$p, ")", sep = ""),
                         "Model according to AIC: cannot be estimated as the time segment is not stationary"
                       )
                       return(model1)
                     })
                     output$text_model2 <- renderText({
                       model2 <- ifelse(
                         stationarity,
                         paste("Chosen model: VAR(", model2$p, ")", sep = ""),
                         "Chosen model: cannot be estimated as the time segment is not stationary"
                       )
                       return(model2)
                     })
                     output$text_stability <- renderText({
                       if (stationarity) {
                         stable <- ifelse(
                           stable,
                           "Stability: the model is stable",
                           "Stability: the model is not stable"
                         )
                         return(stable)
                       } else {
                         return("Stability: cannot be evaluated as the time segment is not stationary")
                       }
                     })
                     output$text_whitenoise <- renderText({
                       if (stationarity) {
                         whitenoise <- ifelse(
                           whitenoise,
                           "Residuals: residuals are a white noise process",
                           "Residuals: residuals are not a white noise process"
                         )
                         return(whitenoise)
                       } else {
                         return("Residuals: residuals cannot be evaluated as the segment is not stationary")
                       }
                     })
                     output$text_validity <- renderText({
                       validity <-
                         ifelse(validity,
                                "Model Evaluation: VALID",
                                "Model Evaluation: NOT VALID")
                       return(validity)
                       
                     })
                   }
                   
                   #}
                 }, error = function(CardioRVAR_error) {
                   showNotification(as.character(CardioRVAR_error),
                                    type = "error",
                                    duration = NULL)
                 })
               })
  
  
  
  
  
  observeEvent(input$calculate, {
    tryCatch({
      check_brush <- !is.null(input$brush_raw)
      data_validity <- isolate(Data$Validity)
      Structure <- isolate(Data$Structure)
      steps <- isolate(Data$Steps)
      if (check_brush & data_validity) {
        data <- isolate(Data$Det_Signals)
        model <- isolate(Data$Time_model2)
        Names <- colnames(data)
        freq <- isolate(Data$freq)
        freq_model <- isolate(Data$Freq_model)
        form <- isolate(Data$Form)
        if (Names[NROW(Names)] == "SBP") {
          units <- c("ms", "mmHg")
        } else {
          units <- c("mmHg", "ms")
        }
        form$"VAR(p)" <- model$p
        freq_model1 <- ParamFreqModel(model, A0 = FALSE, dt = 1 / freq)
        freq_model2 <- ParamFreqModel(model, A0 = TRUE, dt = 1 / freq)
        Structure <- freq_model2
        Structure$Signals <- data
        Structure$p <- model$p
        in_var <- match(input$input, Names)
        out_var <- match(input$output, Names)
        form$Input <- input$input
        form$Output <- input$output
        form$"Transfer Units" <-
          paste(units[out_var], "/", units[in_var], sep = "")
        form$"Coherence Threshold" <- input$coh_thr
        form$"Gaussian Weight" <-
          ifelse(
            input$Use_weight,
            "Gaussian weighted-average was used.",
            "No weighted-average was used."
          )
        rr_pos <- match("RR", Names)
        sbp_pos <- match("SBP", Names)
        coherence <- CalculateCoherence(freq_model2, out_var, in_var)
        estimations1 <-
          GetExpectedValues(
            freq_model1,
            str = FALSE,
            coherence = coherence,
            thr = input$coh_thr,
            use.coh = input$Use_coh,
            weight = input$Use_weight
          )
        estimations2 <-
          GetExpectedValues(
            freq_model2,
            str = FALSE,
            coherence = coherence,
            thr = input$coh_thr,
            use.coh = input$Use_coh,
            weight = input$Use_weight
          )
        est_form1 <-
          GetExpectedValues(
            freq_model2,
            str = FALSE,
            coherence = coherence,
            thr = input$coh_thr,
            use.coh = FALSE,
            weight  = input$Use_weight
          )
        est_form2 <-
          GetExpectedValues(
            freq_model2,
            str = FALSE,
            coherence = coherence,
            thr = input$coh_thr,
            use.coh = ifelse(input$Use_weight, FALSE, TRUE),
            weight = input$Use_weight
          )
        peaks1 <- GetPeaks(freq_model1, str = FALSE)
        peaks2 <- GetPeaks(freq_model2, str = FALSE)
        peaks3 <-
          GetEstimateAtMaxCoh(freq_model1, str = FALSE, coherence = coherence)
        peaks4 <-
          GetEstimateAtMaxCoh(freq_model2, str = FALSE, coherence = coherence)
        output$coherence_plot <-
          renderPlot({
            PlotCoherence(freq_model1,
                          out_var,
                          in_var,
                          coherence,
                          thr = input$coh_thr)
          })
        output$noDEL <-
          renderPlot({
            PlotTransferFun(freq_model1,
                            out_var,
                            in_var,
                            plot.phase = input$Plot_phase)
          })
        output$yesDEL <-
          renderPlot({
            PlotTransferFun(freq_model2,
                            out_var,
                            in_var,
                            plot.phase = input$Plot_phase)
          })
        output$open <-
          renderPlot({
            PlotTransferFun(
              freq_model1,
              out_var,
              in_var ,
              open = TRUE,
              plot.phase = input$Plot_phase
            )
          })
        output$HF1 <- renderText({
          HF <- estimations1$HF$Transfer_Functions[out_var, in_var, 1]
          HF1 <-
            paste(
              "Estimated transfer function at HF band: ",
              round(HF, 3),
              " ",
              units[out_var],
              "/",
              units[in_var],
              sep = ""
            )
          return(HF1)
        })
        output$HFpeak1 <- renderText({
          HF <- peaks1$HF$Transfer_Functions[out_var, in_var, 1]
          HF1 <-
            paste("Estimated HF modulation peak at", round(HF, 3), "Hz ")
          return(HF1)
        })
        output$HFcohMax1 <- renderText({
          HF <- peaks3$HF$Transfer_Functions[out_var, in_var, 1]
          HF1 <-
            paste(
              "Estimate at maximum squared coherence at HF band: ",
              round(HF, 3),
              " ",
              units[out_var],
              "/",
              units[in_var],
              sep = ""
            )
          return(HF1)
        })
        output$LF1 <- renderText({
          LF <- estimations1$LF$Transfer_Functions[out_var, in_var, 1]
          LF1 <-
            paste(
              "Estimated transfer function at LF band: ",
              round(LF, 3),
              " ",
              units[out_var],
              "/",
              units[in_var],
              sep = ""
            )
          return(LF1)
        })
        output$LFpeak1 <- renderText({
          LF <- peaks1$LF$Transfer_Functions[out_var, in_var, 1]
          LF1 <-
            paste("Estimated LF modulation peak at", round(LF, 3), "Hz ")
          return(LF1)
        })
        output$LFcohMax1 <- renderText({
          LF <- peaks3$LF$Transfer_Functions[out_var, in_var, 1]
          LF1 <-
            paste(
              "Estimate at maximum squared coherence at LF band: ",
              round(LF, 3),
              " ",
              units[out_var],
              "/",
              units[in_var],
              sep = ""
            )
          return(LF1)
        })
        output$HF2 <- renderText({
          HF <- estimations2$HF$Transfer_Functions[out_var, in_var, 1]
          HF2 <-
            paste(
              "Estimated transfer function at HF band: ",
              round(HF, 3),
              " ",
              units[out_var],
              "/",
              units[in_var],
              sep = ""
            )
          return(HF2)
        })
        form$"Transfer (HF):" <-
          round(est_form1$HF$Transfer_Functions[out_var, in_var, 1], 3)
        form$"Transfer (HF) with coherence threshold:" <-
          round(est_form2$HF$Transfer_Functions[out_var, in_var, 1], 3)
        output$HFpeak2 <- renderText({
          HF <- peaks2$HF$Transfer_Functions[out_var, in_var, 1]
          HF2 <-
            paste("Estimated HF modulation peak at", round(HF, 3), "Hz ")
          return(HF2)
        })
        form$"Peak (HF, Hz):" <-
          round(peaks2$HF$Transfer_Functions[out_var, in_var, 1], 3)
        output$HFcohMax2 <- renderText({
          HF <- peaks4$HF$Transfer_Functions[out_var, in_var, 1]
          HF2 <-
            paste(
              "Estimate at maximum squared coherence at HF band: ",
              round(HF, 3),
              " ",
              units[out_var],
              "/",
              units[in_var],
              sep = ""
            )
          return(HF2)
        })
        form$"Transfer at max coh (HF):" <-
          round(peaks4$HF$Transfer_Functions[out_var, in_var, 1], 3)
        output$LF2 <- renderText({
          LF <- estimations2$LF$Transfer_Functions[out_var, in_var, 1]
          LF2 <-
            paste(
              "Estimated transfer function at LF band: ",
              round(LF, 3),
              " ",
              units[out_var],
              "/",
              units[in_var],
              sep = ""
            )
          return(LF2)
        })
        form$"Transfer (LF):" <-
          round(est_form1$LF$Transfer_Functions[out_var, in_var, 1], 3)
        form$"Transfer (LF) with coherence threshold:" <-
          round(est_form2$LF$Transfer_Functions[out_var, in_var, 1], 3)
        output$LFpeak2 <- renderText({
          LF <- peaks2$LF$Transfer_Functions[out_var, in_var, 1]
          LF2 <-
            paste("Estimated LF modulation peak at", round(LF, 4), "Hz ")
          return(LF2)
        })
        form$"Peak (LF, Hz):" <-
          round(peaks2$LF$Transfer_Functions[out_var, in_var, 1], 3)
        output$LFcohMax2 <- renderText({
          LF <- peaks4$LF$Transfer_Functions[out_var, in_var, 1]
          LF2 <-
            paste(
              "Estimate at maximum squared coherence at LF band: ",
              round(LF, 3),
              " ",
              units[out_var],
              "/",
              units[in_var],
              sep = ""
            )
          return(LF2)
        })
        form$"Transfer at max coh (LF):" <-
          round(peaks4$LF$Transfer_Functions[out_var, in_var, 1], 3)
        output$HF3 <- renderText({
          HF <- estimations1$HF$Open_Transfer_Functions[out_var, in_var, 1]
          HF3 <-
            paste(
              "Estimated transfer function at HF band: ",
              round(HF, 3),
              " ",
              units[out_var],
              "/",
              units[in_var],
              sep = ""
            )
          return(HF3)
        })
        output$HFpeak3 <- renderText({
          HF <- peaks1$HF$Open_Transfer_Functions[out_var, in_var, 1]
          HF3 <-
            paste("Estimated HF modulation peak at", round(HF, 3), "Hz ")
          return(HF3)
        })
        output$HFcohMax3 <- renderText({
          HF <- peaks3$HF$Open_Transfer_Functions[out_var, in_var, 1]
          HF3 <-
            paste(
              "Estimate at maximum squared coherence at HF band: ",
              round(HF, 3),
              " ",
              units[out_var],
              "/",
              units[in_var],
              sep = ""
            )
          return(HF3)
        })
        output$LF3 <- renderText({
          LF <- estimations1$LF$Open_Transfer_Functions[out_var, in_var, 1]
          LF3 <-
            paste(
              "Estimated transfer function at LF band: ",
              round(LF, 3),
              " ",
              units[out_var],
              "/",
              units[in_var],
              sep = ""
            )
          return(LF3)
        })
        output$LFpeak3 <- renderText({
          LF <- peaks1$LF$Open_Transfer_Functions[out_var, in_var, 1]
          LF3 <-
            paste("Estimated LF modulation peak at", round(LF, 3), "Hz ")
          return(LF3)
        })
        output$LFcohMax3 <- renderText({
          LF <- peaks3$LF$Open_Transfer_Functions[out_var, in_var, 1]
          LF3 <-
            paste(
              "Estimate at maximum squared coherence at LF band: ",
              round(LF, 3),
              " ",
              units[out_var],
              "/",
              units[in_var],
              sep = ""
            )
          return(LF3)
        })
        output$barplot <- renderPlot({
          par(mar = c(5, 4, 4, 8))
          barplot(
            cbind(
              HF = c(
                estimations1$HF$Transfer_Functions[out_var, in_var, 1],
                estimations2$HF$Transfer_Functions[out_var, in_var, 1],
                estimations1$HF$Open_Transfer_Functions[out_var, in_var, 1]
              ),
              LF = c(
                estimations1$LF$Transfer_Functions[out_var, in_var, 1],
                estimations2$LF$Transfer_Functions[out_var, in_var, 1],
                estimations1$LF$Open_Transfer_Functions[out_var, in_var, 1]
              )
            ),
            beside = TRUE,
            legend.text = c("Closed", "Closed (A0)", "Open"),
            args.legend = list(
              x = "topright",
              bty = "n",
              xpd = TRUE,
              inset = c(-0.456, 0)
            )
          )
        })
        output$barplot2 <- renderPlot({
          par(mar = c(5, 4, 4, 12))
          barplot(
            cbind(
              Ratio = c(
                estimations1$LF$Transfer_Functions[out_var, in_var, 1],
                estimations2$LF$Transfer_Functions[out_var, in_var, 1],
                estimations1$LF$Open_Transfer_Functions[out_var, in_var, 1]
              ) /
                c(
                  estimations1$HF$Transfer_Functions[out_var, in_var, 1],
                  estimations2$HF$Transfer_Functions[out_var, in_var, 1],
                  estimations1$HF$Open_Transfer_Functions[out_var, in_var, 1]
                )
            )
            ,
            beside = TRUE,
            legend.text = c("Closed", "Closed (A0)", "Open"),
            args.legend = list(
              x = "topright",
              bty = "n",
              xpd = TRUE,
              inset = c(-0.7, 0)
            )
          )
          abline(h = 1, col = "red")
        })
        output$barplot3 <- renderPlot({
          barplot((diag(ncol(
            freq_model2$a0
          )) - freq_model2$a0)[out_var, in_var],
          width = 0.3,
          xlim = c(-0.2, 0.7),
          col = "red"
          )
        })
        form$lag0 <-
          round((diag(ncol(
            freq_model2$a0
          )) - freq_model2$a0)[out_var, in_var], 3)
        output$estimated_a0 <-  renderText({
          a0 <- (diag(ncol(freq_model2$a0)) - freq_model2$a0)[out_var, in_var]
          a0 <-
            paste("Estimated 0-lag effect: ",
                  round(a0, 3),
                  " ",
                  units[out_var],
                  "/",
                  units[in_var],
                  sep = "")
          return(a0)
        })
        output$estimated_a0_2 <-  renderText({
          a0 <- (diag(ncol(freq_model2$a0)) - freq_model2$a0)[out_var, in_var]
          a0 <- paste("Estimated 0-lag effect:", a0)
          return(a0)
        })
        output$noise1_noA0 <-
          renderPlot({
            PlotNoiseTransferFun(freq_model1, in_var, in_var,
                                 plot.phase = input$Plot_phase)
          })
        output$noise2_noA0 <-
          renderPlot({
            PlotNoiseTransferFun(freq_model1,
                                 out_var,
                                 in_var,
                                 plot.phase = input$Plot_phase)
          })
        output$noise1_A0 <-
          renderPlot({
            PlotNoiseTransferFun(freq_model2, in_var, in_var,
                                 plot.phase = input$Plot_phase)
          })
        output$noise2_A0 <-
          renderPlot({
            PlotNoiseTransferFun(freq_model2,
                                 out_var,
                                 in_var,
                                 plot.phase = input$Plot_phase)
          })
        nconRR1 <-
          NoiseContribution(
            freq_model2,
            rr_pos,
            rr_pos,
            coherence = coherence,
            thr = input$coh_thr,
            use.coh = input$Use_coh
          )
        nconRR2 <-
          NoiseContribution(
            freq_model2,
            rr_pos,
            sbp_pos,
            coherence = coherence,
            thr = input$coh_thr,
            use.coh = input$Use_coh
          )
        nconSBP1 <-
          NoiseContribution(
            freq_model2,
            sbp_pos,
            rr_pos,
            coherence = coherence,
            thr = input$coh_thr,
            use.coh = input$Use_coh
          )
        nconSBP2 <-
          NoiseContribution(
            freq_model2,
            sbp_pos,
            sbp_pos,
            coherence = coherence,
            thr = input$coh_thr,
            use.coh = input$Use_coh
          )
        output$noiseCon1 <-
          renderPlot({
            PlotNoiseContribution(nconRR1, nconRR2)
          })
        output$noiseCon2 <-
          renderPlot({
            PlotNoiseContribution(nconSBP1, nconSBP2)
          })
        CCoh <- CalculateCausalCoherence(freq_model2)
        output$causal <-
          renderPlot({
            PlotCausalCoherence(freq_model2, CCoh)
          })
        output$causal2 <-
          renderPlot({
            PlotCausality(freq_model2, in_var)
          })
        est_coh <- GetMeanCoherence(freq_model2, CCoh)
        max_est_coh <- GetMaxCoherence(freq_model2, CCoh)
        output$CausalCoh_HF <-
          renderText({
            return(
              paste(
                "Coherence from SBP to RR at HF (n.u.): ",
                round(est_coh$HF$C2, 3),
                " (maximum value: ",
                round(max_est_coh$HF$C2, 3),
                ") .Coherence from RR to SBP at HF (n.u.): ",
                round(est_coh$HF$C1, 3) ,
                " (maximum value: ",
                round(max_est_coh$HF$C1, 3),
                ")",
                sep  = ""
              )
            )
          })
        output$CausalCoh_LF <-
          renderText({
            return(
              paste(
                "Coherence from SBP to RR at LF (n.u.): ",
                round(est_coh$LF$C2, 3),
                " (maximum value: ",
                round(max_est_coh$LF$C2, 3),
                ") .Coherence from RR to SBP at LF (n.u.): ",
                round(est_coh$LF$C1, 3) ,
                " (maximum value: ",
                round(max_est_coh$LF$C1, 3),
                ")",
                sep  = ""
              )
            )
          })
        form$"Coherence at HF (n.u.)" <- round(est_coh$HF$Cr, 3)
        form$"Coherence at LF (n.u.)" <- round(est_coh$LF$Cr, 3)
        form$"Coherence from SBP to RR at HF (n.u.)" <-
          round(est_coh$HF$C2, 3)
        form$"Coherence from SBP to RR at LF (n.u.)" <-
          round(est_coh$LF$C2, 3)
        form$"Coherence from RR to SBP at HF (n.u.)" <-
          round(est_coh$HF$C1, 3)
        form$"Coherence from RR to SBP at LF (n.u.)" <-
          round(est_coh$LF$C1, 3)
        form$"Maximum coherence at HF (n.u.)" <-
          round(max_est_coh$HF$Cr, 3)
        form$"Maximum coherence at LF (n.u.)" <-
          round(max_est_coh$LF$Cr, 3)
        form$"Maximum coherence from SBP to RR at HF (n.u.)" <-
          round(max_est_coh$HF$C2, 3)
        form$"Maximum coherence from SBP to RR at LF (n.u.)" <-
          round(max_est_coh$LF$C2, 3)
        form$"Maximum coherence from RR to SBP at HF (n.u.)" <-
          round(max_est_coh$HF$C1, 3)
        form$"Maximum coherence from RR to SBP at LF (n.u.)" <-
          round(max_est_coh$LF$C1, 3)
        output$RRnoiseVarT <-
          renderText({
            return(paste(
              "Estimated RR noise variance:",
              freq_model2$Noise_Spectra[rr_pos, rr_pos]
            ))
          })
        output$SBPnoiseVarT <-
          renderText({
            return(paste(
              "Estimated SBP noise variance:",
              freq_model2$Noise_Spectra[sbp_pos, sbp_pos]
            ))
          })
        Data$Freq_model <- freq_model2
        S <-
          SimulateWithModel(
            freq_model2,
            c(
              freq_model2$Noise_Spectra[sbp_pos, sbp_pos],
              freq_model2$Noise_Spectra[rr_pos, rr_pos]
            ),
            freq_model2$a0[out_var, in_var],
            f = freq
          )
        output$psd1 <- renderPlot({
          PlotSimulatedS(freq_model2, S, 2)
        })
        output$psd2 <- renderPlot({
          PlotSimulatedS(freq_model2, S, 1)
        })
        RR <- data[, "RR"]
        SBP <- data[, "SBP"]
        output$ar1 <- renderPlot({
          PSD(RR, f = freq, p = model$p)
        })
        output$ar2 <-
          renderPlot({
            PSD(SBP,
                f = freq,
                p = model$p,
                unit = "mmHg2/Hz")
          })
        RR_PSD <-
          PSD(
            RR,
            f = freq,
            p = model$p,
            plot = FALSE,
            output = TRUE
          )
        SBP_PSD <-
          PSD(
            SBP,
            f = freq,
            p = model$p,
            plot = FALSE,
            output = TRUE
          )
        RR_PSD2 <- RR_PSD / sum(RR_PSD[1:2]) * 100
        SBP_PSD2 <- SBP_PSD / sum(SBP_PSD[1:2]) * 100
        output$arHF1 <-
          renderText({
            return(paste("HF:", round(RR_PSD[1], 3), "ms2"))
          })
        output$arHF11 <-
          renderText({
            return(paste("HF:", round(RR_PSD2[1], 3), "n.u."))
          })
        output$arHFpeak1 <-
          renderText({
            return(paste("HF peak at", round(RR_PSD[3], 3), "Hz"))
          })
        output$arLF1 <-
          renderText({
            return(paste("LF:", round(RR_PSD[2], 3), "ms2"))
          })
        output$arLF11 <-
          renderText({
            return(paste("LF:", round(RR_PSD2[2], 3), "n.u."))
          })
        output$arLFpeak1 <-
          renderText({
            return(paste("LF peak at", round(RR_PSD[4], 3), "Hz"))
          })
        output$arHF2 <-
          renderText({
            return(paste("HF:", round(SBP_PSD[1], 3), "mmHg2"))
          })
        output$arHF22 <-
          renderText({
            return(paste("HF:", round(SBP_PSD2[1], 3), "n.u."))
          })
        output$arHFpeak2 <-
          renderText({
            return(paste("HF peak at", round(SBP_PSD[3], 3), "Hz"))
          })
        output$arLF2 <-
          renderText({
            return(paste("LF:", round(SBP_PSD[2], 3), "mmHg2"))
          })
        output$arLF22 <-
          renderText({
            return(paste("LF:", round(SBP_PSD2[2], 3), "n.u."))
          })
        output$arLFpeak2 <-
          renderText({
            return(paste("LF peak at", round(SBP_PSD[4], 3), "Hz"))
          })
        form$"IBI PSD (HF, ms2)" <- round(RR_PSD[1], 3)
        form$"IBI PSD (HF, n.u.)" <- round(RR_PSD2[1], 3)
        form$"IBI PSD peak (HF, Hz)" <- round(RR_PSD[3], 3)
        form$"IBI PSD (LF, ms2)" <- round(RR_PSD[2], 3)
        form$"IBI PSD (LF, n.u.)" <- round(RR_PSD2[2], 3)
        form$"IBI PSD peak (LF, Hz)" <- round(RR_PSD[4], 3)
        form$"SBP PSD (HF, mmHg2)" <- round(SBP_PSD[1], 3)
        form$"SBP PSD (HF, n.u.)" <- round(SBP_PSD2[1], 3)
        form$"SBP PSD peak (HF, Hz)" <- round(SBP_PSD[3], 3)
        form$"SBP PSD (LF, mmHg2)" <- round(SBP_PSD[2], 3)
        form$"SBP PSD (LF, n.u.)" <- round(SBP_PSD2[2], 3)
        form$"SBP PSD peak (LF, Hz)" <- round(SBP_PSD[4], 3)
        Data$Form <- form
        Data$Structure <- Structure
        results <- isolate(Data$Results)
        Data$Results <- TRUE
      }
      
      observeEvent(input$simulate, {
        results <- isolate(Data$Results)
        if (results) {
          data <- isolate(Data$Freq_model)
          freq <- isolate(Data$freq)
          S <-
            SimulateWithModel(
              data,
              c(input$SBPnoiseVar, input$RRnoiseVar),
              input$A0_effect,
              f = input$int_freq
            )
          output$psd1 <- renderPlot({
            PlotSimulatedS(data, S, 2)
          })
          output$psd2 <- renderPlot({
            PlotSimulatedS(data, S, 1)
          })
        }
      })
      
      
    }, error = function(CardioRVAR_error) {
      showNotification(as.character(CardioRVAR_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$download <-
    downloadHandler(
      filename = function() {
        paste(input$name, ".txt", sep = "")
      },
      content = function(file) {
        data <- isolate(Data$Form)
        data <- t(data.frame(data))
        write.table(
          data,
          file  = file,
          sep = "\t",
          col.names = FALSE,
          quote = FALSE,
          dec = "."
        )
      }
    )
  
  output$download2 <-
    downloadHandler(
      filename = function() {
        paste(input$name, ".RDS", sep = "")
      },
      content = function(file) {
        data <- isolate(Data$Structure)
        saveRDS(data, file = file)
      }
    )
  session$onSessionEnded(function() {
    stopApp()
    if (.Platform$GUI != "RStudio")
      q(save = "no")
  })
  
}

# Run CardioRVAR
shinyApp(ui = ui, server = server)
