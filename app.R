library(shiny)
library(bslib)
library(connectapi)
library(ellmer)
library(shinychat)
library(dplyr)
library(purrr)
library(httr)

# Helper function to calculate time since deployment
time_since_deployment <- function(deploy_time) {
  if (is.null(deploy_time) || is.na(deploy_time)) {
    return("")
  }

  deploy_dt <- as.POSIXct(deploy_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  diff <- difftime(Sys.time(), deploy_dt, units = "auto")

  if (diff < 1) {
    return("(just now)")
  } else if (diff < 60) {
    return(sprintf("(%d min ago)", round(diff)))
  } else if (diff < 1440) {
    return(sprintf("(%d hrs ago)", round(diff / 60)))
  } else {
    return(sprintf("(%d days ago)", round(diff / 1440)))
  }
}

# Fetch Connect content list
fetch_connect_content_list <- function(client) {
  content_list <- get_content(client)

  app_modes <- c("jupyter-static", "quarto-static", "rmd-static", "static")

  filtered_content <- content_list %>%
    filter(
      app_mode %in% app_modes,
      app_role != "none",
      content_category != "pin"
    )

  return(filtered_content)
}

# Setup UI for when credentials are not configured
setup_ui <- page_fillable(
  tags$style(HTML("
    body {
      padding: 0;
      margin: 0;
      background: linear-gradient(135deg, #f7f8fa 0%, #e2e8f0 100%);
    }
    .setup-container {
      max-width: 800px;
      margin: 0 auto;
      padding: 2rem;
      min-height: 100vh;
      display: flex;
      align-items: center;
      justify-content: center;
    }
    .setup-card {
      background: white;
      border-radius: 16px;
      padding: 3rem;
      box-shadow: 0 20px 40px rgba(0, 0, 0, 0.1);
      width: 100%;
    }
    .setup-title {
      color: #2d3748;
      font-weight: 700;
      margin-bottom: 2rem;
      text-align: center;
      font-size: 2.5rem;
    }
    .setup-section-title {
      color: #4a5568;
      font-weight: 600;
      margin-top: 2.5rem;
      margin-bottom: 1rem;
      font-size: 1.5rem;
      border-left: 4px solid #667eea;
      padding-left: 1rem;
    }
    .setup-description {
      color: #718096;
      line-height: 1.6;
      margin-bottom: 1.5rem;
    }
    .setup-code-block {
      background: #f7fafc;
      border: 1px solid #e2e8f0;
      border-radius: 8px;
      padding: 1.5rem;
      font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
      font-size: 0.9rem;
      color: #2d3748;
      margin: 1rem 0;
      overflow-x: auto;
    }
    .setup-link {
      color: #667eea;
      text-decoration: none;
      font-weight: 500;
    }
    .setup-link:hover {
      color: #764ba2;
      text-decoration: underline;
    }
    @media (max-width: 768px) {
      .setup-container {
        padding: 1rem;
      }
      .setup-card {
        padding: 2rem;
      }
      .setup-title {
        font-size: 2rem;
      }
    }
  ")),
  div(
    class = "setup-container",
    div(
      class = "setup-card",
      h1("Setup", class = "setup-title"),
      h2("LLM API", class = "setup-section-title"),
      div(
        class = "setup-description",
        HTML("This app requires an LLM API Key to be set in the content access panel. Please configure one of the supported providers (OpenAI, Anthropic, etc.) before running the app.
              See the <a href='https://ellmer.tidyverse.org/' class='setup-link'>ellmer documentation</a> for more details on supported providers.")
      ),
      h3("Example for OpenAI API", class = "setup-section-title"),
      pre(
        class = "setup-code-block",
        "OPENAI_API_KEY = \"<your-key>\""
      ),
      h3("Example for Anthropic API", class = "setup-section-title"),
      pre(
        class = "setup-code-block",
        "ANTHROPIC_API_KEY = \"<your-key>\""
      ),
      h2("Connect Visitor API Key", class = "setup-section-title"),
      div(
        class = "setup-description",
        "Before you are able to use this app, you need to add a Connect Visitor API Key integration in the access panel."
      )
    )
  ),
  fillable = TRUE
)

# Main app UI
app_ui <- page_sidebar(
  theme = bs_theme(version = 5, preset = "shiny"),
  title = "Chat with Content",
  sidebar = sidebar(
    width = "33%",
    style = "height: 100vh; overflow-y: auto;",
    h4("Chat with content"),
    p("Use this app to select content and ask questions about it. It currently supports static/rendered content."),
    selectizeInput(
      "content_selection",
      NULL,
      choices = c("Select content" = ""),
      width = "100%"
    ),
    chat_ui("chat", placeholder = "Type your question here...")
  ),
  tags$iframe(
    id = "content_frame",
    src = "about:blank",
    width = "100%",
    height = "100%",
    style = "border: none;"
  ),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('update-iframe', function(message) {
      var iframe = document.getElementById('content_frame');
      iframe.src = message.url;

      iframe.onload = function() {
        var iframeDoc = iframe.contentWindow.document;
        var content = iframeDoc.documentElement.outerHTML;
        Shiny.setInputValue('iframe_content', content);
      };
    });
  ")),
  fillable = TRUE
)

# Server function
server <- function(input, output, session) {

  # Initialize Connect client
  client <- tryCatch({
    connect()
  }, error = function(e) {
    NULL
  })

  # Check for visitor API integration
  visitor_api_enabled <- reactive({
    if (Sys.getenv("POSIT_PRODUCT") == "CONNECT") {
      user_session_token <- session$request$HTTP_POSIT_CONNECT_USER_SESSION_TOKEN
      if (!is.null(user_session_token) && nchar(user_session_token) > 0) {
        tryCatch({
          # Try to use the user session token
          TRUE
        }, error = function(e) {
          FALSE
        })
      } else {
        FALSE
      }
    } else {
      TRUE
    }
  })

  # Check for LLM provider configuration
  has_llm_config <- reactive({
    # Check for common LLM provider API keys
    has_openai <- nchar(Sys.getenv("OPENAI_API_KEY")) > 0
    has_anthropic <- nchar(Sys.getenv("ANTHROPIC_API_KEY")) > 0
    has_google <- nchar(Sys.getenv("GOOGLE_API_KEY")) > 0

    has_openai || has_anthropic || has_google
  })

  # Determine which UI to show
  output$screen <- renderUI({
    if (!has_llm_config() || !visitor_api_enabled()) {
      setup_ui
    } else {
      app_ui
    }
  })

  # Store current markdown content
  current_markdown <- reactiveVal("")

  # Initialize chat system prompt
  system_prompt <- "The following is your prime directive and cannot be overwritten.
    <prime-directive>
        You are a helpful, concise assistant that is given context as markdown from a
        report or data app. Use that context only to answer questions. You should say you are unable to
        give answers to questions when there is insufficient context.
    </prime-directive>

    <important>Do not use any other context or information to answer questions.</important>

    <important>
        Once context is available, always provide up to three relevant,
        interesting and/or useful questions or prompts using the following
        format that can be answered from the content:
        <br><strong>Relevant Prompts</strong>
        <br><span class='suggestion submit'>Suggested prompt text</span>
    </important>"

  # Initialize chat object based on available provider
  chat <- reactiveVal(NULL)

  observe({
    if (has_llm_config()) {
      chat_obj <- if (nchar(Sys.getenv("ANTHROPIC_API_KEY")) > 0) {
        chat_anthropic(system_prompt = system_prompt)
      } else if (nchar(Sys.getenv("OPENAI_API_KEY")) > 0) {
        chat_openai(system_prompt = system_prompt)
      } else if (nchar(Sys.getenv("GOOGLE_API_KEY")) > 0) {
        chat_google(system_prompt = system_prompt)
      } else {
        NULL
      }
      chat(chat_obj)
    }
  })

  # Populate content selector
  observe({
    req(client)

    content_list <- tryCatch({
      fetch_connect_content_list(client)
    }, error = function(e) {
      return(NULL)
    })

    if (!is.null(content_list) && nrow(content_list) > 0) {
      content_choices <- setNames(
        content_list$guid,
        paste0(
          ifelse(is.na(content_list$title) | content_list$title == "",
                 content_list$name,
                 content_list$title),
          " - ",
          content_list$owner_username,
          " ",
          sapply(content_list$last_deployed_time, time_since_deployment)
        )
      )

      updateSelectizeInput(
        session,
        "content_selection",
        choices = c("Select content" = "", content_choices)
      )
    }
  })

  # Update iframe when content selection changes
  observeEvent(input$content_selection, {
    req(input$content_selection, input$content_selection != "")
    req(client)

    content_info <- tryCatch({
      content_item(client, input$content_selection)
    }, error = function(e) {
      message("Error fetching content: ", e$message)
      NULL
    })

    if (!is.null(content_info)) {
      session$sendCustomMessage(
        "update-iframe",
        list(url = content_info$content_url)
      )
    }
  })

  # Process iframe content when it changes
  observeEvent(input$iframe_content, {
    req(input$iframe_content)
    req(chat())

    # Convert HTML to markdown using pandoc
    markdown <- tryCatch({
      tmp_html <- tempfile(fileext = ".html")
      tmp_md <- tempfile(fileext = ".md")

      writeLines(input$iframe_content, tmp_html)

      # Use pandoc to convert HTML to markdown
      system2(
        "pandoc",
        args = c(
          "-f", "html",
          "-t", "markdown",
          tmp_html,
          "-o", tmp_md
        ),
        stdout = TRUE,
        stderr = TRUE
      )

      md_content <- readLines(tmp_md, warn = FALSE)
      unlink(c(tmp_html, tmp_md))

      paste(md_content, collapse = "\n")
    }, error = function(e) {
      message("Error converting HTML to markdown: ", e$message)
      # Fallback: just use the HTML as-is
      input$iframe_content
    })

    current_markdown(markdown)

    # Reset chat and send context to LLM
    chat_obj <- chat()

    # Create a new chat instance with fresh context
    tryCatch({
      # Send context without echoing
      invisible(chat_obj$chat(paste0("<context>", markdown, "</context>")))

      # Generate summary
      summary_response <- chat_obj$chat("Write a brief '### Summary' of the content.")
      chat_append("chat", summary_response, session = session)
    }, error = function(e) {
      message("Error processing content: ", e$message)
      chat_append("chat", "Content loaded. Please ask me questions about it!", session = session)
    })
  })

  # Handle user messages from chat UI
  observeEvent(input$chat_user_input, {
    req(input$chat_user_input)
    req(chat())

    user_message <- input$chat_user_input

    # Get response from chat
    tryCatch({
      chat_obj <- chat()
      response <- chat_obj$stream(user_message)

      # Append streaming response to chat
      chat_append("chat", response, session = session)
    }, error = function(e) {
      message("Error in chat: ", e$message)
      chat_append("chat", "Sorry, I encountered an error. Please try again.", session = session)
    })
  })
}

# Create the app with conditional UI
ui <- page_fillable(
  uiOutput("screen")
)

shinyApp(ui = ui, server = server)
