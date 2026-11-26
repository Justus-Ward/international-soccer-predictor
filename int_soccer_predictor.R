library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(bslib)

# Data & model setup

results <- read.csv("results.csv") #Credit to https://github.com/martj42/international_results/blob/master/results.csv

new_results <- results %>%
  filter(date >= as.Date("2022-01-01"))

current_rankings <- read.csv("fifa_rankings.csv")

new_results <- new_results |>
  left_join(current_rankings |> select(team, points), by = c("home_team" = "team")) |>
  rename(home_rating = points) |>
  left_join(current_rankings |> select(team, points), by = c("away_team" = "team")) |>
  rename(away_rating = points)

team_results <- bind_rows(
  new_results |>
    filter(home_team %in% current_rankings$team) |>
    select(team = home_team, goals = home_score, opp_goals = away_score, opp_rating = away_rating),
  new_results |>
    filter(away_team %in% current_rankings$team) |>
    select(team = away_team, goals = away_score, opp_goals = home_score, opp_rating = home_rating)
)

team_models <- team_results |>
  group_by(team) |>
  nest() |>
  mutate(
    goals_lm     = map(data, \(df) lm(goals ~ opp_rating, data = df)),
    opp_goals_lm = map(data, \(df) lm(opp_goals ~ opp_rating, data = df))
  )

team_coeffs <- team_models |>
  mutate(
    goals_coefs     = map(goals_lm,     \(m) tidy(m)),
    opp_goals_coefs = map(opp_goals_lm, \(m) tidy(m))
  ) |>
  mutate(
    goals_intercept = map_dbl(goals_coefs,     \(df) df$estimate[df$term == "(Intercept)"]),
    goals_slope     = map_dbl(goals_coefs,     \(df) df$estimate[df$term == "opp_rating"]),
    opp_intercept   = map_dbl(opp_goals_coefs, \(df) df$estimate[df$term == "(Intercept)"]),
    opp_slope       = map_dbl(opp_goals_coefs, \(df) df$estimate[df$term == "opp_rating"])
  ) |>
  select(team, goals_intercept, goals_slope, opp_intercept, opp_slope)

HOME_ADVANTAGE <- 1.15

predict_goals <- function(home_team, away_team, team_coeffs, rankings) {
  home <- team_coeffs |> filter(team == home_team)
  away <- team_coeffs |> filter(team == away_team)
  home_rating <- rankings$points[rankings$team == home_team]
  away_rating <- rankings$points[rankings$team == away_team]
  home_boost <- HOME_ADVANTAGE
  away_suppression <- 1/HOME_ADVANTAGE
  lambda <- (home$goals_intercept + home$goals_slope * away_rating) * home_boost
  mu     <- (away$goals_intercept + away$goals_slope * home_rating) * away_suppression
  list(lambda = max(lambda, 0.05), mu = max(mu, 0.05))
}

scoreline_probs <- function(lambda, mu, max_goals = 10) {
  scores      <- expand.grid(home_goals = 0:max_goals, away_goals = 0:max_goals)
  scores$prob <- dpois(scores$home_goals, lambda) * dpois(scores$away_goals, mu)
  scores$prob <- scores$prob / sum(scores$prob)
  scores
}

match_probs <- function(home_team, away_team, team_coeffs, rankings) {
  goals  <- predict_goals(home_team, away_team, team_coeffs, rankings)
  scores <- scoreline_probs(goals$lambda, goals$mu)
  list(
    home_win = sum(scores$prob[scores$home_goals >  scores$away_goals]),
    draw     = sum(scores$prob[scores$home_goals == scores$away_goals]),
    away_win = sum(scores$prob[scores$home_goals <  scores$away_goals]),
    lambda   = goals$lambda,
    mu       = goals$mu
  )
}


# Knockout function during tournaments
ko_winner <- function(t1, t2) {
  p   <- match_probs(t1, t2, team_coeffs, current_rankings)
  p1  <- p$home_win + p$draw / 2
  p2  <- p$away_win + p$draw / 2
  list(
    t1 = t1, t2 = t2,
    p1 = round(p1 * 100, 1),
    p2 = round(p2 * 100, 1),
    winner = if (p1 >= p2) t1 else t2
  )
}

all_teams <- sort(unique(team_coeffs$team))

# UI 
# ---------------- PART 1: theme + css ----------------

app_theme <- bs_theme(
  version      = 5,
  bg           = "#f4f6fa",
  fg           = "#1a1f2b",
  primary      = "#1b6ef3",
  base_font    = font_google("Inter"),
  heading_font = font_google("Poppins"),
  "border-radius" = "0.75rem"
)
# If font_google() ever fails on deploy, use:
# base_font = font_collection("system-ui", "Segoe UI", "sans-serif")

app_css <- "
  .card { border: none; box-shadow: 0 1px 3px rgba(16,24,40,.08), 0 6px 16px rgba(16,24,40,.05); }
  .card-header { font-weight: 600; background: transparent; border-bottom: 1px solid #e8ecf2; }
  .bslib-sidebar-layout > .sidebar { background: #fff; }

  /* flags */
  .flag img { border-radius: 6px !important; box-shadow: 0 2px 8px rgba(16,24,40,.25); }

  /* matchup header */
  .matchup { display:flex; align-items:center; justify-content:center; gap:40px; padding:12px 0 4px; }
  .mu-team { display:flex; flex-direction:column; align-items:center; gap:8px; min-width:140px; }
  .mu-name { font-size:16px; font-weight:600; text-align:center; }
  .mu-prob { font-size:34px; font-weight:700; line-height:1; font-variant-numeric:tabular-nums; }
  .mu-prob.home { color:#1b6ef3; }
  .mu-prob.away { color:#f97316; }
  .mu-mid { display:flex; flex-direction:column; align-items:center; gap:4px; color:#8a94a6; }
  .mu-draw-label { font-size:12px; font-weight:600; letter-spacing:.08em; text-transform:uppercase; }
  .mu-draw { font-size:22px; font-weight:600; font-variant-numeric:tabular-nums; }

  /* stacked probability bar */
  .prob-bar { display:flex; height:30px; border-radius:999px; overflow:hidden; margin:16px 8px 6px; background:#e8ecf2; }
  .prob-bar .seg { display:flex; align-items:center; justify-content:center; color:#fff;
                   font-size:12px; font-weight:600; transition:width .4s ease; }
  .seg.home { background:#1b6ef3; } .seg.draw { background:#9aa4b2; } .seg.away { background:#f97316; }

  /* scoreboard */
  .scoreboard { display:flex; align-items:center; justify-content:center; gap:28px; padding:8px 0; }
  .sb-team { display:flex; flex-direction:column; align-items:center; gap:6px; min-width:130px; }
  .sb-score { display:flex; align-items:center; gap:14px; color:#fff; border-radius:14px; padding:12px 28px;
              background:linear-gradient(135deg,#1a1a2e,#2b2d57); box-shadow:0 6px 18px rgba(26,26,46,.35); }
  .sb-num { font-size:36px; font-weight:700; font-variant-numeric:tabular-nums; }
  .sb-dash { font-size:28px; font-weight:300; color:#aab; }
  .sb-caption { text-align:center; color:#8a94a6; font-size:12px; margin-top:2px; }

  /* matrix heatmap */
  .matrix-wrap { overflow-x:auto; }
  .matrix-wrap table { border-collapse:separate; border-spacing:3px; font-size:13px; margin:0 auto; }
  .matrix-wrap th, .matrix-wrap td { padding:7px 10px; text-align:center; border-radius:6px; min-width:44px;
                                     font-variant-numeric:tabular-nums; }
  .matrix-wrap th { background:#eef1f6; font-weight:600; }
  .matrix-wrap .dim-header { background:#e3ecfd; color:#1b4fb3; font-weight:700; }
  .matrix-wrap td.best { outline:2px solid #f97316; outline-offset:1px; font-weight:700; }
  .matrix-note { text-align:center; color:#6b7686; font-size:13px; margin-bottom:10px; }

  .empty-state { text-align:center; color:#8a94a6; padding:56px 12px; }
  .empty-state .big { font-size:44px; }

  @media (max-width: 640px) {
    .matchup { gap:16px; } .mu-team { min-width:90px; } .mu-prob { font-size:26px; }
    .sb-score { padding:10px 16px; } .sb-num { font-size:28px; }
  }
"


# ---------------- PART 2: ui ----------------

ui <- page_sidebar(
  title     = "International Soccer Predictor",
  theme     = app_theme,
  fillable  = FALSE,
  sidebar   = sidebar(
    width = 320,
    selectInput("home_team", "Home team", choices = all_teams, selected = "United States"),
    selectInput("away_team", "Away team", choices = all_teams, selected = "Mexico"),
    actionButton("swap", "\u21C4  Swap teams", class = "btn-outline-secondary btn-sm w-100"),
    actionButton("predict", "Predict match", class = "btn-primary w-100")
  ),
  tags$head(tags$style(HTML(app_css))),
  
  # shown before the first prediction
  conditionalPanel(
    "input.predict == 0",
    card(div(class = "empty-state",
             div(class = "big", "\u26BD"),
             h4("Select two teams and Predict"),
             p("You'll get win probabilities, projected goals and a scoreline matrix.")))
  ),
  
  # shown after the first prediction
  conditionalPanel(
    "input.predict > 0",
    card(card_header("Win probabilities"), htmlOutput("prob_display")),
    card(card_header("Projected goals"),   htmlOutput("goals_display")),
    card(card_header("Scoreline probability matrix (in %)"), htmlOutput("matrix_display"))
  )
)


# Server 

server <- function(input, output, session) {
  
  # Match predictor
  result <- eventReactive(input$predict, {
    req(input$home_team, input$away_team)
    validate(need(input$home_team != input$away_team, "Please select two different teams"))
    home   <- input$home_team
    away   <- input$away_team
    probs  <- match_probs(home, away, team_coeffs, current_rankings)
    goals  <- predict_goals(home, away, team_coeffs, current_rankings)
    scores <- scoreline_probs(goals$lambda, goals$mu)
    sub    <- scores[scores$home_goals <= 8 & scores$away_goals <= 8, ]
    mat    <- tapply(sub$prob * 100, list(sub$home_goals, sub$away_goals), sum)
    dimnames(mat) <- list(as.character(0:8), as.character(0:8))
    list(home = home, away = away, probs = probs, goals = goals, matrix = round(mat, 3))
  })
  
  # Flag helper
  flag_codes <- c(
    "Afghanistan"="af","Albania"="al","Algeria"="dz","American Samoa"="as",
    "Andorra"="ad","Angola"="ao","Anguilla"="ai","Antigua and Barbuda"="ag",
    "Argentina"="ar","Armenia"="am","Aruba"="aw","Australia"="au",
    "Austria"="at","Azerbaijan"="az","Bahamas"="bs","Bahrain"="bh",
    "Bangladesh"="bd","Barbados"="bb","Belarus"="by","Belgium"="be",
    "Belize"="bz","Benin"="bj","Bermuda"="bm","Bhutan"="bt","Bolivia"="bo",
    "Bosnia and Herzegovina"="ba","Botswana"="bw","Brazil"="br",
    "British Virgin Islands"="vg","Brunei"="bn","Bulgaria"="bg",
    "Burkina Faso"="bf","Burundi"="bi","Cambodia"="kh","Cameroon"="cm",
    "Canada"="ca","Cape Verde"="cv","Cayman Islands"="ky",
    "Central African Republic"="cf","Chad"="td","Chile"="cl","China"="cn",
    "Colombia"="co","Comoros"="km","Congo"="cg","Cook Islands"="ck",
    "Costa Rica"="cr","Croatia"="hr","Cuba"="cu","Curaçao"="cw",
    "Cyprus"="cy","Czechia"="cz","Denmark"="dk","Djibouti"="dj",
    "Dominica"="dm","Dominican Republic"="do","DR Congo"="cd",
    "East Timor"="tl","Ecuador"="ec","Egypt"="eg","El Salvador"="sv",
    "England"="gb-eng","Equatorial Guinea"="gq","Eritrea"="er",
    "Estonia"="ee","Eswatini"="sz","Ethiopia"="et","Faroe Islands"="fo",
    "Fiji"="fj","Finland"="fi","France"="fr","Gabon"="ga","Gambia"="gm",
    "Georgia"="ge","Germany"="de","Ghana"="gh","Gibraltar"="gi",
    "Greece"="gr","Grenada"="gd","Guam"="gu","Guatemala"="gt","Guinea"="gn",
    "Guinea-Bissau"="gw","Guyana"="gy","Haiti"="ht","Honduras"="hn",
    "Hong Kong"="hk","Hungary"="hu","Iceland"="is","India"="in",
    "Indonesia"="id","Iran"="ir","Iraq"="iq","Ireland"="ie","Israel"="il",
    "Italy"="it","Ivory Coast"="ci","Jamaica"="jm","Japan"="jp",
    "Jordan"="jo","Kazakhstan"="kz","Kenya"="ke","Kosovo"="xk",
    "Kuwait"="kw","Kyrgyzstan"="kg","Laos"="la","Latvia"="lv",
    "Lebanon"="lb","Lesotho"="ls","Liberia"="lr","Libya"="ly",
    "Liechtenstein"="li","Lithuania"="lt","Luxembourg"="lu","Macau"="mo",
    "Madagascar"="mg","Malawi"="mw","Malaysia"="my","Maldives"="mv",
    "Mali"="ml","Malta"="mt","Mauritania"="mr","Mauritius"="mu",
    "Mexico"="mx","Moldova"="md","Mongolia"="mn","Montenegro"="me",
    "Montserrat"="ms","Morocco"="ma","Mozambique"="mz","Myanmar"="mm",
    "Namibia"="na","Nepal"="np","Netherlands"="nl","New Caledonia"="nc",
    "New Zealand"="nz","Nicaragua"="ni","Niger"="ne","Nigeria"="ng",
    "North Korea"="kp","North Macedonia"="mk","Northern Ireland"="gb-nir",
    "Norway"="no","Oman"="om","Pakistan"="pk","Palestine"="ps",
    "Panama"="pa","Papua New Guinea"="pg","Paraguay"="py","Peru"="pe",
    "Philippines"="ph","Poland"="pl","Portugal"="pt","Puerto Rico"="pr",
    "Qatar"="qa","Romania"="ro","Russia"="ru","Rwanda"="rw",
    "Saint Kitts and Nevis"="kn","Saint Lucia"="lc","Samoa"="ws",
    "San Marino"="sm","Sao Tome and Principe"="st","Saudi Arabia"="sa",
    "Scotland"="gb-sct","Senegal"="sn","Serbia"="rs","Seychelles"="sc",
    "Sierra Leone"="sl","Singapore"="sg","Slovakia"="sk","Slovenia"="si",
    "Solomon Islands"="sb","Somalia"="so","South Africa"="za",
    "South Korea"="kr","South Sudan"="ss","Spain"="es","Sri Lanka"="lk",
    "St Vincent & Grenadines"="vc","Sudan"="sd","Suriname"="sr",
    "Sweden"="se","Switzerland"="ch","Syria"="sy","Tahiti"="pf",
    "Taiwan"="tw","Tajikistan"="tj","Tanzania"="tz","Thailand"="th",
    "Togo"="tg","Tonga"="to","Trinidad and Tobago"="tt","Tunisia"="tn",
    "Turkey"="tr","Turkmenistan"="tm","Turks and Caicos Islands"="tc",
    "Uganda"="ug","Ukraine"="ua","United Arab Emirates"="ae",
    "United States"="us","Uruguay"="uy","US Virgin Islands"="vi",
    "Uzbekistan"="uz","Vanuatu"="vu","Venezuela"="ve","Vietnam"="vn",
    "Wales"="gb-wls","Yemen"="ye","Zambia"="zm","Zimbabwe"="zw"
  )
  
  # Returns just the ISO code for use in bracket img tags
  team_flag_code <- function(team) {
    code <- unname(flag_codes[team])
    ifelse(is.na(code), "", tolower(code))
  }
  
  team_flag <- function(team, size = 48) {
    code <- unname(flag_codes[team])
    ifelse(
      is.na(code),
      paste0('<span style="font-size:', round(size * 0.75), 'px">&#127987;</span>'),
      paste0('<img src="https://flagcdn.com/', size, 'x', round(size * 0.75), '/', tolower(code),
             '.png" style="border-radius:2px;vertical-align:middle" alt="', team, '">')
    )
  }
  
  observeEvent(input$swap, {
    h <- input$home_team
    a <- input$away_team
    updateSelectInput(session, "home_team", selected = a)
    updateSelectInput(session, "away_team", selected = h)
  })
  
  output$prob_display <- renderUI({
    r  <- result()
    hp <- r$probs$home_win * 100
    dp <- r$probs$draw     * 100
    ap <- r$probs$away_win * 100
    lab <- function(p) if (p >= 9) paste0(round(p, 1), "%") else ""   # hide label on tiny segments
    HTML(paste0('
    <div class="matchup">
      <div class="mu-team">
        <div class="flag">', team_flag(r$home, 64), '</div>
        <div class="mu-name">', r$home, '</div>
        <div class="mu-prob home">', round(hp, 1), '%</div>
      </div>
      <div class="mu-mid">
        <div class="mu-draw-label">Draw</div>
        <div class="mu-draw">', round(dp, 1), '%</div>
      </div>
      <div class="mu-team">
        <div class="flag">', team_flag(r$away, 64), '</div>
        <div class="mu-name">', r$away, '</div>
        <div class="mu-prob away">', round(ap, 1), '%</div>
      </div>
    </div>
    <div class="prob-bar">
      <div class="seg home" style="width:', hp, '%">', lab(hp), '</div>
      <div class="seg draw" style="width:', dp, '%">', lab(dp), '</div>
      <div class="seg away" style="width:', ap, '%">', lab(ap), '</div>
    </div>'))
  })
  
  output$goals_display <- renderUI({
    r <- result()
    HTML(paste0('
    <div class="scoreboard">
      <div class="sb-team">
        <div class="flag">', team_flag(r$home, 64), '</div>
        <div class="mu-name">', r$home, '</div>
      </div>
      <div class="sb-score">
        <span class="sb-num">', round(r$goals$lambda, 2), '</span>
        <span class="sb-dash">&ndash;</span>
        <span class="sb-num">', round(r$goals$mu, 2), '</span>
      </div>
      <div class="sb-team">
        <div class="flag">', team_flag(r$away, 64), '</div>
        <div class="mu-name">', r$away, '</div>
      </div>
    </div>
    <div class="sb-caption">Expected goals</div>'))
  })
  
  output$matrix_display <- renderUI({
    r   <- result()
    mat <- r$matrix
    mat[is.na(mat)] <- 0
    n   <- 0:8
    mx  <- max(mat)
    best <- which(mat == mx, arr.ind = TRUE)[1, ]
    
    cell <- function(v, is_best) {
      a  <- v / mx
      bg <- sprintf("rgba(27,110,243,%.3f)", 0.05 + 0.9 * a)
      fg <- if (a > 0.5) "#fff" else "#1a1f2b"
      sprintf('<td class="%s" style="background:%s;color:%s">%s</td>',
              if (is_best) "best" else "", bg, fg, formatC(v, format = "f", digits = 1))
    }
    
    hf <- team_flag(r$home, 20)
    af <- team_flag(r$away, 20)
    hdr1 <- paste0('<tr><th class="dim-header" rowspan="2" style="min-width:120px">',
                   hf, ' ', r$home, '<br><span style="font-weight:400;font-size:11px">&#8595; goals</span></th>',
                   '<th class="dim-header" colspan="9">', af, ' ', r$away, ' &#8594; goals</th></tr>')
    hdr2 <- paste0('<tr>', paste0('<th>', n, '</th>', collapse = ''), '</tr>')
    rows <- vapply(seq_len(nrow(mat)), function(i) {
      cells <- vapply(seq_len(ncol(mat)),
                      function(j) cell(mat[i, j], i == best[1] && j == best[2]),
                      character(1))
      paste0('<tr><th>', n[i], '</th>', paste(cells, collapse = ''), '</tr>')
    }, character(1))
    
    HTML(paste0(
      '<div class="matrix-note">Most likely scoreline: <b>', n[best[1]], ' &ndash; ', n[best[2]],
      '</b> (', formatC(mx, format = "f", digits = 1), '%)</div>',
      '<div class="matrix-wrap"><table><thead>', hdr1, hdr2, '</thead><tbody>',
      paste(rows, collapse = ''), '</tbody></table></div>'))
  })
  
  
  
}

shinyApp(ui, server)
