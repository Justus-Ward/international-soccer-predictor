library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)

# Data & model setup

results <- read.csv("results.csv")

new_results <- results %>%
  filter(date >= as.Date("2022-01-01"))

current_rankings_txt <- "team,points
Argentina,1970.37
Spain,1995.88
France,1948.97
England,1922.83
Portugal,1787.85
Brazil,1804.92
Morocco,1803.99
Netherlands,1775.54
Belgium,1778.36
Germany,1726.22
Croatia,1723.05
Italy,1704.73
Colombia,1739.89
Mexico,1754.30
Senegal,1653.43
Uruguay,1634.70
United States,1690.33
Japan,1673.68
Switzerland,1710.88
Iran,1609.85
Denmark,1619.47
Turkey,1582.54
Ecuador,1592.59
Austria,1598.82
South Korea,1558.72
Nigeria,1585.02
Australia,1581.35
Algeria,1576.80
Egypt,1597.04
Canada,1571.34
Norway,1651.29
Ukraine,1549.29
Ivory Coast,1565.47
Panama,1478.41
Russia,1529.60
Poland,1526.18
Wales,1516.95
Sweden,1525.58
Hungary,1506.39
Czech Republic,1467.26
Paraguay,1542.48
Scotland,1491.22
Serbia,1502.13
Cameroon,1481.24
Tunisia,1426.58
DR Congo,1495.48
Slovakia,1473.66
Greece,1473.19
Venezuela,1469.18
Uzbekistan,1409.73
Chile,1458.20
Peru,1457.69
Costa Rica,1456.03
Romania,1455.89
Mali,1455.59
Qatar,1411.06
Iraq,1404.17
Republic of Ireland,1441.10
Slovenia,1441.09
South Africa,1451.24
Saudi Arabia,1425.52
Burkina Faso,1406.99
Jordan,1350.41
Bosnia and Herzegovina,1408.93
Honduras,1378.97
Albania,1376.03
Cape Verde,1402.97
United Arab Emirates,1370.47
North Macedonia,1369.16
Northern Ireland,1365.30
Jamaica,1357.84
Georgia,1355.26
Ghana,1387.00
Iceland,1342.77
Finland,1341.92
Israel,1333.90
Bolivia,1326.00
Kosovo,1319.12
Oman,1306.90
Montenegro,1301.98
Guinea,1295.60
Curaçao,1285.64
Haiti,1264.58
Syria,1283.05
New Zealand,1269.80
Gabon,1272.51
Bulgaria,1271.68
Angola,1265.58
Uganda,1264.09
Zambia,1255.82
China,1254.81
Bahrain,1254.41
Benin,1252.17
Thailand,1250.80
Palestine,1243.71
Belarus,1242.88
Guatemala,1238.74
Luxembourg,1232.82
Vietnam,1225.68
El Salvador,1225.34
Tajikistan,1224.19
Trinidad and Tobago,1219.59
Mozambique,1218.62
Madagascar,1202.69
Equatorial Guinea,1195.20
Kyrgyzstan,1192.16
Armenia,1189.63
Comoros,1187.91
Kenya,1185.08
Libya,1182.08
Kazakhstan,1180.78
Tanzania,1180.27
Mauritania,1176.68
Niger,1175.33
Lebanon,1172.22
Gambia,1159.64
Sudan,1157.22
Indonesia,1157.14
Togo,1152.76
North Korea,1151.05
Namibia,1148.84
Sierra Leone,1147.56
Faroe Islands,1136.59
Cyprus,1133.25
Suriname,1132.43
Azerbaijan,1132.00
Estonia,1130.64
Rwanda,1126.62
Malawi,1122.05
Zimbabwe,1119.78
Nicaragua,1114.63
Guinea-Bissau,1108.38
Kuwait,1106.47
Congo,1105.96
Philippines,1100.95
Malaysia,1086.22
Latvia,1085.66
India,1084.93
Central African Republic,1080.82
Liberia,1080.44
Turkmenistan,1078.65
Burundi,1078.01
Ethiopia,1077.52
Dominican Republic,1076.50
Yemen,1065.24
Lesotho,1064.29
Botswana,1063.63
Singapore,1057.95
Lithuania,1056.85
Guyana,1049.32
New Caledonia,1036.95
St Kitts and Nevis,1036.33
Solomon Islands,1031.89
Puerto Rico,1024.30
Fiji,1024.17
Hong Kong China,1024.16
Tahiti,1019.04
Myanmar,1010.91
Moldova,1008.24
Vanuatu,1002.53
Malta,992.79
Antigua and Barbuda,986.58
Grenada,981.82
Cuba,981.42
Eswatini,979.01
St Lucia,976.71
Bermuda,975.05
Papua New Guinea,974.90
Afghanistan,971.20
South Sudan,970.94
St Vincent and the Grenadines,968.27
Andorra,946.43
Maldives,943.92
Chinese Taipei,923.78
Cambodia,922.32
Montserrat,916.75
Nepal,914.54
Mauritius,911.49
Barbados,909.89
Belize,907.00
Bangladesh,902.93
Dominica,897.69
Chad,896.85
Eritrea,887.06
Laos,885.03
Cook Islands,877.53
Sri Lanka,876.86
Samoa,876.41
Aruba,875.61
Mongolia,874.47
American Samoa,871.61
Bhutan,870.81
Macau,858.03
Brunei Darussalam,857.73
São Tomé and Príncipe,855.44
Djibouti,853.58
Cayman Islands,850.06
Somalia,839.17
Pakistan,837.15
Tonga,835.64
Timor-Leste,831.00
Gibraltar,820.26
Guam,819.54
Seychelles,804.16
Turks and Caicos Islands,803.98
Liechtenstein,797.70
Bahamas,786.82
US Virgin Islands,779.76
British Virgin Islands,777.41
Anguilla,760.25
San Marino,721.20"

current_rankings <- read.csv(text = current_rankings_txt)

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

ui <- fluidPage(
  titlePanel("International Soccer Predictor"),
             br(),
             sidebarLayout(
               sidebarPanel(
                 selectInput("home_team", "Home Team", choices = all_teams, selected = "United States"),
                 selectInput("away_team", "Away Team", choices = all_teams, selected = "Mexico"),
                 actionButton("predict", "Predict", class = "btn-primary", width = "100%")
               ),
               mainPanel(
                 tags$style(HTML("
            .match-header { display:flex; align-items:center; justify-content:center; gap:32px; margin:24px 0 8px 0; font-family:sans-serif; }
            .team-block { display:flex; flex-direction:column; align-items:center; gap:6px; }
            .team-name { font-size:15px; font-weight:600; color:#333; }
            .team-prob { font-size:22px; font-weight:700; color:#2c7be5; }
            .vs-block { display:flex; flex-direction:column; align-items:center; gap:4px; color:#888; }
            .vs-label { font-size:13px; font-weight:500; }
            .draw-prob { font-size:18px; font-weight:600; color:#888; }
            .matrix-wrap { overflow-x:auto; margin-top:16px; }
            .matrix-wrap table { border-collapse:collapse; font-size:13px; font-family:monospace; }
            .matrix-wrap th, .matrix-wrap td { border:1px solid #ddd; padding:5px 9px; text-align:center; }
            .matrix-wrap th { background:#f5f5f5; font-weight:600; }
            .matrix-wrap .dim-header { background:#e8f0fe; font-weight:700; font-size:14px; }
            .scoreboard { display:flex; align-items:center; justify-content:center; gap:24px; margin:20px 0; font-family:sans-serif; }
            .sb-team { display:flex; flex-direction:column; align-items:center; gap:4px; min-width:120px; }
            .sb-left { align-items:flex-end; }
            .sb-right { align-items:flex-start; }
            .sb-score { display:flex; align-items:center; gap:10px; background:#1a1a2e; color:#fff; border-radius:10px; padding:10px 22px; }
            .sb-num { font-size:32px; font-weight:700; font-variant-numeric:tabular-nums; }
            .sb-dash { font-size:28px; font-weight:300; color:#aaa; }
            .section-label { font-size:16px; font-weight:600; color:#444; margin:28px 0 4px 0; border-bottom:2px solid #eee; padding-bottom:4px; }
          ")),
                 div(class="section-label", "Win Probabilities"),
                 htmlOutput("prob_display"),
                 div(class="section-label", "Projected Goals"),
                 htmlOutput("goals_display"),
                 div(class="section-label", "Scoreline Probability Matrix (in %)"),
                 htmlOutput("matrix_display")
               )
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
  team_flag <- function(team, size = 48) {
    codes <- c(
      "Afghanistan"="af","Albania"="al","Algeria"="dz","Angola"="ao",
      "Antigua and Barbuda"="ag","Argentina"="ar","Armenia"="am","Aruba"="aw",
      "Australia"="au","Austria"="at","Azerbaijan"="az","Bahamas"="bs",
      "Bahrain"="bh","Bangladesh"="bd","Barbados"="bb","Belarus"="by",
      "Belgium"="be","Belize"="bz","Benin"="bj","Bhutan"="bt","Bolivia"="bo",
      "Bosnia and Herzegovina"="ba","Botswana"="bw","Brazil"="br",
      "Brunei Darussalam"="bn","Bulgaria"="bg","Burkina Faso"="bf",
      "Burundi"="bi","Cambodia"="kh","Cameroon"="cm","Canada"="ca",
      "Cape Verde"="cv","Central African Republic"="cf","Chad"="td",
      "Chile"="cl","China"="cn","Colombia"="co","Comoros"="km","Congo"="cg",
      "Costa Rica"="cr","Croatia"="hr","Cuba"="cu","Curaçao"="cw",
      "Cyprus"="cy","Czech Republic"="cz","Denmark"="dk","Djibouti"="dj",
      "Dominican Republic"="do","DR Congo"="cd","Ecuador"="ec","Egypt"="eg",
      "El Salvador"="sv","England"="gb-eng","Equatorial Guinea"="gq",
      "Eritrea"="er","Estonia"="ee","Eswatini"="sz","Ethiopia"="et",
      "Faroe Islands"="fo","Fiji"="fj","Finland"="fi","France"="fr",
      "Gabon"="ga","Gambia"="gm","Georgia"="ge","Germany"="de","Ghana"="gh",
      "Greece"="gr","Guatemala"="gt","Guinea"="gn","Guinea-Bissau"="gw",
      "Guyana"="gy","Haiti"="ht","Honduras"="hn","Hong Kong China"="hk",
      "Hungary"="hu","Iceland"="is","India"="in","Indonesia"="id","Iran"="ir",
      "Iraq"="iq","Republic of Ireland"="ie","Israel"="il","Italy"="it",
      "Ivory Coast"="ci","Jamaica"="jm","Japan"="jp","Jordan"="jo",
      "Kazakhstan"="kz","Kenya"="ke","Kosovo"="xk","Kuwait"="kw",
      "Kyrgyzstan"="kg","Laos"="la","Latvia"="lv","Lebanon"="lb",
      "Lesotho"="ls","Liberia"="lr","Libya"="ly","Liechtenstein"="li",
      "Lithuania"="lt","Luxembourg"="lu","Macau"="mo","Madagascar"="mg",
      "Malawi"="mw","Malaysia"="my","Maldives"="mv","Mali"="ml","Malta"="mt",
      "Mauritania"="mr","Mauritius"="mu","Mexico"="mx","Moldova"="md",
      "Mongolia"="mn","Montenegro"="me","Morocco"="ma","Mozambique"="mz",
      "Myanmar"="mm","Namibia"="na","Nepal"="np","Netherlands"="nl",
      "New Zealand"="nz","Nicaragua"="ni","Niger"="ne","Nigeria"="ng",
      "North Korea"="kp","North Macedonia"="mk","Northern Ireland"="gb-nir",
      "Norway"="no","Oman"="om","Pakistan"="pk","Palestine"="ps",
      "Panama"="pa","Paraguay"="py","Peru"="pe","Philippines"="ph",
      "Poland"="pl","Portugal"="pt","Qatar"="qa","Romania"="ro","Russia"="ru",
      "Rwanda"="rw","Saudi Arabia"="sa","Scotland"="gb-sct","Senegal"="sn",
      "Serbia"="rs","Sierra Leone"="sl","Singapore"="sg","Slovakia"="sk",
      "Slovenia"="si","Somalia"="so","South Africa"="za","South Korea"="kr",
      "South Sudan"="ss","Spain"="es","Sri Lanka"="lk","Sudan"="sd",
      "Suriname"="sr","Sweden"="se","Switzerland"="ch","Syria"="sy",
      "Tajikistan"="tj","Tanzania"="tz","Thailand"="th","Togo"="tg",
      "Trinidad and Tobago"="tt","Tunisia"="tn","Turkey"="tr",
      "Turkmenistan"="tm","Uganda"="ug","Ukraine"="ua",
      "United Arab Emirates"="ae","United States"="us","Uruguay"="uy",
      "Uzbekistan"="uz","Venezuela"="ve","Vietnam"="vn","Wales"="gb-wls",
      "Yemen"="ye","Zambia"="zm","Zimbabwe"="zw"
    )
    code <- codes[team]
    if (is.na(code)) return(paste0('<span style="font-size:', round(size*0.75), 'px">&#127987;</span>'))
    paste0('<img src="https://flagcdn.com/', size, 'x', round(size*0.75), '/', tolower(code),
           '.png" style="border-radius:2px;vertical-align:middle" alt="', team, '">')
  }
  
  
  
  # Returns just the ISO code for use in bracket img tags
  team_flag_code <- function(team) {
    codes <- c(
      "Afghanistan"="af","Albania"="al","Algeria"="dz","Angola"="ao",
      "Antigua and Barbuda"="ag","Argentina"="ar","Armenia"="am","Aruba"="aw",
      "Australia"="au","Austria"="at","Azerbaijan"="az","Bahamas"="bs",
      "Bahrain"="bh","Bangladesh"="bd","Barbados"="bb","Belarus"="by",
      "Belgium"="be","Belize"="bz","Benin"="bj","Bhutan"="bt","Bolivia"="bo",
      "Bosnia and Herzegovina"="ba","Botswana"="bw","Brazil"="br",
      "Brunei Darussalam"="bn","Bulgaria"="bg","Burkina Faso"="bf",
      "Burundi"="bi","Cambodia"="kh","Cameroon"="cm","Canada"="ca",
      "Cape Verde"="cv","Central African Republic"="cf","Chad"="td",
      "Chile"="cl","China"="cn","Colombia"="co","Comoros"="km","Congo"="cg",
      "Costa Rica"="cr","Croatia"="hr","Cuba"="cu","Curaçao"="cw",
      "Cyprus"="cy","Czech Republic"="cz","Denmark"="dk","Djibouti"="dj",
      "Dominican Republic"="do","DR Congo"="cd","Ecuador"="ec","Egypt"="eg",
      "El Salvador"="sv","England"="gb-eng","Equatorial Guinea"="gq",
      "Eritrea"="er","Estonia"="ee","Eswatini"="sz","Ethiopia"="et",
      "Faroe Islands"="fo","Fiji"="fj","Finland"="fi","France"="fr",
      "Gabon"="ga","Gambia"="gm","Georgia"="ge","Germany"="de","Ghana"="gh",
      "Greece"="gr","Guatemala"="gt","Guinea"="gn","Guinea-Bissau"="gw",
      "Guyana"="gy","Haiti"="ht","Honduras"="hn","Hong Kong China"="hk",
      "Hungary"="hu","Iceland"="is","India"="in","Indonesia"="id","Iran"="ir",
      "Iraq"="iq","Republic of Ireland"="ie","Israel"="il","Italy"="it",
      "Ivory Coast"="ci","Jamaica"="jm","Japan"="jp","Jordan"="jo",
      "Kazakhstan"="kz","Kenya"="ke","Kosovo"="xk","Kuwait"="kw",
      "Kyrgyzstan"="kg","Laos"="la","Latvia"="lv","Lebanon"="lb",
      "Lesotho"="ls","Liberia"="lr","Libya"="ly","Liechtenstein"="li",
      "Lithuania"="lt","Luxembourg"="lu","Macau"="mo","Madagascar"="mg",
      "Malawi"="mw","Malaysia"="my","Maldives"="mv","Mali"="ml","Malta"="mt",
      "Mauritania"="mr","Mauritius"="mu","Mexico"="mx","Moldova"="md",
      "Mongolia"="mn","Montenegro"="me","Morocco"="ma","Mozambique"="mz",
      "Myanmar"="mm","Namibia"="na","Nepal"="np","Netherlands"="nl",
      "New Zealand"="nz","Nicaragua"="ni","Niger"="ne","Nigeria"="ng",
      "North Korea"="kp","North Macedonia"="mk","Northern Ireland"="gb-nir",
      "Norway"="no","Oman"="om","Pakistan"="pk","Palestine"="ps",
      "Panama"="pa","Paraguay"="py","Peru"="pe","Philippines"="ph",
      "Poland"="pl","Portugal"="pt","Qatar"="qa","Romania"="ro","Russia"="ru",
      "Rwanda"="rw","Saudi Arabia"="sa","Scotland"="gb-sct","Senegal"="sn",
      "Serbia"="rs","Sierra Leone"="sl","Singapore"="sg","Slovakia"="sk",
      "Slovenia"="si","Somalia"="so","South Africa"="za","South Korea"="kr",
      "South Sudan"="ss","Spain"="es","Sri Lanka"="lk","Sudan"="sd",
      "Suriname"="sr","Sweden"="se","Switzerland"="ch","Syria"="sy",
      "Tajikistan"="tj","Tanzania"="tz","Thailand"="th","Togo"="tg",
      "Trinidad and Tobago"="tt","Tunisia"="tn","Turkey"="tr",
      "Turkmenistan"="tm","Uganda"="ug","Ukraine"="ua",
      "United Arab Emirates"="ae","United States"="us","Uruguay"="uy",
      "Uzbekistan"="uz","Venezuela"="ve","Vietnam"="vn","Wales"="gb-wls",
      "Yemen"="ye","Zambia"="zm","Zimbabwe"="zw"
    )
    code <- codes[team]
    if (is.na(code)) return("") else tolower(code)
  }
  
  
  # Match predictor outputs
  output$prob_display <- renderUI({
    r  <- result()
    hw <- paste0(round(r$probs$home_win * 100, 1), "%")
    dr <- paste0(round(r$probs$draw     * 100, 1), "%")
    aw <- paste0(round(r$probs$away_win * 100, 1), "%")
    HTML(paste0('
      <div class="match-header">
        <div class="team-block">
          <div>', team_flag(r$home), '</div>
          <div class="team-name">', r$home, '</div>
          <div class="team-prob">', hw, '</div>
        </div>
        <div class="vs-block" style="justify-content:flex-end;padding-bottom:2px">
          <div class="vs-label">Draw</div>
          <div class="draw-prob">', dr, '</div>
        </div>
        <div class="team-block">
          <div>', team_flag(r$away), '</div>
          <div class="team-name">', r$away, '</div>
          <div class="team-prob">', aw, '</div>
        </div>
      </div>'))
  })
  
  output$goals_display <- renderUI({
    r  <- result()
    HTML(paste0('
      <div class="scoreboard">
        <div class="sb-team sb-left">
          <div>', team_flag(r$home), '</div>
          <div class="team-name">', r$home, '</div>
        </div>
        <div class="sb-score">
          <span class="sb-num">', round(r$goals$lambda, 3), '</span>
          <span class="sb-dash">&ndash;</span>
          <span class="sb-num">', round(r$goals$mu, 3), '</span>
        </div>
        <div class="sb-team sb-right">
          <div>', team_flag(r$away), '</div>
          <div class="team-name">', r$away, '</div>
        </div>
      </div>'))
  })
  
  output$matrix_display <- renderUI({
    r   <- result()
    mat <- r$matrix
    hf  <- team_flag(r$home, 20)
    af  <- team_flag(r$away, 20)
    n   <- 0:8
    hdr1 <- paste0('<tr><th class="dim-header" rowspan="2" style="min-width:110px">',
                   hf, ' ', r$home, '<br><span style="font-weight:300;font-size:11px">&#8595; goals</span></th>',
                   '<th class="dim-header" colspan="9">', af, ' ', r$away, ' &#8594; goals</th></tr>')
    hdr2 <- paste0('<tr>', paste0('<th>', n, '</th>', collapse=''), '</tr>')
    rows <- paste0(sapply(seq_len(nrow(mat)), function(i) {
      paste0('<tr><th>', n[i], '</th>',
             paste0('<td>', mat[i,], '</td>', collapse=''), '</tr>')
    }), collapse='')
    HTML(paste0('<div class="matrix-wrap"><table><thead>', hdr1, hdr2,
                '</thead><tbody>', rows, '</tbody></table></div>'))
  })
  

  }

shinyApp(ui, server)