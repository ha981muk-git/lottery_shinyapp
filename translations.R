# ============================================================================
# STEP 1: Create translations.R file (save this in your project root)
# ============================================================================

translations <- list(
  en = list(
    # Header
    title = "6/49 Statistical Visualization",
    subtitle = "Statistical Analysis & Educational Platform",
    testing_badge = "TESTING",
    nav_home = "Home",
    nav_analyzer = "Analyzer",
    nav_educational = "Educational Info",
    nav_disclaimer = "Disclaimer",
    
    # Analysis Settings
    analysis_settings = "Analysis Settings",
    
    # Input Module
    input_live_dashboard = "Live Dashboard",
    input_realtime = "Real-time analytics",
    input_ball_range = "Ball Range",
    input_analysis_type = "Analysis Type",
    input_time_window = "Time Window",
    input_refresh = "Refresh Data",
    
    # Time choices
    time_last_7 = "Last 7 Weeks",
    time_last_30 = "Last 30 Weeks",
    time_last_60 = "Last 60 Weeks",
    time_last_90 = "Last 90 Weeks",
    time_last_120 = "Last 120 Weeks",
    time_last_150 = "Last 150 Weeks",
    time_last_180 = "Last 180 Weeks",
    
    # Metric choices
    metric_balls = "Balls",
    metric_sums = "Sums",
    metric_odds_evens = "Odds Evens",
    metric_tables = "Tables",
    metric_difference = "Difference",
    metric_lag = "Lag",
    
    
    # Balls Metric Module
    balls_title = "6/49 Statistical Analysis Demo",
    balls_subtitle = "Educational demonstration of probability theory and data visualization techniques",
    
    # Metric Cards
    balls_metric_coverage = "Total Coverage",
    balls_metric_occurrence = "Total Occurrence",
    balls_metric_chance = "Total Chance",
    balls_metric_range = "Range Difference",
    
    # Chart Titles
    balls_trend_title = "Trend Analysis",
    balls_distribution_title = "Performance Distribution",
    balls_overview_title = "Comprehensive Overview",
    balls_boxplot_title = "Box Plot of Balls 1 to 6",
    balls_violin_title = "Violin Plot of Balls 1 to 6",
    balls_raincloud_title = "Raincloud Plot of Balls 1 to 6",
    balls_chart_density = "Density Distribution",
    balls_chart_density_title = "Density of Numbers for Each Ball",
    
    # Axis Labels
    ball_label = "Ball",
    value_label = "Value",
    
    # Individual Ball Labels
    ball_1 = "Ball 1",
    ball_2 = "Ball 2",
    ball_3 = "Ball 3",
    ball_4 = "Ball 4",
    ball_5 = "Ball 5",
    ball_6 = "Ball 6",
    
    
    
    # Educational Notice
    notice_title = "⚠️ Important Notice - Educational Purpose Only",
    notice_1 = "This is a TESTING and EDUCATIONAL platform",
    notice_1b = " for demonstrating statistical analysis methods",
    notice_2 = "This website is ",
    notice_2b = "under construction",
    notice_2c = " and not intended for commercial use",
    notice_3 = "No real lottery services, betting, or gambling features are provided",
    notice_4 = "All data analysis is for ",
    notice_4b = "educational and research purposes",
    notice_4c = " only",
    notice_5 = "This tool demonstrates probability theory, data visualization, and statistical methods",
    notice_6 = "Warning:",
    notice_6b = " Gambling can be addictive. Please play responsibly. This site does NOT encourage gambling",
    notice_purpose = "🔬 Purpose: Academic demonstration of statistical computing",
    
    # Educational Section
    edu_title = "About This Educational Project",
    edu_intro = "This application demonstrates advanced statistical analysis techniques using publicly available lottery data. It serves as an educational resource for understanding probability distributions, frequency analysis, and data visualization methods. The platform is designed for students, researchers, and data science enthusiasts interested in learning about statistical computing and interactive web applications.",
    edu_objectives = "Learning Objectives:",
    edu_obj_1 = "Understanding probability theory and statistical distributions",
    edu_obj_2 = "Interactive web application development",
    edu_obj_3 = "Time series analysis and pattern recognition",
    edu_obj_4 = "Responsible interpretation of statistical results",
    
    # Footer
    footer_about = "About This Project",
    footer_edu_only = "Educational & Testing Only",
    footer_desc = "Professional analysis tools for demonstrating statistical methods with public Lotto 6aus49 data. Based on historical data and modern statistical approaches.",
    footer_construction = "⚠️ Under Construction - Testing Phase",
    footer_quick = "Quick Links",
    footer_legal = "Legal & Disclaimer",
    footer_full_disclaimer = "Full Disclaimer",
    footer_privacy = "Privacy Policy",
    footer_terms = "Terms of Use",
    footer_edu_statement = "Educational Purpose Statement",
    footer_no_gambling = "⚠️ No gambling services provided",
    footer_info = "Important Information",
    footer_project_type = "Project Type: Educational/Academic",
    footer_status = "Status: Under Construction (Testing)",
    footer_copyright = "Educational & Testing Project",
    footer_for_edu = "FOR EDUCATIONAL PURPOSES ONLY",
    footer_play_resp = "Play Responsibly",
    footer_no_services = "No Real Gambling Services Provided",
    footer_under_const = "Under Construction"
  ),
  
  de = list(
    # Header
    title = "6/49 Statistische Visualisierung",
    subtitle = "Statistische Analyse & Bildungsplattform",
    testing_badge = "TEST",
    nav_home = "Startseite",
    nav_analyzer = "Analysator",
    nav_educational = "Bildungsinfo",
    nav_disclaimer = "Haftungsausschluss",
    
    # Analysis Settings
    analysis_settings = "Analyseeinstellungen",
    
    # Input Module
    input_live_dashboard = "Live Dashboard",
    input_realtime = "Echtzeit-Analytik",
    input_ball_range = "Kugelbereich",
    input_analysis_type = "Analysetyp",
    input_time_window = "Zeitfenster",
    input_refresh = "Daten aktualisieren",
    
    # Time choices
    time_last_7 = "Letzte 7 Wochen",
    time_last_30 = "Letzte 30 Wochen",
    time_last_60 = "Letzte 60 Wochen",
    time_last_90 = "Letzte 90 Wochen",
    time_last_120 = "Letzte 120 Wochen",
    time_last_150 = "Letzte 150 Wochen",
    time_last_180 = "Letzte 180 Wochen",
    
    # Metric choices
    metric_balls = "Kugeln",
    metric_sums = "Summen",
    metric_odds_evens = "Gerade/Ungerade",
    metric_tables = "Tabellen",
    metric_difference = "Differenz",
    metric_lag = "Verzögerung",
    
    # Balls Metric Module
    balls_title = "6/49 Statistische Analyse Demo",
    balls_subtitle = "Pädagogische Demonstration der Wahrscheinlichkeitstheorie und Datenvisualisierungstechniken",
    
    # Metric Cards
    balls_metric_coverage = "Gesamtabdeckung",
    balls_metric_occurrence = "Gesamthäufigkeit",
    balls_metric_chance = "Gesamtchance",
    balls_metric_range = "Bereichsdifferenz",
    
    # Chart Titles
    balls_trend_title = "Trendanalyse",
    balls_distribution_title = "Leistungsverteilung",
    balls_overview_title = "Umfassende Übersicht",
    balls_boxplot_title = "Boxplot der Kugeln 1 bis 6",
    balls_violin_title = "Violinplot der Kugeln 1 bis 6",
    balls_raincloud_title = "Raincloud-Plot der Kugeln 1 bis 6",
    balls_chart_density = "Dichteverteilung",
    balls_chart_density_title = "Dichte der Zahlen für jede Kugel",
    
    
    # Axis Labels
    ball_label = "Kugel",
    value_label = "Wert",
    
    # Individual Ball Labels
    ball_1 = "Kugel 1",
    ball_2 = "Kugel 2",
    ball_3 = "Kugel 3",
    ball_4 = "Kugel 4",
    ball_5 = "Kugel 5",
    ball_6 = "Kugel 6",
    
    # Educational Notice
    notice_title = "⚠️ Wichtiger Hinweis - Nur zu Bildungszwecken",
    notice_1 = "Dies ist eine TEST- und BILDUNGSPLATTFORM",
    notice_1b = " zur Demonstration statistischer Analysemethoden",
    notice_2 = "Diese Website befindet sich ",
    notice_2b = "im Aufbau",
    notice_2c = " und ist nicht für kommerzielle Zwecke bestimmt",
    notice_3 = "Es werden keine echten Lotterie-, Wett- oder Glücksspielfunktionen angeboten",
    notice_4 = "Alle Datenanalysen dienen ",
    notice_4b = "ausschließlich Bildungs- und Forschungszwecken",
    notice_4c = "",
    notice_5 = "Dieses Tool demonstriert Wahrscheinlichkeitstheorie, Datenvisualisierung und statistische Methoden",
    notice_6 = "Warnung:",
    notice_6b = " Glücksspiel kann süchtig machen. Bitte spielen Sie verantwortungsvoll. Diese Seite fördert KEIN Glücksspiel",
    notice_purpose = "🔬 Zweck: Akademische Demonstration statistischer Berechnungen",
    
    # Educational Section
    edu_title = "Über dieses Bildungsprojekt",
    edu_intro = "Diese Anwendung demonstriert fortgeschrittene statistische Analysetechniken unter Verwendung öffentlich verfügbarer Lottodaten. Sie dient als Bildungsressource zum Verständnis von Wahrscheinlichkeitsverteilungen, Häufigkeitsanalysen und Datenvisualisierungsmethoden. Die Plattform richtet sich an Studenten, Forscher und Data-Science-Enthusiasten, die sich für statistisches Computing und interaktive Webanwendungen interessieren.",
    edu_objectives = "Lernziele:",
    edu_obj_1 = "Verständnis der Wahrscheinlichkeitstheorie und statistischer Verteilungen",
    edu_obj_2 = "Entwicklung interaktiver Webanwendungen",
    edu_obj_3 = "Zeitreihenanalyse und Mustererkennung",
    edu_obj_4 = "Verantwortungsvolle Interpretation statistischer Ergebnisse",
    
    # Footer
    footer_about = "Über dieses Projekt",
    footer_edu_only = "Nur Bildung & Test",
    footer_desc = "Professionelle Analysetools zur Demonstration statistischer Methoden mit öffentlichen Lotto 6aus49-Daten. Basierend auf historischen Daten und modernen statistischen Ansätzen.",
    footer_construction = "⚠️ Im Aufbau - Testphase",
    footer_quick = "Schnelllinks",
    footer_legal = "Rechtliches & Haftungsausschluss",
    footer_full_disclaimer = "Vollständiger Haftungsausschluss",
    footer_privacy = "Datenschutzrichtlinie",
    footer_terms = "Nutzungsbedingungen",
    footer_edu_statement = "Bildungszweck-Erklärung",
    footer_no_gambling = "⚠️ Keine Glücksspieldienste bereitgestellt",
    footer_info = "Wichtige Informationen",
    footer_project_type = "Projekttyp: Bildung/Akademisch",
    footer_status = "Status: Im Aufbau (Test)",
    footer_copyright = "Bildungs- & Testprojekt",
    footer_for_edu = "NUR FÜR BILDUNGSZWECKE",
    footer_play_resp = "Spielen Sie verantwortungsvoll",
    footer_no_services = "Keine echten Glücksspieldienste bereitgestellt",
    footer_under_const = "Im Aufbau"
  )
)

# Helper function to get translation
t <- function(key, lang = "de") {
  translations[[lang]][[key]] %||% key
}