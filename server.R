#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


server <- function(input, output) {
  
  output$wormPlot <- renderPlot({
    n <- input$n
    n_traj <- input$n_traj
    B <- array(data = NA, dim = c(n, n_traj))
    K <- input$K
    B_MSY <- K / 2
    B_risk <- 0.20 * K
    h <- array(data = input$h, dim = c(n, n_traj))
    r.traj <- rnorm(n = n_traj, mean = input$r, sd = input$sd_r)
    K.traj <- rnorm(n = n_traj, mean = K, sd = input$sd_K)
    sd.noise <- input$sd_noise
    alpha <- 0.25
    
    for (traj in 1:n_traj) {
      B[1, traj] <- alpha * K * exp(rnorm(1, mean = 0, sd = sd.noise))
      for (t in 1:(n - 1)) {
        Bm_new <- max((B[t, traj] + r.traj[traj] * B[t, traj] * (1 - B[t, traj] / K.traj[traj]) - h[t, traj] * B[t, traj]), 0.001 * K)
        log_Bnew <- rnorm(1, mean = log(Bm_new), sd = sd.noise)
        B[t + 1, traj] <- exp(log_Bnew)
      }
    }
    
    plot(1:n, B[, 1], type = "l", ylim = c(0, 1.2 * K), main = "Evolution de la biomasse")
    for (traj in 2:n_traj) {
      points(1:n, B[, traj], type = "l")
    }
    points(1:n, rep(K, times = n), col = "blue", type = "l", lwd = 2)
    text(n / 2, K, "capacite de charge du milieu (K)", pos = 3, col = "blue")
    points(1:n, rep(B_MSY, times = n), col = "green", type = "l", lwd = 2)
    text(n / 2, B_MSY, "niveau de biomasse au MSY", pos = 1, col = "green")
    points(1:n, rep(B_risk, times = n), col = "red", type = "l", lwd = 3)
    text(n / 2, B_risk, "zone a risque : biomasse < 20% de K", pos = 1, col = "red")
  })
  
  output$boxPlot <- renderPlot({
    B <- array(data = NA, dim = c(input$n, input$n_traj))
    K <- input$K
    B_MSY <- K / 2
    B_risk <- 0.20 * K
    h <- array(data = input$h, dim = c(input$n, input$n_traj))
    r.traj <- rnorm(n = input$n_traj, mean = input$r, sd = input$sd_r)
    K.traj <- rnorm(n = input$n_traj, mean = K, sd = input$sd_K)
    sd.noise <- input$sd_noise
    alpha <- 0.25
    
    for (traj in 1:input$n_traj) {
      B[1, traj] <- alpha * K * exp(rnorm(1, mean = 0, sd = sd.noise))
      for (t in 1:(input$n - 1)) {
        Bm_new <- max((B[t, traj] + r.traj[traj] * B[t, traj] * (1 - B[t, traj] / K.traj[traj]) - h[t, traj] * B[t, traj]), 0.001 * K)
        log_Bnew <- rnorm(1, mean = log(Bm_new), sd = sd.noise)
        B[t + 1, traj] <- exp(log_Bnew)
      }
    }
    
    boxplot(as.data.frame(t(B)), main = "Distribution des probabilites marginales de la biomasse", ylim = c(0, 1.2 * K), outline = FALSE)
    points(1:input$n, rep(K, times = input$n), col = "blue", type = "l", lwd = 2)
    text(input$n / 2, K, "Capacite de charge du milieu (K)", pos = 3, col = "blue")
    points(1:input$n, rep(B_MSY, times = input$n), col = "green", type = "l", lwd = 2)
    text(input$n / 2, B_MSY, "niveau de biomasse au MSY", pos = 1, col = "green")
    points(1:input$n, rep(B_risk, times = input$n), col = "red", type = "l", lwd = 3)
    text(input$n / 2, B_risk, "zone a risque : biomasse < 20% de K", pos = 1, col = "red")
  })
  
  output$riskPlot <- renderPlot({
    B <- array(data = NA, dim = c(input$n, input$n_traj))
    K <- input$K
    B_MSY <- K / 2
    B_risk <- 0.20 * K
    h <- array(data = input$h, dim = c(input$n, input$n_traj))
    r.traj <- rnorm(n = input$n_traj, mean = input$r, sd = input$sd_r)
    K.traj <- rnorm(n = input$n_traj, mean = K, sd = input$sd_K)
    sd.noise <- input$sd_noise
    alpha <- 0.25
    
    for (traj in 1:input$n_traj) {
      B[1, traj] <- alpha * K * exp(rnorm(1, mean = 0, sd = sd.noise))
      for (t in 1:(input$n - 1)) {
        Bm_new <- max((B[t, traj] + r.traj[traj] * B[t, traj] * (1 - B[t, traj] / K.traj[traj]) - h[t, traj] * B[t, traj]), 0.001 * K)
        log_Bnew <- rnorm(1, mean = log(Bm_new), sd = sd.noise)
        B[t + 1, traj] <- exp(log_Bnew)
      }
    }
    
    seuil <- B_risk
    risk <- apply(as.data.frame(B) < seuil, MARGIN = 1, FUN = mean)
    plot(1:input$n, risk, ylim = c(0, 1), main = "Risque : B < 20% de K", type = "l", col = "red")
  })
  
  simulate_fishery <- function(fish_type, h_value) {
    n <- 100
    n_traj <- 1
    B <- array(data = NA, dim = c(n, n_traj))
    sd.noise <- input$sd_noise
    alpha <- 0.25
    
    # Parametres specifiques aux poissons (a ajuster selon vos besoins)
    fish_parameters <- switch(fish_type,
                              "Sardines" = list(r_mean = 0.85, K_mean = 400000, sd_r = 0.65, sd_K = 1000),
                              "Maquereau" = list(r_mean = 0.35, K_mean = 350000, sd_r = 0.15, sd_K = 1000))
    
    for (traj in 1:n_traj) {
      
      B[1, traj] <- alpha * fish_parameters$K_mean * exp(rnorm(1, mean = 0, sd = sd.noise))
      
      for (t in 1:(n - 1)) {
        Bm_new <- max((B[t, traj] + fish_parameters$sd_r * B[t, traj] * (1 - B[t, traj] / fish_parameters$K_mean) - h_value * B[t, traj]), 0.001 * fish_parameters$K_mean)
        log_Bnew <- rnorm(1, mean = log(Bm_new), sd = sd.noise)
        B[t + 1, traj] <- exp(log_Bnew)
      }
    }
    
    seuil <- 0.20 * fish_parameters$K_mean
    risk <- any(B < seuil)
    
    return(list(risk = risk, fish_parameters = fish_parameters, seuil = seuil))
  }
  
  
  observeEvent(input$runSimulation, {
    # Recuperer les valeurs choisies par l'utilisateur
    fish_type <- input$fishType
    h_value <- input$hValue
    
    # Appeler la fonction de simulation
    risk_result <- simulate_fishery(fish_type, h_value)
    
    # Afficher le resultat de la simulation
    output$simulationResult <- renderText({
      risk <- risk_result$risk
      seuil <- risk_result$seuil
      
      if (any(risk)) {
        return("La capture de peche entraine un risque pour le maintien de la biomasse.")
      } else {
        return("La capture de peche permet de maintenir le stock de poisson.")
      }
    })
  })
  
  
  
}

