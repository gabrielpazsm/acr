# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

assess_normality <- function(data,
                             group,
                             var) {

  if(group == 0){
    data <- na.omit(data)
    xvar <- na.omit(as.numeric(unlist(data[, var])))
    skewness_kurtosis <- ifelse(e1071::skewness(xvar) >= -2 &
                                  e1071::skewness(xvar) <= 2 &
                                  e1071::kurtosis(xvar) >= -2 &
                                  e1071::kurtosis(xvar) <= 2,
                                "Skewness e Kurtosis - OK",
                                "Skewness e Kurtosis - NÃO OK"
    )

    dif_mean_median <- mean(xvar) - median(xvar)

    skim_df <- skim(xvar) %>%
      as.data.frame()

    quartile_dist <- skim_df %>%
      transmute(
        Distancia1 = round(numeric.p25 - numeric.p0, 3),
        Comma1 = "\n",
        Distancia2 = round(numeric.p50 - numeric.p25, 3),
        Comma2 = "\n",
        Distancia3 = round(numeric.p75 - numeric.p50, 3),
        Comma3 = "\n",
        Distancia4 = round(numeric.p100 - numeric.p75, 3)
      )

    shapiro_p <- ifelse(
      shapiro.test(xvar)$p.value >= 0.05,
      "Podemos assumir normalidade",
      "Não-normal"
    )

    cat("1 -", skewness_kurtosis, "\n")
    cat("Skewness =", round(e1071::skewness(xvar), 3), "\n")
    cat("Kurtosis =", round(e1071::kurtosis(xvar), 3), "\n")
    cat("2 - Diferença entre média e mediana =", dif_mean_median, "\n")
    ## Mínimo e máximo para noção do tamanho de grandeza da variável.
    cat("2.2 - Valor mínimo =", min(xvar), "\n")
    cat("2.1 - Valor máximo =", max(xvar), "\n")
    cat("3 - Distância entre os quartis = \n", as.character(unlist(quartile_dist, 2)), "\n")
    cat("4 - Shapiro-Wilk’s method = \n", shapiro_p)
    cat("\n\n")

    print(ggplot(
      data
    ) +
      geom_histogram(aes(x = xvar, y = ..density..), colour = "black", fill = "white", size = 1) +
      stat_function(fun = dnorm,
                    args = list(
                      mean = mean(xvar, na.rm = T),
                      sd = sd(xvar, na.rm = T)
                    ),
                    size = 1
      ) +
      geom_vline(aes(xintercept=mean(xvar, na.rm=T)),   # Ignore NA values for mean
                 color="red", linetype="dashed", size=1) +
      xlab(var))
    qqnorm(xvar)
  }

  if(group != 0){
    # Calculando o número de níveis da variável "group"
    n <- nlevels(data[[group]])

    # Criando uma lista de gráficos
    plot_hist_list <- list()

    # Pegando o nome dos fatores
    names_group <- list(levels(as.factor(data[[group]])))

    # Dividindo o banco de dados pela variável "group"
    group <- rlang::sym(group) ## Tirando as aspas da variável "group"
    data <- data %>%
      filter(!is.na({{ group }})) %>%
      group_split({{ group }})

    # For loop em que passamos por todos os grupos da variável "group"
    for (i in 1:n) {
      data[[i]] <- na.omit(data[[i]])
      xvar <- na.omit(as.numeric(unlist(data[[i]][, var])))
      skewness_kurtosis <- ifelse(e1071::skewness(xvar) >= -2 &
                                    e1071::skewness(xvar) <= 2 &
                                    e1071::kurtosis(xvar) >= -2 &
                                    e1071::kurtosis(xvar) <= 2,
                                  "Skewness e Kurtosis - OK",
                                  "Skewness e Kurtosis - NÃO OK"
      )

      dif_mean_median <- mean(xvar) - median(xvar)

      skim_df <- skim(xvar) %>%
        as.data.frame()

      quartile_dist <- skim_df %>%
        transmute(
          Distancia1 = round(numeric.p25 - numeric.p0, 3),
          Comma1 = "\n",
          Distancia2 = round(numeric.p50 - numeric.p25, 3),
          Comma2 = "\n",
          Distancia3 = round(numeric.p75 - numeric.p50, 3),
          Comma3 = "\n",
          Distancia4 = round(numeric.p100 - numeric.p75, 3)
        )

      shapiro_p <- ifelse(
        shapiro.test(xvar)$p.value >= 0.05,
        "Podemos assumir normalidade",
        "Não-normal"
      )

      cat("1 -", skewness_kurtosis, "\n")
      cat("Skewness =", round(e1071::skewness(xvar), 3), "\n")
      cat("Kurtosis =", round(e1071::kurtosis(xvar), 3), "\n")
      cat("2 - Diferença entre média e mediana =", dif_mean_median, "\n")
      ## Mínimo e máximo para noção do tamanho de grandeza da variável.
      cat("2.2 - Valor mínimo =", min(xvar), "\n")
      cat("2.1 - Valor máximo =", max(xvar), "\n")
      cat("3 - Distância entre os quartis = \n", as.character(unlist(quartile_dist, 2)), "\n")
      cat("4 - Shapiro-Wilk’s method = \n", shapiro_p)
      cat("\n\n")

      plot_hist <- ggplot(
        data[[i]]
      ) +
        geom_histogram(aes(x = xvar, y = ..density..), colour = "black", fill = "white", size = 1) +
        stat_function(fun = dnorm,
                      args = list(
                        mean = mean(xvar, na.rm = T),
                        sd = sd(xvar, na.rm = T)
                      ),
                      size = 1
        ) +
        geom_vline(aes(xintercept=mean(xvar, na.rm=T)),   # Ignore NA values for mean
                   color="red", linetype="dashed", size=1) +
        xlab(var) +
        ggtitle(map(names_group, i))
      plot_hist_list[[i]] <- as.grob(plot_hist)
      qqnorm(xvar)
    }
    do.call(grid.arrange, plot_hist_list)
  }
}
