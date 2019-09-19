# BUSINESS SCIENCE - LEARNING LAB 16 ----
# CASE 3 - How To Optimize A Portfolio with Nonlinear Programming ----

# https://www.linkedin.com/pulse/power-r-trading-part-1-ralph-sueppel/

# Nonlinear programming allows complext procesess that have branches of if-then, power transformations
# - claculates a global optimum rather than a local optimum like a linear solution might provide

# Modern Portfolio Theory by Harry Markowitz: What is the lowest portolio variance that I can achieve a given return objective?
# - Uses a matrix of covariances.  Identified the Efficient Frontier where a plot (Risk v Return) makes a sharp turn as you take on more and more risk

# Goal - min portfolio variance (covariance)
# Objective:  max return - 40%

# Will change the mix of stocks to achive the min risk high reward objective
# See related Excel Sheet

# ROI is a bit more complex.  In the Product Mix exmaple, OMPR wrapped the ROI complexity for us.

# Nonlinear programming has 4 concepts:
# 1. F_objective: Functional Objective.  Allows you to add a function to objectives.  (Cannot do this with OMPR).  Minimize portfolio calculation
# 2. F_constraint:  Applies functions to constraints.  Inputs assest weights, outputs port.  Return objective here is set to 40%/year
# 3. L-constraint: Linear constraint.  Adding linear contrainsts.  Bounding the weights (of the stocks in the portfolio mix)
# 4. Can combine a linear matrix and non-linear programming (functions)

# 1.0 LIBRARIES ----

# Solver Backend
library(ROI)
library(ROI.plugin.alabama) # non-linear solver

# Finance
library(tidyquant)

# Viz
library(plotly)

# Core
library(tidyverse)

# Timing
library(tictoc)

setwd("~/GitHub/Optimization/Portfolio")

# 2.0 DATA ----
assets <- c("FB", "AMZN", "AAPL", "GOOG", "NFLX") %>% sort()

stock_prices_tbl <- tq_get(assets, from = "2013-01-01", to = "2019-09-01")

stock_returns_tbl <- stock_prices_tbl %>% select(symbol, date, adjusted) %>% group_by(symbol) %>%
    tq_transmute(adjusted, mutate_fun = periodReturn, period = "yearly", col_rename = "returns")

returns_matrix_tbl <- stock_returns_tbl %>% spread(symbol, returns) %>% select(assets) 

returns_matrix_tbl

# 3.0 OPTIMIZATION ----

# 3.1 STATIC VARIABLES ----

cov_mtx <- cov(returns_matrix_tbl) # create covariance matrix - a stats function

stats_tbl <- stock_returns_tbl %>% summarise(mean  = mean(returns), stdev = sd(returns)) # See the Excel file - these are simply inputs

# 3.2 FUNCTIONS ----

# These are reuseable!  They are used in the F_ functions

calc_portfolio_variance <- function(weights) {
    t(weights) %*% (cov_mtx %*% weights) %>% as.vector()
}

calc_portfolio_variance(c(1, 0, 0, 0, 0))


calc_portfolio_return <- function(weights) {
    stats <- stats_tbl$mean
    sum(stats * weights)
}

calc_portfolio_return(c(1, 0, 0, 0, 0))

# 3.3 OBJECTIVE ----

n_assets <- length(assets)

# OP is an Optimization Problem Constructor from ROI
# F_objective arguments: F (an R function), n (num objective variables), G = NULL, H = NULL, names = NULL
# - G and H simply improve the performance of the calculations
# F-constraint arguments: F (R function), rhs (the value of F to be optimized)
# L_constraint arguments (linear or box constraints): 
# - diag from R base extracts or replaces the diagonal of a matrix, or construct a diagonal matrix.
# - 
#   - diag(n_assets) - try it
model_nlp <- OP(
    objective   = F_objective(F = calc_portfolio_variance, n = n_assets, names = assets),
    constraints = rbind(F_constraint(F = calc_portfolio_return, dir = ">=", rhs = 0.40), # return requirement - right hand side of the equation
                        L_constraint(diag(n_assets), rep(">=", n_assets), rep(0, n_assets)), # the weights given to the stocks must be >= than 0
                        # above creates a 5 x 5 matrix with all 0s except a diagonal of 1s (identity matrix)
                        # simply means the stocks cannot be less than 0 weight
                        
                        # Below is the same as the identy matrix above but <= is changed to all values are <= 1
                        L_constraint(diag(n_assets), rep("<=", n_assets), rep(1, n_assets)), # # the weights given to the stocks must be <= than 1
                        L_constraint(rep(1, n_assets), "==", 1) ), # this simply requires all the weights to equal 1 (all the money is invested)
    maximum = FALSE) # requires a minumum return of 0.4

tic()
sol <- ROI_solve(model_nlp, solver = "alabama", start = rep(1/n_assets, n_assets)) # start simply provides a starting point for each of the stock weights
toc()

sol$objval # portfolio variance (0.12)

solution(sol) %>% round(2) # optimal weights of each of the stocks.


# 4.0 SIMULATION - Iterative Optimization ----

# 4.1 Create Function ----
optimize_portfolio <- function(required_return = 0.4) {
    
    model_nlp <- OP(objective   = F_objective(F = calc_portfolio_variance, n = n_assets, names = assets),
                    constraints = rbind(
                        F_constraint(F = calc_portfolio_return, dir = ">=", rhs = required_return),
                        L_constraint(diag(n_assets), rep(">=", n_assets), rep(0, n_assets)),
                        L_constraint(diag(n_assets), rep("<=", n_assets), rep(1, n_assets)),
                        L_constraint(rep(1, n_assets), "==", 1) ),
        maximum = FALSE)
    
    sol <- ROI_solve(model_nlp, solver = "alabama", start = rep(1/n_assets, n_assets))
    
    return(bind_cols(tibble(return_constraint = required_return),
                     tibble(portfolio_return  = calc_portfolio_return(sol$solution)),
                     tibble(portfolio_stdev   = (sol$objval)^0.5),
                     enframe(sol$solution) %>% spread(key = name, value = value)) )
}

optimize_portfolio(0.4) 

# 4.2 Map (Simulation) ----

tic()
portfolio_sim_results_tbl <- seq(0.10, 0.50, length.out = 20) %>% map_dfr(optimize_portfolio)
toc()

portfolio_sim_results_tbl 

# 4.3 Visualize ----

# 4.3.1 Heat Map ----
plot_heatmap <- function(data) {
    
    data_transformed_tbl <- data %>%
        mutate(sharpe_ratio = portfolio_return / portfolio_stdev) %>%
        mutate(portfolio_id = row_number()) %>%
        gather(key = stock, value = weight,
               -sharpe_ratio, -portfolio_return, -portfolio_stdev, 
               -portfolio_id, -return_constraint,
               factor_key = TRUE) %>%
        mutate(return_objective = scales::percent(return_constraint)) %>%
        mutate(label_text = str_glue("Return Objective: {scales::percent(return_constraint)}
                                     Portfolio Return: {scales::percent(portfolio_return)}
                                     Portfolio Sharpe: {round(sharpe_ratio, 2)}
                                     Portfolio StdDev: {round(portfolio_stdev, 2)}"))
    
    g <- data_transformed_tbl %>%
        ggplot(aes(stock, y = return_objective, fill = weight)) +
        geom_tile() +
        geom_point(aes(text = label_text), size = 0.1, alpha = 0) +
        scale_fill_gradient(low = "#FFFFFF", high = "#2c3e50") +
        geom_text(aes(label = scales::percent(weight)), size = 3) +
        theme_tq() +
        labs(title = "Optimized Portfolio Weights", x = "Stock", y = "Return Objective")
    
    ggplotly(g, tooltip = "text")
    
}

portfolio_sim_results_tbl %>% plot_heatmap()
#SOmewhere around 25% return gives the highest Sharpe ratio - probably where you want to be

# 4.3.2 Efficient Fronteir ----
plot_efficient_frontier <- function(data) {
    
    portfolio_metrics_tbl <- data %>%
        select(return_constraint, portfolio_return, portfolio_stdev)
    
    stock_weights_tbl <- data %>%
        select(-c(return_constraint, portfolio_return, portfolio_stdev))
    
    stock_text <- names(stock_weights_tbl) %>%
        map(~ str_c(.x, stock_weights_tbl %>% pull(.x) %>% scales::percent(),
                    sep = ": ")) %>%
        set_names(names(stock_weights_tbl)) %>%
        as_tibble() %>%
        mutate(stock_text = str_c(!!! syms(names(stock_weights_tbl)), sep = "\n")) %>%
        pull(stock_text)
        
    
    g <- portfolio_metrics_tbl %>%
        mutate(sharpe_ratio = portfolio_return / portfolio_stdev) %>%
        mutate(label_text = str_glue("Return Objective: {scales::percent(return_constraint)}
                                     Portfolio Return: {scales::percent(portfolio_return)}
                                     Portfolio Sharpe: {round(sharpe_ratio, 2)}
                                     Portfolio StdDev: {round(portfolio_stdev, 2)}
                                     ---")) %>%
        mutate(label_text = str_c(label_text, stock_text, sep = "\n")) %>% 
        
        ggplot(aes(x = portfolio_stdev, y = portfolio_return, 
                   color = sharpe_ratio, size = sharpe_ratio)) +
        geom_point(aes(text = label_text)) +
        expand_limits(x = 0, y = 0) +
        labs(title = "Efficient Frontier", 
             x = "Portfolio Risk (Standard Deviation)", 
             y = "Portfolio Return (Mean)") +
        theme_tq()
    
    ggplotly(g, tooltip = "text")
    
}

portfolio_sim_results_tbl %>% plot_efficient_frontier()
# as portfolio changes, there is a bend where the risk starts to accellerate