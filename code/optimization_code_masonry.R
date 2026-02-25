# either:
# 1. open the file in RStudio desktop or
# 2. navigate here & login: https://login.posit.cloud/login > "Posit Cloud" > New Project > upload the file or copy & paste file contents

# 1. INSTALLATION (Students only need to run these once)
# install.packages(c("dplyr", "ompr", "ompr.roi", "ROI.plugin.glpk"))

# 2. LOAD LIBRARIES
library(dplyr)           # For data manipulation
library(ompr)            # The modeling language
library(ompr.roi)        # Connects ompr to the solver
library(ROI.plugin.glpk) # The actual engine that does the math

# 3. DEFINE INPUT DATA
# Example: Homewood Masonry
# plant_id | cost_per_unit | removal_capacity | removal_efficiency
products <- data.frame(
  id = 1:2,
  cost = c(140, 160),        # Cost ($) per ton hydit/fillit
  max_cap = c(8, 6),     # Max storage in each week
  clay_per_ton = c( 2, 4),
  work_hours = c( 5, 5)
)
products

# define maxima for the constraings
total_clay <- 28
total_time <- 50

# 4. BUILD THE MODEL
model <- MIPModel() %>%

  # Define Variables: How much should each plant remove?
  # x[i] refers to removal at plant i (1 and 2)
  add_variable(x[i], i = 1:2, type = "continuous", lb = 0) %>%

  # Objective: Minimize total treatment cost
  # Syntax: sum_over(expression, index = range)
  set_objective(sum_over(products$cost[i] * x[i], i = 1:2), "max") %>%

  # Constraint 1: Total clay available
  add_constraint(sum_over(products$clay_per_ton[i] * x[i], i = 1:2) <= total_clay) %>%

  # Constraint 2: Work hours
  add_constraint(sum_over(products$work_hours[i] * x[i], i = 1:2) <= total_time) %>%

  # Constraint 3: Stay within each plant's physical capacity
  # This automatically creates one constraint for every plant 'i'
  add_constraint(x[i] <= products$max_cap[i], i = 1:2)

# 5. SOLVE THE MODEL
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

# 6. VIEW RESULTS
print(paste("Optimization Status:", solver_status(result)))

# Extract and view the values of 'x'
solution <- get_solution(result, x[i])
print(solution)

# Extract the minimized total cost
print(paste("Maximum Total Revenue: $", objective_value(result)))

## ===================================================
# plot the feasible region
plot(1, type="n", xlim=c(-1, 10), ylim=c(-1, 10),
     main = 'Homewood Masonry',
     xlab = 'HYDIT, x1 [tons]',
     ylab = 'FILIT, x2 [tons]')

# add the constraint lines
abline( h = 6)
abline( h = 0)
abline( v = 8)
abline( v = 0)
abline( 10, -1)
abline( 7, -.5)


# create the polygon—solved for nodes using equations above
x1 <- c( 0, 8,  8, 6, 2, 0)
x2 <- c( 0, 0,  2, 4, 6, 6)
polygon( x1, x2, col = 'yellow')

# plot for series of Z values
# Z = 140x1 + 160x2
# y = -140x/160 + Z/160
for( Z in seq( 0, 1480, 185))
  abline( Z / 160, -140 / 160, lty = 'dashed', col = 'green')

# circle the optimal point
points( solution[1,'value'], solution[2,'value'], cex = 2, col = 'red')
