# Create an account on shiniapps.io to deploy your first app. This
# procedure together will take approximately 10 minutes.

# - Go to: https://www.shinyapps.io/admin/#/signup
# 
# - Create an account (name + password). You can use whatever email
#   you like.
# 
# - Fill in your tokens (accountname + token + secret) in the code
#   below
# 
# - Run the code until line 15 (the rest is for the workshop today)
# 
# - In Line 21, set appDir= an empty folder that you will use for
#   today’s class. Use complete path (example: appDir =
#   "C:/Users/david/Google
#   Drive/R_learning/shiny/shiny.rodrigo/covid-19” )



#install.packages("shiny")
#install.packages("rsconnect") # used to deploy
library(shiny)
library(data.table)
library(rsconnect) # open libraries that you will use today

setAccountInfo(name='rbelo',
			  token='',
			  secret='')
deployApp('shiny/covid-19', account='rbelo')
