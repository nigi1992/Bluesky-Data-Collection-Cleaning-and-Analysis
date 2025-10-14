## Install & Load Packages ##

install.packages(c("bskyr", "BTM", "ellmer", "ggraph", "reshape2",
                   "textdata", "tidygraph", "tidyverse", "tidytext",
                   "topicmodels", "usethis"))
library("bskyr")
library("BTM")
library("ellmer")
library("ggplot2")
library("ggraph")
library("reshape2")
library("textdata")
library("tidygraph")
library("tidyverse")
library("tidytext")
library("topicmodels")
library("usethis")


# Ollama Model Download ---------------------------------------------------

# To pull the model, open the Terminal tab in RStudio tab in RStudio (normally found next to the Console tab),
# and enter the following command:

# ollama pull gemma3:4b

# Please wait until the download completes. This is about 3.3 GB, so it may take a while. 
# You will know it has finished when you see a messages related to successful complete of the pull and the Terminal prompt reappears.


# Bsky API Key ------------------------------------------------------------

## Setting up API access involves three steps:

# 1. Open an account on the website

# nigi.wasi@hotmail.com
# password: A5S4xSv!HUzj&2
# App Password: 6xk5-7cgp-kny5-f4ms
# api-muncher.bsky.social
# @api-muncher.bsky.social

# 2. Generate a password — this is used instead of your normal password to authenticate secure access to the API.

# 3. Save your credentials to your .Renviron file 
# — this ensures they load automatically in each R session, while reducing the risk of accidentally sharing them.


## Open .Renviron file

# The .Renviron file is where you will securely store your API credentials. Open your
# .Renviron file by typing this command into the Console in RStudio:

# usethis::edit_r_environ()

# Copy the lines below into the file (we will update the placeholders in the next steps):

# BSkyUser='your_username_here'
# BSkyPassword='your_password_here'

# If your .Renviron file already contains other environment variables, simply add the new
# lines below to the existing content rather than replacing it. Each variable should be on its own line.

# Keep the file open — you will fill in the values after creating your account and generating the
# API password.


## Bsky account and password

# Once you have your handle, replace the value for BLUESKY_APP_USER with your Bluesky handle in your .Renviron file, 
# without the .Renviron @ symbol. For example:

# BLUESKY_APP_USER='resulumit.bsky.social'

# Next, you should create an app password, at bsky.app/settings/app-passwords: Click
# '+ Add App Password' , give your app a name, and copy the password shown.

# In your .Renviron file, replace the value for BLUESKY_APP_PASS with the password you just copied.
# You will not be able to see this password again, so save it now.

# Once you have added both values to your .Renviron file, save and close it. 
# You then need to restart your R session for the new password and token to take effect. 
# Follow: Session → Restart R 
# in RStudio.