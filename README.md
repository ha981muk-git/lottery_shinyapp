
app.R file changed by Github to App.R by default : Need to know it for Docker
Step 1: Edit renv/settings.json
Open the file renv/settings.json in your text editor and change it to:
json{
  "use.cache": false,
  "snapshot.type": "implicit"

# 1. Edit renv/settings.json (already done)

# 2. Clean up lockfile
R -e "renv::snapshot(type = 'implicit')" in Terminal 
