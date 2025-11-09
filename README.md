
app.R file changed by Github to App.R by default : Need to know it for Docker
Step 1: Edit renv/settings.json
Open the file renv/settings.json in your text editor and change it to:
json{
  "use.cache": false,
  "snapshot.type": "implicit"

# 1. Edit renv/settings.json (already done)

# 2. Clean up lockfile
R -e "renv::snapshot(type = 'implicit')" in Terminal 


Ab January 2026
add prediction part,
i) Steiner Triple System
https://de.wikipedia.org/wiki/Fano-Ebene
https://marwahaha.github.io/steinersystems/
https://www.dmgordon.org/cover/
ii) from book archive ,, 
https://archive.org/details/combinatoriallot0000iliy
iii) set prepared data previously, 25-sets, 15-sets 10-sets
personally developed
iv) add more models