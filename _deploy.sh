### In Plesk, execute with:
### bash _deploy.sh >> _deployment.log 2>&1
###
### Comment/uncomment this to show the commands as they are executed
#set -x

echo '\n'

echo - - - STARTING DEPLOYMENT SCRIPT at $(date) - - -

cd ~/deploy_rock.opens.science

echo Deleting old public directory before creating new site...

rm -rf ~/deploy_rock.opens.science/public

echo Running PkgDown in deployment directory...

/usr/local/bin/R -e "pkgdown::build_site();"

echo Done with PkgDown. Removing old site contents...

rm -rf ~/rock.opens.science/*.*
rm -rf ~/rock.opens.science/*

echo Deleted old contents. Copying new contents.

cp -RT public ~/rock.opens.science

echo Done copying over new website.

echo - - - ENDING DEPLOYMENT SCRIPT at $(date) - - -

echo '\n'
