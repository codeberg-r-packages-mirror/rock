### In Plesk, execute with:
### bash _deploy.sh >> deployment.log 2>&1
###
### Comment/uncomment this to show the commands as they are executed
#set -x

echo - - - STARTING DEPLOYMENT SCRIPT at $(date) - - -

### Go to directory with cloned git repo
cd ~/deploy_rock.opens.science

### Delete old 'public' directory if it exists
#rm -rf public

pwd
echo $PATH

echo Deleting old public directory before creating new site...

rm -rf ~/deploy_rock.opens.science/public

echo Running PkgDown in deployment directory...

/usr/local/bin/R -e "pkgdown::build_site();"

echo Done with PkgDown. Copying old iROCK to a subdirectory of "public"...

mkdir -p public/iROCK
cp -R iROCK public
mkdir -p public/img
cp img/hex-logo.png public/img/hex-logo.png

echo Copied old iROCK. Deleting old contents in publi HTML directory.

rm -rf ~/rock.opens.science/*.*
rm -rf ~/rock.opens.science/*
rm -f ~/rock.opens.science/.htaccess

echo Deleted old contents. Copying new contents.

### Copy website
cp -RT public ~/rock.opens.science

### Copy .htaccess
cp -f .htaccess ~/rock.opens.science

echo Done copying over new website.

echo - - - ENDING DEPLOYMENT SCRIPT at $(date) - - -
