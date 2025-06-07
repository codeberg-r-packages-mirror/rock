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

echo Running PkgDown in deployment directory...

### Render the site
if /usr/local/bin/R -e "pkgdown::build_site();" then

# https://unix.stackexchange.com/questions/22726/how-to-conditionally-do-something-if-a-command-succeeded-or-failed

  echo Done with PkgDown. Copying old iROCK to a subdirectory of "public"...

  mkdir public/iROCK
  cp -R iROCK public
  mkdir public/img
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

else

  echo ERROR: PkgDown failed, doing nothing!

fi

echo - - - ENDING DEPLOYMENT SCRIPT at $(date) - - -
