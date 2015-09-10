#!/bin/bash
GH_REPO="@github.com/RomanTsegelskyi/yummlyr.git"
FULL_REPO="https://$GH_TOKEN$GH_REPO"

mkdir out
cd out
git init
git remote add origin $FULL_REPO
git fetch
git config user.name "rapporter-travis"
git config user.email "travis"

# handle master
git checkout master
CHANGED_FILES=`git show --stat $TRAVIS_COMMIT`
# check if vignettes were updated
if [[ $CHANGED_FILES =~ .*vignettes.*\.Rmd.* ]]
then
  R -e 'devtools::install_github("rstudio/rmarkdown"); devtools::build_vignettes()'
  git add inst/doc
fi
# check if readme was update
if [[ $CHANGED_FILES =~ .*README\.Rmd.* ]]
then
  R -e 'devtools::install_github("rstudio/rmarkdown"); rmarkdown::render("README.Rmd")'
  git add README.md
fi
git commit -m "Update by travis after $TRAVIS_COMMIT"
git push origin master
