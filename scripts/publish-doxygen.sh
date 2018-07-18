#!/bin/bash

# Settings
REPO_PATH=https://github.com/JeffersonLab/remoll.git
HTML_PATH=doc/html
CHANGESET=$(git rev-parse --verify HEAD)

# Get a clean version of the HTML documentation repo.
rm -rf ${HTML_PATH}
mkdir -p ${HTML_PATH}
git clone -b gh-pages "${REPO_PATH}" --single-branch ${HTML_PATH}

# rm all the files through git to prevent stale files.
cd ${HTML_PATH}
git rm -rf .
cd -

# Generate the HTML documentation.
doxygen doc/remolldox.conf

# Create and commit the documentation repo.
cd ${HTML_PATH}
git add .
git commit -m "Automated documentation build for changeset ${CHANGESET}."
cd -

# Leave push to the user
echo ""
echo "Completed. If you are satisfied with the result, push to gh-pages using:"
echo "  cd ${HTML_PATH}"
echo "  git status"
echo "  git push origin gh-pages"
