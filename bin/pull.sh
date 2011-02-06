#
# Simple script to resolve a pull request
# Contributor must have cloned zguide.git
# Pass contributor name as single argument
#
# Syntax: sh bin/pull.sh contributor
#
set -x

git checkout -b $1-master master
git pull https://github.com/$1/zguide.git master
git checkout master
git merge $1-master
git push origin master
git branch -d $1-master
