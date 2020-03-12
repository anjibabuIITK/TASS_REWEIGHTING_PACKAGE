#
# This script is a part of TASS Reweighting Pachage
# Commands to use git 
# 
# Authour: Anji Babu Kapakayala
#
#
#!/bin/bash
case "$1" in

  "pull")
	git pull origin master;;
  "push")
	git push origin master;;
  "status")
	git status ;;
  "add")
	git add *
	git status;;
  "commit")
	git commit -m $2
	git status;;
  "*")
	echo "Give appropriate git command";;
esac

