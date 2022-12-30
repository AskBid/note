### ssh access
```bash
$ mkdir ~/.ssh
$ cd ~/.ssh
$ ssh-keygen -t rsa -C "youremail@youremailcom"
```
now copy the .pub key created an head over Github.com to Account Settings -> SSH Keys -> Add Key.
Paste the .pub key, save.
Head back to terminal:
```bash
$ ssh-agent /bin/sh //only if `ssh-add` doesn't work.
$ ssh-add
$ exit
``` 
to test `ssh -T git@github.com`

```bash
$ git init
	#

$ rm -rf .git
	#

$ touch README.md
	#

$ git add <filename-or-path>
	#staging step

$ git commit -m "Initial commit"
	#

$ git status
	#

$ git restore .
	# gets rid of all the changes appearing in `git status`

$ git commit -a			
	# to avoid the staging (add) step, only if a first staging already happened

$ git commit -am
	# same but with message, also same as -a -m

$ git reset --soft HEAD~1
git reset --hard HEAD~1
git reset --hard 4a155e5
	# Undo Last Git Commit with reset

$ git clone <your-copied-github-url>
	#

$ git pull
    # will pull most recent commit

$ git remote
	#to see the names of each remote repository (or, "remote") available.

$ git remote show origin
	#

$ ls -a
	#

$ git remote add <nickname-of-origin> <your-copied-remote-repository-URL>
	# it just set a nickname to the otherwise long shh repository name

$ git remote -v
	#

$ git push -u <nickname-of-origin> branch
	# https://help.github.com/en/github/using-git/pushing-commits-to-a-remote-repository


$ git diff 
	# show differences of uncommitted changes
$ git diff --staged


$ git branch
	# show branches

$ git branch -la
	# list all

$ git branch <name-of-new-branch>
	#

$ git checkout <name-of-branch-to-move-to>
	#

$ git checkout -b <new-branch-name-to-move-to-straight-away>
	#

$ git checkout -
	# move back to previous branch

$ git log --graph
	# use "Space" to page down the history of commits. Use q to exit.
$ git log --oneline

$ git merge <branch-to-be-merged-into-current>
	# (merge = pull) important to be currently working on your target branch

$ git branch -d <branch-to-delete>
	# this is used after we merged the <branch-to-delete>
```