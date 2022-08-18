### ssh access
```
$ mkdir ~/.ssh
$ cd ~/.ssh
$ ssh-keygen -t rsa -C "youremail@youremailcom"
```
now copy the .pub key created an head over Github.com to Account Settings -> SSH Keys -> Add Key.
Paste the .pub key, save.
Head back to terminal:
```
$ ssh-agent /bin/sh //only if `ssh-add` doesn't work.
$ ssh-add
$ exit
``` 
to test `ssh -T git@github.com`