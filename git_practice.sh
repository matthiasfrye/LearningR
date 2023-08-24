### Cloned repository: getting started
#create local directory
mkdir git-example
cd git-example

#create a cop of repo in working directory"
git clone https://github.com/rairizarry/murders.git

#get status of files
git status


##initial file version
#create a new files
echo "test this" >> new-file.txt
echo "temp" >> temporary.txt

#stage file = add file to staging area, file is now being tracked
git add new-file.txt

#check status to confirm
git status

#commit file with comment
git commit -m "initial version"

#check status to confirm
git status


##new version of file
#alter new-file
echo "new additional line" >> new-file.txt

#check status to confirm change
git status

#stage file = add file to staging area
git add new-file.txt

#check status to confirm
git status

#commit file with new comment
git commit -m "new version"

#check status to confirm
git status


## shortcut add/commit
#alter new-file
echo "second additional line" >> new-file.txt

#check status to confirm change
git status

#stage and commit file with new comment
git commit -m "3 lines now" new-file.txt

#check status to confirm
git status


##track changes
git log new-file.txt

##push all changes upstream
git push

#get changes from upstream repo
git fetch

#get changes into working directory
git merge

#shortcut fetch+merge
git pull



### own repository - create repo in GitHub!
cd Data Science/murders

# initialise local repo
git init

#add and commit one file
git add README.txt
git commit -m "first commit"

#connect local report to upstream repo
#git branch -M main
git remote add origin git@github.com:matthiasfrye/murders.git

#push changes upstream
git push --set-upstream origin master
#later
git push
