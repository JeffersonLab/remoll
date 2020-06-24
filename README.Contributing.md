# Contributing to the repository

If you participate in the development of simulations for the MOLLER experiment (code, geometry, macros, analysis), you are strongly encouraged to store your work in this repository so it is available for future use.

The instructions below assume that you have cloned the repository.

## Access permissions

To contribute code to this repository, you must be added to the group of permitted users. Contact Rakitha Beminiwattha, Wouter Deconinck, Ciprian Gal, Paul King, or Kent Paschke.

## Make changes in branches

Before starting work make sure you have the latest changes from the remote repository:
```
git pull
```

Create a branch (see https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging for more details on branching) for the issue/improvement you are trying to add:
```
git checkout -b branch-name
```
You are now in new branch named "branch-name". Good names for branches are short and descriptive. Often they start with `bugfix-`, `feature-`, `enhancement-`, or a similar tag that allows to easily classify them.

## Sharing branches with others

If you want others to see your work make sure you setup tracking of this branch on the remote repository:
```
git push -u origin remote-branch-name
```
Note that the remote-branch-name can be different from the name you use on your local copy (i.e. the currently checked out branch). If remote-branch-name does not exist on the remote server yet then a new branch with that name will be created there (but not on your local copy), and your local branch will be set up to track this remote branch.

## Adding and committing changes

Modify any files you need to change. For any modified files, add and commit them:
```
git add folder/modified_file.hh
git commit
```
Enter a descriptive commit message, starting with a short title line and expanding in the body of the message. Try to keep commits as small as reasonable.

## Pushing commits to the repository

At this point your code changes are tracked and committed on the local repository. To make changes available to others on a remote branch (can be the same as the name you use on your local copy):
```
git push -u origin remote-branch-name
```
or simply `git push` if you are already tracking a remote branch.

## Submit pull request into the develop branch

If, after sufficient testing, you think your changes merits inclusion in the main development branch, submit a pull request on the GitHub site.

Enter a clear pull request description, and indicate what kind of testing you have completed. Tag a reviewer and wait until a maintainer merges the request into the `develop` branch. Do not merge the pull request into `develop` yourself without discussion with maintainers.

Only submit pull requests for clear bug fixes against the `master` branch and do not merge these pull request into `master` yourself.
