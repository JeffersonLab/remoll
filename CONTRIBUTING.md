## Creating a fork:
Click the 'fork' button on the top right of the remoll repository. You can now clone this repository:
  git clone git@github.com:USERNAME/remoll.git
This will fork all current branches in the jeffersonlab/remoll repository.

## Adding a new remote (if you create a fresh clone of your forked repository):
- Add the upstream repository jeffersonlab/remoll to your list of remotes (you only need to do this once):
  git remote add upstream https://github.com/jeffersonlab/remoll.git

## Renaming an existing remote (if you already had the base repository checked out):
After creating a fork, you will need to add this fork as the primary remote (origin) of your local copy.
- Rename the previous remote (jeffersonlab/remoll):
  git remote rename origin upstream
- Add your forked repository as a new remote:
  git remote add origin git@github.com:USERNAME/remoll.git

## Keeping your fork up to date:
- Whenever you want to update, fetch the latest upstream changes:
  git fetch upstream
  Variant: git fetch --all
- ... and merge the upstream branch in your branch, e.g. for the 'develop' branch
  git checkout develop
  git merge upstream/develop
Avoid making changes directly on master or develop, even in your own repository (for the same reasons).

## Doing your work:
It is easiest to do your work in branches, one per logically separate tasks, and to merge those to jeffersonlab/remoll frequently.
- Create a new branch from the develop branch (the base of your branch):
  git checkout develop
  git branch new-feature-xyz
  git checkout new-feature-xyz
- Now, you can do your work in this branch.
- Periodically, push the changes to your repository:
  git push

## Cleaning up your work:
Before contributing your changes back to jeffersonlab/remoll for others to use, it make sense to clean up some.
- Make sure your branch works with the most up to date code in the upstream base branch:
  git fetch upstream
  git checkout develop
  git merge upstream/develop
- Rebase your branch to point to the most recent code in the base branch:
  git checkout new-feature-xyz
  git rebase develop
The rebase phase is when you may need to resolve some conflicts: when you have modified lines that someone else has modified upstream as well. Ask in Slack if you need help resolving conflicts (in git; we can't help with the neighbors).

## Submitting a pull request:
When you are done with your work in a branch and ready to share it with others, go to your branch page on GitHub and click the 'pull request' button. Write a clear summary and wait for someone to merge it (or request some changes, if necessary). After your branch has been merged, you can go ahead and delete it. When you update your develop branch, the changes will be included.

