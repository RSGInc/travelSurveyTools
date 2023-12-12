## Contributing

This project uses a [feature-branch](https://deepsource.io/blog/git-branch-naming-conventions/) naming convention and workflow. `main` is the main branch (not `master`), base your work off of `main`. Contribute to the project by making changes to your own feature branch and issuing pull-requests when you're ready to integrate into the `main` branch.

### Create a feature branch

Pull the `main` branch; `git pull`, and if necessary `git checkout main` to switch to `main`

Create a feature branch and check out your branch, e.g., `git checkout -b crosstab-performance`
  * You can use your initials to prefix a feature branch, e.g.,
  `aa-crosstab-performance`.

### Your feature branch should do one thing only

For example: 

  * debug an error,
  * create a new function,
  * generate a new part of the dashboard, 
  * integrate a new dataset, or
  * fix an issue - [please name your branch with the issue number](https://deepsource.io/blog/git-branch-naming-conventions/)

### Save and share your work

***Commit*** changes related to your feature and push them to GitHub. 

***Push*** changes to your feature branch at any time.

***Create a pull request*** on GitHub When you're ready to have your work reviewed. You can submit a PR before you are done, if you want guidance on your work-in-progress.

Make changes or respond to comments in your pull-request reviews. New commits pushed to your branch will update the pull-request. 

### Keep it clean

When your pull request is approved the approver will merge your branch into main and may delete your branch from GitHub.

To remove deleted feature branches from your local copy of the repository run `git remote prune origin`.

Do not attempt to push additional commits to a merged pull-request. Instead, start a new feature branch and issue a new pull request.

Remember to update and branch off of `main` whenever you start a new feature, e.g., `git checkout main; git pull origin main; git checkout -b a-new-feature`.
