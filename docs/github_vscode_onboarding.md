# GitHub + VS Code Onboarding (Student Workflow)

This guide is the required workflow for working in `NCC_PIE`.

Goal:
- Keep the repository public and reproducible.
- Prevent accidental changes to `main`.
- Let each student contribute safely through pull requests.

## 1. One-Time Setup

1. Install:
- Git: https://git-scm.com/downloads
- VS Code: https://code.visualstudio.com/

2. Open VS Code and install extensions:
- `GitHub Pull Requests and Issues` (GitHub.vscode-pull-request-github)
- `R` (REditorSupport.r)

3. Sign in:
- VS Code -> Accounts (bottom-left) -> Sign in to GitHub.

4. Set Git identity (run once in VS Code terminal):

```powershell
git config --global user.name "Your Name"
git config --global user.email "your_email@example.com"
```

5. Verify:

```powershell
git config --global --get user.name
git config --global --get user.email
```

## 2. Clone the Repository

1. In VS Code: `Ctrl+Shift+P` -> `Git: Clone`.
2. Paste repo URL:
- `https://github.com/jnelson54/NCC_PIE.git`
3. Choose local folder.
4. Open the cloned folder in VS Code.

## 3. First Pull and Branch Creation

Never work directly on `main`.

1. In terminal:

```powershell
git checkout main
git pull origin main
```

2. Create your branch (use your name and task):

```powershell
git checkout -b yourname/isotope-data-cleaning
```

3. Confirm current branch:

```powershell
git branch --show-current
```

## 4. Daily Work Cycle (Do This Every Session)

1. Start clean:

```powershell
git checkout main
git pull origin main
git checkout yourname/isotope-data-cleaning
git merge main
```

2. Make edits in VS Code.
3. Review changed files in Source Control tab.
4. Stage files (click `+` or `Stage All Changes`).
5. Commit with a clear message:
- Example: `Add isotope QC checks and harmonize station labels`
6. Push your branch:

```powershell
git push -u origin yourname/isotope-data-cleaning
```

After the first push, you can use:

```powershell
git push
```

## 5. Open a Pull Request (PR)

1. In VS Code Source Control, click `Create Pull Request` (or use GitHub website).
2. Set:
- Base branch: `main`
- Compare branch: your branch
3. PR title should be specific.
4. PR description must include:
- What changed
- Why it changed
- How you tested it
- Any outputs/files generated

5. Submit PR.

## 6. Respond to Review Comments

1. Make requested edits on your same branch.
2. Commit and push again:

```powershell
git add .
git commit -m "Address PR review comments"
git push
```

The PR updates automatically.

## 7. After PR Merge

1. Update local main:

```powershell
git checkout main
git pull origin main
```

2. Delete old local branch:

```powershell
git branch -d yourname/isotope-data-cleaning
```

3. Start next task with a new branch from updated `main`.

## 8. Rules You Must Follow

- Do not push directly to `main`.
- Do not use force push: `git push --force`.
- Do not delete or rewrite commit history.
- Keep commits small and meaningful.
- If unsure, stop and ask before committing.

## 9. Common Errors and Fixes

### A) "rejected (non-fast-forward)" on push

Your branch is behind remote.

```powershell
git pull --rebase origin yourname/isotope-data-cleaning
git push
```

### B) "Please tell me who you are"

Set Git identity:

```powershell
git config --global user.name "Your Name"
git config --global user.email "your_email@example.com"
```

### C) Merge conflict

1. Open conflicted file in VS Code.
2. Use conflict buttons (`Accept Current`, `Accept Incoming`, etc.).
3. Save file.
4. Mark resolved and commit:

```powershell
git add .
git commit -m "Resolve merge conflict"
```

### D) Committed to wrong branch

Stop and ask before trying to fix history.

## 10. Repo-Specific Notes (NCC_PIE)

- Main hydrography workflow: `analysis/run_all.R`
- Sonde outputs: `analysis/output/sonde/`
- Figures: `analysis/figures/`
- Repro scripts and reports: `repro/scripts/` and `repro/reports/`
- Manuscript files are intentionally gitignored and must not be committed.

## 11. Quick Command Cheat Sheet

```powershell
# Update main
git checkout main
git pull origin main

# Create new task branch
git checkout -b yourname/task-name

# Stage + commit + push
git add .
git commit -m "Describe your change"
git push -u origin yourname/task-name
```
