# Upstream sync notes

Remotes: `origin` = fork (push goes here), `upstream` = original repo (fetch updates from here, push disabled). Check: `git remote -v`.

Branches:
- `master` = upstream mirror (keep this close to `upstream/master`)
- `sdbd` = development branch (do all work here)

Daily work (on sdbd):
- edit / test
- `git add -A && git commit -m "..." && git push`  (pushes to `origin/sdbd`)

Check for upstream updates (safe, changes nothing):
- `bash tools/sync_upstream.sh`

Update upstream mirror (`master`) when clean (fast-forward only):
- `git checkout master`
- `bash tools/sync_upstream.sh --ff`
- `git push origin master`

Bring upstream updates into my work branch:
- `git checkout sdbd`
- `git merge master`
- `git push`

For tests work in afivo/tests, otherwise in programs/
