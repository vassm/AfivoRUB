#!/usr/bin/env bash
set -euo pipefail

# Sync upstream safely:
# - always fetch upstream
# - optionally fast-forward local main/master if:
#   * working tree is clean
#   * branch exists locally
#   * fast-forward is possible (no merge commits)
#
# Usage:
#   ./tools/sync_upstream.sh
#   ./tools/sync_upstream.sh --ff   (try fast-forward update of main/master)

DO_FF=0
if [[ "${1:-}" == "--ff" ]]; then
  DO_FF=1
fi

# Ensure we're in a git repo
git rev-parse --is-inside-work-tree >/dev/null

# Identify main branch name on your repo (main or master)
if git show-ref --verify --quiet refs/heads/main; then
  MAIN_BRANCH="main"
elif git show-ref --verify --quiet refs/heads/master; then
  MAIN_BRANCH="master"
else
  echo "Could not find local 'main' or 'master' branch."
  echo "Create one or edit this script to set MAIN_BRANCH manually."
  exit 1
fi

# Ensure upstream remote exists
if ! git remote get-url upstream >/dev/null 2>&1; then
  echo "Remote 'upstream' not found. Add it first:"
  echo "  git remote add upstream https://github.com/ORIGINAL_OWNER/REPO.git"
  exit 1
fi

CURRENT_BRANCH="$(git branch --show-current)"

echo "==> Fetching upstream..."
git fetch --prune upstream

UPSTREAM_REF="upstream/${MAIN_BRANCH}"
if ! git show-ref --verify --quiet "refs/remotes/${UPSTREAM_REF}"; then
  echo "Upstream branch '${UPSTREAM_REF}' not found."
  echo "Available upstream branches:"
  git branch -r | grep upstream/ || true
  exit 1
fi

LOCAL_SHA="$(git rev-parse ${MAIN_BRANCH} 2>/dev/null || true)"
UP_SHA="$(git rev-parse ${UPSTREAM_REF})"

echo "==> Local ${MAIN_BRANCH}:    ${LOCAL_SHA}"
echo "==> Upstream ${MAIN_BRANCH}: ${UP_SHA}"

if [[ "${LOCAL_SHA}" == "${UP_SHA}" ]]; then
  echo "==> You're already up to date with upstream/${MAIN_BRANCH}."
  exit 0
fi

echo "==> New commits on upstream/${MAIN_BRANCH} since your local ${MAIN_BRANCH}:"
git log --oneline --decorate "${MAIN_BRANCH}..${UPSTREAM_REF}" | head -n 20
echo "    (showing up to 20; run 'git log ${MAIN_BRANCH}..${UPSTREAM_REF}' for more)"

if [[ "${DO_FF}" -ne 1 ]]; then
  echo
  echo "Fetched upstream. No local branches changed."
  echo "To fast-forward your local ${MAIN_BRANCH} (only if clean), run:"
  echo "  $0 --ff"
  exit 0
fi

# Fast-forward attempt
echo
echo "==> Attempting fast-forward update of local ${MAIN_BRANCH}..."

# Only if working tree is clean
if [[ -n "$(git status --porcelain)" ]]; then
  echo "Working tree is not clean. Commit/stash changes first."
  echo "No fast-forward performed."
  exit 1
fi

# Temporarily switch to main branch to fast-forward it
git checkout "${MAIN_BRANCH}" >/dev/null

# Fast-forward only (fails if merge needed)
if git merge --ff-only "${UPSTREAM_REF}"; then
  echo "==> Fast-forwarded local ${MAIN_BRANCH} to ${UPSTREAM_REF}."
else
  echo "==> Cannot fast-forward (you have local commits or history diverged)."
  echo "No merge was done."
  exit 1
fi

# Switch back to original branch
if [[ "${CURRENT_BRANCH}" != "${MAIN_BRANCH}" && -n "${CURRENT_BRANCH}" ]]; then
  git checkout "${CURRENT_BRANCH}" >/dev/null
fi

echo "==> Done."
