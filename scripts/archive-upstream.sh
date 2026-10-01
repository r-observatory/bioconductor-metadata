#!/usr/bin/env bash
# Commits OUT_DIR/upstream/ to the orphan upstream-archive branch of REMOTE
# when any byte changed, with OUT_DIR/archive-message.txt as the message.
# Files already on the branch and absent this run are kept.
# usage: archive-upstream.sh OUT_DIR REMOTE
set -euo pipefail
out="$1"
remote="$2"
branch="upstream-archive"
src="$out/upstream"

if [ ! -d "$src" ] || [ -z "$(find "$src" -type f -print -quit)" ]; then
  echo "No upstream files this run."
  exit 0
fi

dir="$(mktemp -d)"
trap 'rm -rf "$dir"' EXIT

set +e
git ls-remote --exit-code --heads "$remote" "$branch" >/dev/null
rc=$?
set -e
case "$rc" in
  0) git clone -q --depth 1 --branch "$branch" --single-branch "$remote" "$dir" ;;
  2) git init -q -b "$branch" "$dir"
     git -C "$dir" remote add origin "$remote" ;;
  *) echo "Could not read the $branch branch (git exit $rc)." >&2
     exit 1 ;;
esac

cp -R "$src/." "$dir/"
git -C "$dir" add -A
if git -C "$dir" diff --cached --quiet; then
  echo "Upstream files unchanged since the last archive commit."
  exit 0
fi

git -C "$dir" -c user.name="github-actions[bot]" \
  -c user.email="41898282+github-actions[bot]@users.noreply.github.com" \
  -c commit.gpgsign=false commit -q -F "$out/archive-message.txt"
git -C "$dir" push -q origin "HEAD:refs/heads/$branch"
git -C "$dir" log --oneline -1
