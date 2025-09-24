#!/usr/bin/env bash
#
# fetch_metadata.sh — Download metadata_qmd.yaml from all IPBES_TG_* repos
# into ./metadata.qmd/<repo>.yaml (within IPBES_TG_Directory).
#
# Uses GitHub API to list repos in org IPBES-Data, filters IPBES_TG_*, excludes
# IPBES_TG_Directory. Then fetches
#   https://ipbes-data.github.io/<repo>/metadata_qmd.yaml
#
# Requires: curl, jq (optional but preferred). If jq not available, uses grep/sed.

set -euo pipefail

cd "$(dirname "$0")/.."  # into IPBES_TG_Directory
outdir="metadata.qmd"
rm -rf "$outdir"
mkdir -p "$outdir"

ORG="IPBES-Data"
API="https://api.github.com/orgs/${ORG}/repos?per_page=200&sort=full_name"

if command -v jq >/dev/null 2>&1; then
  repos=$(curl -fsSL "$API" | jq -r '.[].name' | grep -E '^IPBES_TG_' | grep -v '^IPBES_TG_Directory$' | LC_ALL=C sort -f)
else
  repos=$(curl -fsSL "$API" | grep -E '"name":\s*"IPBES_TG_' | sed -E 's/.*"name"\s*:\s*"([^"]+)".*/\1/' | grep -v '^IPBES_TG_Directory$' | LC_ALL=C sort -f)
fi

echo "Found repos:" >&2
printf '  %s\n' $repos >&2 || true

for repo in $repos; do
  dst="${outdir}/${repo}.yaml"
  # Try .yaml first
  url_yaml="https://ipbes-data.github.io/${repo}/metadata_qmd.yaml"
  # Fallback: some repos may publish .yml
  url_yml="https://ipbes-data.github.io/${repo}/metadata_qmd.yml"
  echo "Fetching ${url_yaml} (or .yml) -> ${dst}" >&2
  if curl -fsSL "$url_yaml" -o "$dst"; then
    :
  elif curl -fsSL "$url_yml" -o "$dst"; then
    :
  else
    echo "  WARN: metadata not found for ${repo} (skipping)" >&2
    rm -f "$dst" || true
  fi
done

echo "Done. Files in $outdir/"
