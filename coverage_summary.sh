#!/usr/bin/env zsh
# coverage_summary.sh
# Concise scalacf coverage summary (robust): uses Cobertura XML hits="0".

set -euo pipefail

OUTFILE="/tmp/scalacf_coverage_summary.txt"
LOGFILE="/tmp/scalacf_coverage_run.log"

COB_XML="target/scala-3.8.1/coverage-report/cobertura.xml"
SCOV_XML="target/scala-3.8.1/scoverage-report/scoverage.xml"

rm -f "$OUTFILE" "$LOGFILE"

echo "Running sbt coverage..."
sbt clean coverage test coverageReport | tee "$LOGFILE" >/dev/null

if [[ ! -f "$COB_XML" ]]; then
  echo "ERROR: Cobertura XML not found at: $COB_XML"
  echo "See log: $LOGFILE"
  exit 1
fi

STMT_LINE="$(grep -E 'Statement coverage\.\:' "$LOGFILE" | tail -n 1 || true)"
BRCH_LINE="$(grep -E 'Branch coverage\.\.\.\.' "$LOGFILE" | tail -n 1 || true)"

{
  echo "============================================================"
  echo "SCALACF COVERAGE SUMMARY"
  echo "Generated: $(date)"
  echo "============================================================"
  echo
  echo "Overall coverage (from sbt output):"
  [[ -n "$STMT_LINE" ]] && echo "  $STMT_LINE" || echo "  (Statement coverage line not found)"
  [[ -n "$BRCH_LINE" ]] && echo "  $BRCH_LINE" || echo "  (Branch coverage line not found)"
  echo
  echo "Log file:     $LOGFILE"
  echo "Cobertura:    $COB_XML"
  if [[ -f "$SCOV_XML" ]]; then
    echo "Scoverage XML: $SCOV_XML"
  fi
  echo
  echo "============================================================"
  echo "TOP UNCOVERED FILES (by uncovered line count)"
  echo "============================================================"
  echo
  echo "Count  File"
  echo "-----  ----"
} > "$OUTFILE"

python3 - "$COB_XML" >> "$OUTFILE" <<'PY'
import sys
import xml.etree.ElementTree as ET
from collections import Counter, defaultdict

path = sys.argv[1]
tree = ET.parse(path)
root = tree.getroot()

# Cobertura structure:
# <coverage><packages><package><classes><class filename="..."><lines><line number=".." hits=".."/>
uncovered = Counter()
lines_by_file = defaultdict(list)

for cls in root.findall(".//class"):
  fn = cls.attrib.get("filename", "UNKNOWN")
  for line in cls.findall(".//line"):
    hits = line.attrib.get("hits")
    num = line.attrib.get("number")
    if hits == "0" and num and num.isdigit():
      uncovered[fn] += 1
      lines_by_file[fn].append(int(num))

if not uncovered:
  print("(no uncovered lines detected in cobertura.xml — verify coverage really ran and file isn’t empty)")
  sys.exit(0)

for fn, cnt in uncovered.most_common(25):
  print(f"{cnt:5d}  {fn}")

print()
print("============================================================")
print("UNCOVERED LINE NUMBERS (TOP 10 FILES)")
print("============================================================")
print()

for i, (fn, cnt) in enumerate(uncovered.most_common(10), start=1):
  print("------------------------------------------------------------")
  print(f"[{i}] Uncovered lines: {cnt}")
  print(f"File: {fn}")
  ls = sorted(set(lines_by_file[fn]))
  # print as one wrapped-ish line; still concise
  print("Lines:")
  print(" ".join(map(str, ls)))
  print()
PY

{
  echo "============================================================"
  echo "END OF REPORT"
  echo "============================================================"
} >> "$OUTFILE"

echo
echo "Coverage summary written to: $OUTFILE"
echo "Log written to:              $LOGFILE"