#!/bin/bash

set +e

echo 'Project Statistics'

# We want
#
# cyclomatic complexity
# line of code
# lines of code, by prod/test
# % code between languages
# hx/htmx commands used/number of times
# hotspots/commit analysis on file change frequency?

all_files=`git ls-files`

# total_lines will include files without an extension but later we show lines by
# extension
total_lines=`git ls-files | grep -Ev "(mock|test|lock\.json|_templ.go)" | xargs cat | wc -l`
exts=`git ls-files | awk -F. 'NF>1 && $NF !~ /\// && !a[$NF]++{print $NF}'`
exts="$exts no-ext"

echo '+--------------+------------+------------+------------+'
echo '| ext          | prod-lines | test-lines | %-share    |'
echo '|--------------|------------|------------|------------|'
for x in $exts; do
    if [ "$x" = "no-ext" ]; then
        test_lines=`git ls-files | grep "test" | awk -F. 'NF==1 || $NF ~ /\//' | xargs cat 2>/dev/null | wc -l`
        prod_lines=`git ls-files | grep -Ev "(mock|test|lock\.json|_templ.go)" |  awk -F. 'NF==1 || $NF ~ /\//' | xargs cat 2>/dev/null | wc -l`
    else
        test_lines=`git ls-files | grep "test" | grep "$x\$" | xargs cat 2>/dev/null | wc -l`
        prod_lines=`git ls-files | grep -Ev "(mock|test|lock\.json|_templ\.go)" | grep "$x\$" | xargs cat 2>/dev/null | wc -l`
    fi
    echo $x $prod_lines $test_lines
done | sort -k2,2nr | awk -v total="$total_lines" '{
    ext_total = $2 + $3
    if (total > 0) {
        percentage = (ext_total / total) * 100
    } else {
        percentage = 0
    }
    printf "| %-12s | %10s | %10s | %9.2f%% |\n", $1, $2, $3, percentage
}'
echo '+--------------+------------+------------+------------+'

# Longer than 80 we want to know about, more than 3000 it's probably
# minified/generated.
long_files=$(git ls-files | grep -Ev "(mock|test|lock\.json|_templ.go)" | xargs wc -L --total=never | awk '$1 > 80 && $1 < 3000' | sort -rn | head -10)
if [[ -n "$long_files" ]]; then
    echo
    echo 'Files with long line lengths:'
    echo "$long_files"
fi

echo
echo 'General'
echo $total_lines

