#!/usr/bin/env sh
set -eu

cobc -x -free trip_budget.cob -o trip_budget
./trip_budget data/expenses.csv > test/expected_report.txt
./trip_budget data/expenses.csv
