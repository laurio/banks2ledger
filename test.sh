#! /bin/bash

bb test-runner.clj
export RC=$?
if [ $RC -ne 0 ]; then
  exit $RC
fi

echo
echo Now running end-to-end tests:

LEDGER="test/data/ledger.dat"

rm -f test/data/*.out

echo -n "testing bb.csv... "
bb src/banks2ledger/banks2ledger.clj \
  --ledger-file $LEDGER \
  --csv-file test/data/bb.csv \
  --csv-skip-header-lines 3 \
  --csv-skip-trailer-lines 2 \
  --date-format 'yyyy/MM/dd' \
  --ref-col 3 \
  --amount-col 4 \
  --descr-col '%9!%1 %6 %7 %8' \
  --account 'Assets:BB Account' \
  --currency HUF \
  >test/data/bb.out
diff -u test/data/bb.out test/data/bb.ref-out >/dev/null
export RC=$?
if [ $RC -ne 0 ]; then
  echo "FAIL, inspect bb.out!"
  exit $RC
else
  rm test/data/bb.out
  echo "OK"
fi

echo -n "testing ica.csv... "
bb src/banks2ledger/banks2ledger.clj \
  --ledger-file $LEDGER \
  --csv-file test/data/ica.csv \
  --csv-field-separator ';' \
  --csv-skip-header-lines 1 \
  --amount-col 4 \
  --descr-col '%1' \
  --account 'Assets:ICA Account' \
  --amount-decimal-separator "," \
  --amount-grouping-separator " " \
  >test/data/ica.out
diff -u test/data/ica.out test/data/ica.ref-out >/dev/null
export RC=$?
if [ $RC -ne 0 ]; then
  echo "FAIL, inspect ica.out!"
  exit $RC
else
  rm test/data/ica.out
  echo "OK"
fi

echo -n "testing seb.csv... "
bb src/banks2ledger/banks2ledger.clj \
  --ledger-file $LEDGER \
  --csv-file test/data/seb.csv \
  --csv-skip-header-lines 5 \
  --ref-col 2 \
  --amount-col 4 \
  --descr-col '%3' \
  --hooks-file test/data/hooks.clj \
  --account 'Assets:SEB Account' \
  >test/data/seb.out
diff -u test/data/seb.out test/data/seb.ref-out >/dev/null
export RC=$?
if [ $RC -ne 0 ]; then
  echo "FAIL, inspect seb.out!"
  exit $RC
else
  rm test/data/seb.out
  echo "OK"
fi
