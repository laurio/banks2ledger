#!/bin/bash -e
cd "$(dirname "${BASH_SOURCE[0]}")"
clojure -M -m uberdeps.uberjar --deps-file ../deps.edn --main-class banks2ledger.banks2ledger --target ../target/banks2ledger.jar
