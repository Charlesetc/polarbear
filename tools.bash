
# tools for testing and building

function new_test () {
  file=$(basename $1 .plr).plr
  vim ./test/parse/input/$file
}

function generate_test () {
  file=$(basename $1 .plr).plr
  ./polarbear_main.native < ./test/parse/input/$file | tee ./test/parse/output/$file
}

function list_tests () {
  ls ./test/parse/input
}

function remove_test () {
  file=$(basename $1 .plr).plr
  rm ./test/parse/input/$file ./test/parse/output/$file
}

function run_test () {
  file=$(basename $1 .plr).plr
  temp=$(mktemp '/tmp/XXXX.plrt')

  if ./polarbear_main.native < ./test/parse/input/$file > $temp && cmp ./test/parse/output/$file $temp
  then
    echo -e "$file \033[1;32my\033[0m"
  else
    echo >&2
    echo -e "$file " >&2
    diff "$temp" "./test/parse/output/$file" >&2
    echo >&2
    echo -e "$file \033[0;31mn\033[0m"
  fi
  rm $temp
}

function run_all() {
  $0 list_tests | while read testname; do $0 run_test $testname; done | column -t
}

function help () {
  declare -F | awk '{ print $3 }'
}

# execute the arguments
$@
