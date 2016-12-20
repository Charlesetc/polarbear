
# tools for testing and building

function new_test () {
  file=$(basename $1 .plr).plr
  vim ./test/parse/input/$file
}

function generate_test () {
  file=$(basename $1 .plr).plr
  ./snow_frontend.native < ./test/parse/input/$file | tee ./test/parse/output/$file
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

  if ./snow_frontend.native < ./test/parse/input/$file > $temp && cmp ./test/parse/output/$file $temp
  then
    temp2=$(mktemp '/tmp/XXXX.plrt')

    # also assert that the generated output can be parsed
    if ./snow_frontend.native < ./test/parse/output/$file > $temp2 && cmp $temp $temp
    then
      echo -e "$file \033[1;32my\033[0m"
    else

      # set as failed
      rm -f $2

      echo >&2
      diff "$temp" "$temp2" >&2
      echo >&2
      echo -e "$file \033[0;31mn\033[0m"
    fi
    rm $temp2
  else

    # set as failed
    rm -f $2

    echo >&2
    echo -e "$file " >&2
    diff "$temp" "./test/parse/output/$file" >&2
    echo >&2
    echo -e "$file \033[3;31mn\033[0m"
  fi
  rm $temp
}

function run_all() {
  success_metric=$(mktemp /tmp/XXXX.boolean)

  {
  $0 list_tests | xargs -P 0 -IX $0 run_test X $success_metric
  if [ -f $success_metric ] ; then
    echo
    echo -e "all \033[0;32m______\033[0m"
    rm -f $success_metric
  else
    echo
    echo -e "all \033[0;31m______\033[0m"
  fi
} | column -t
}

function help () {
  declare -F | awk '{ print $3 }'
}

# execute the arguments
$@
