for example in "$@"
do
  filename=`basename $example`
  bash -c "diff <(bin/scheme < examples/$filename) <(bin/scheme < examples/test/$filename)"
done
