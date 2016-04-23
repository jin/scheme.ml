for example in "$@"
do
  filename=`basename $example`
  bash -c "diff <(./scheme < examples/$filename) <(./scheme < examples/test/$filename)"
done

echo "Test completed"
