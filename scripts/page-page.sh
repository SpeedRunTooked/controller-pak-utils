for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16;
  do for f in "$@"; do
    xxd $f | sed -n "$i"p;
  done;
  read -n 1;
done
