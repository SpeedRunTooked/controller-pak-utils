while true;
  do for f in "$@"; do
    clear
    echo "xxd $f"
    xxd $f
    #cat $f
    read -n 1;
  done;
done
