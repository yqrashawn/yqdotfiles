HERE=${PWD}
echo "${HERE}"
for BACKUPDIR in $(find . -type d)
do
    cd "$BACKUPDIR" && sh ~/local/bin/funcs/replaceFirstAndLastLine.sh
    cd "$HERE"
done
