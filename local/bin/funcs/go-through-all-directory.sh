for BACKUPDIR in $(find . -type d)
do
    BASE=$(dirname "${BACKUPDIR}")

    LASTPART=$(basename "${BACKUPDIR}")
    D1=${LASTPART#}
    D2=${D1%.*}

    if [ -d "${BASE}/${D2}" ]
    then
        echo "${BASE}/${D2}"
        sh ~/local/bin/funcs/replaceFirstAndLastLine.sh
    fi

done
