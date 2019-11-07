for FILE in $(git ls-files)
do
    TIME=$(git log --pretty=format:%cd -n 1 --date=iso $FILE)
    TIME2=`echo $TIME | sed 's/-//g;s/ //;s/://;s/:/\./;s/ .*//'`
    touch -m -t $TIME2 $FILE
done
