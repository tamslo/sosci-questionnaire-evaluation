new_directory="AllData"
old_directory="_AllData"
for old_file in data/${old_directory}/* results/${old_directory}/*
do
  if [[ $old_file == *.csv ]]
  then
    new_file=${old_file/$old_directory/$new_directory}
    if [[ -f $new_file ]]
    then
      result=$(diff -q $old_file $new_file)
      if [[ $result != "" ]]
      then
        echo "⚠️  $result"
      else
        echo "✅ Files $old_file and $new_file match"
      fi
    fi
  fi
done