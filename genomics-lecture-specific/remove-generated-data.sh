directory="AllData"
for file in results/${directory} data/${directory}/*
do
    if [[ $file != *combination_config.json ]]
    then
        rm -rf $file
  fi
done