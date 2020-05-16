#!/bin/bash

# settings
CONTAINER_ID=cef3ef8c8f43
MYSQL_USER=root
MYSQL_PASSWORD=an0i28gdnn9

# execute import
echo "Starting import procedure!"
while read FNAME; do

  # extract csv file
  echo "extracting $FNAME..."
  tar -zxvf ../data/20181113_training_set.tar.gz $FNAME -C ./import --strip-components=1
  mv ./$(basename $FNAME) ./import/sequences
  echo "File extracted and available in /import/sequences"

  # import csv sequences into mysql
  echo "importing in mysql..."
  H=$(head -n 1 ./import/sequences)
  docker exec $CONTAINER_ID mysqlimport --user=$MYSQL_USER --password=$MYSQL_PASSWORD --columns=$H --ignore-lines=1 --fields-terminated-by=',' seqdb /import/sequences

  #docker exec $CONTAINER_ID mysql --user=$MYSQL_USER --password=$MYSQL_PASSWORD seqdb -e "SELECT * FROM sequences LIMIT 1" > out.tab

  # remove csv file
  echo "removing data..."
  rm ./import/sequences
done <tardump
echo "Import finished!"


