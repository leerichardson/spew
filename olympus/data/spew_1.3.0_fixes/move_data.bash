#!/bin/bash

base_dir=/mnt/beegfs1/data/shared_group_data/syneco
echo $base_dir

geohive_dir=$base_dir/old/spew_olympus/getting_data/geohive
echo $geohive_dir

ipums_shapedir=$base_dir/old/spew_olympus/getting_data/ipums/world_geolev1
echo $ipums_shapedir

ipums_dir=$base_dir/old/spew_olympus/getting_data/ipums/countries
echo $ipums_dir

# Palestine ---
palestine_dir=$base_dir/spew_input/asia/western_asia/pse/input

# Remove the Palestine data w/ rm -r counts, rm -r shapefile 
mkdir -p $palestine_dir/counts/geohive/2014/2
mkdir -p $palestine_dir/counts/geohive/2014/4
mkdir -p $palestine_dir/shapefiles/ipums/2007/1
cp $geohive_dir/palestine/palestine_extended.csv $palestine_dir/counts/geohive/2014/4
cp $geohive_dir/palestine/palestine_admin.csv $palestine_dir/counts/geohive/2014/2
cp $ipums_shapedir/palestine* $palestine_dir/shapefiles/ipums/2007/1

# Burkina Faso ---
bfa_dir=$base_dir/spew_input/africa/western_africa/bfa/input
#rm -r $bfa_dir/counts/geohive
#rm -r $bfa_dir/shapefiles/ipums

mkdir -p $bfa_dir/counts/geohive/2013/2
mkdir -p $bfa_dir/counts/geohive/2013/4
mkdir -p $bfa_dir/shapefiles/ipums/2006/1

cp $geohive_dir/burkina/burkina_admin.csv $bfa_dir/counts/geohive/2013/2
cp $geohive_dir/burkina/burkina_extended.csv $bfa_dir/counts/geohive/2013/4
cp $ipums_shapedir/burkina_faso.* $bfa_dir/shapefiles/ipums/2006/1

# Czech Republic ---
cze_dir=$base_dir/spew_input/europe/eastern_europe/cze/input
#rm -r $cze_dir/counts/geohive
#rm -r $cze_dir/shapefiles/ipums

new_cze_count=$cze_dir/counts/geohive/2015
new_cze_shape=$cze_dir/shapefiles/ipums/2009/1

mkdir -p $new_cze_count/2
mkdir -p $new_cze_count/4
mkdir -p $new_cze_shape

cp $geohive_dir/czech/czech_admin.csv $new_cze_count/2
cp $geohive_dir/czech/czech_extended.csv $new_cze_count/4
cp $ipums_shapedir/czech_republic.* $new_cze_shape

# Guinea ---
gin_dir=$base_dir/spew_input/africa/western_africa/gin/input
#rm -r $gin_dir/counts/geohive
#rm -r $gin_dir/shapefiles/ipums

new_gin_count=$gin_dir/counts/geohive/2014
new_gin_shape=$gin_dir/shapefiles/ipums/1996/1

mkdir -p $new_gin_count/2
mkdir -p $new_gin_count/4
mkdir -p $new_gin_shape

cp $geohive_dir/guinea/guinea_admin.csv $new_gin_count/2
cp $geohive_dir/guinea/guinea_extended.csv $new_gin_count/4
cp $ipums_shapedir/guinea.* $new_gin_shape

# Kyrgyzstan ---
kgz_dir=$base_dir/spew_input/asia/central_asia/kgz/input

new_kgz_count=$kgz_dir/counts/geohive/2015
new_kgz_shape=$kgz_dir/shapefiles/ipums/2009/1

mkdir -p $new_gin_count/2
mkdir -p $new_gin_count/4
mkdir -p $new_gin_shape

cp $geohive_dir/kyrgyzstan/kyrgyzstan_admin.csv $new_kgz_count/2
cp $geohive_dir/kyrgyzstan/kyrgyzstan_extended.csv $new_kgz_count/4
cp $ipums_shapedir/kyrgyz_republic.* $new_kgz_shape

mkdir -p $kgz_dir/pums/ipums/2009/1

kgz_ipums=$base_dir/old/spew_olympus/getting_data/ipums/countries/kyrgyz_republic.csv
cp $kgz_ipums $kgz_dir/pums/ipums/2009/1

# Sierra Leone ---
sle_dir=$base_dir/spew_input/africa/western_africa/sle/input

new_sle_count=$sle_dir/counts/geohive/2015
new_sle_shape=$sle_dir/shapefiles/ipums/2004/1

mkdir -p $new_sle_count/2
mkdir -p $new_sle_shape

cp $geohive_dir/sierraleone/sierraleone_admin.csv $new_sle_count/2
cp $ipums_shapedir/sierra_leone.* $new_sle_shape

# Puerto Rico ---
pri_dir=$base_dir/spew_input/americas/caribbean/pri/input

new_pri_count=$pri_dir/counts/geohive/2010
new_pri_shape=$pri_dir/shapefiles/ipums/2010/1

mkdir -p $new_pri_count/2
mkdir -p $new_pri_shape

cp $geohive_dir/us-pr/us-pr_admin.csv $new_pri_count/2
cp $ipums_shapedir/puerto_rico.* $new_pri_shape

