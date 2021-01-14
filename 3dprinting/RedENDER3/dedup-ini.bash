echo > config_bundle_dedup.ini 

cat <(sed -e 's/^$/\xBF/' < config_bundle.ini) <(printf '\xBF') | while read -d $'\xBF' var
do
    newline=$"\n"
    header="$( cut -d $'\n' -f 1 <<< "$var" )"
    pairs="$( cut -d $'\n' -f 2- <<< "$var" )"
    (
	echo "$header" 
	echo "$pairs" | sort -u -t' ' -k1,1 
	printf "\n" 
    ) >> config_bundle_dedup.ini
done
