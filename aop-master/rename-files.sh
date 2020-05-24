# To be used as follows 
# find . -name '*.prolog' -exec ./rename-files.sh {} \;
mv $1 ${1%.prolog}.pl
