mkdir -p data
wget -O data/hsk-word-list.tsv https://github.com/tomcumming/hsk-word-list/raw/refs/heads/master/hsk-word-list.tsv
wget -O data/tocfl.tsv https://github.com/tomcumming/tocfl-word-list/raw/refs/heads/master/dist/tocfl.tsv

wget -O data/hanzi-writer.js https://cdn.jsdelivr.net/npm/hanzi-writer/dist/hanzi-writer.js
wget -O data/hanzi-writer-data.tar.gz https://github.com/chanind/hanzi-writer-data/archive/refs/tags/v2.0.1.tar.gz

tar -xzf data/hanzi-writer-data.tar.gz -C data
mv data/hanzi-writer-data-2.0.1 data/hanzi-writer-data
