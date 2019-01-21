#!/bin/bash
set -e
version="`cat VERSION`"
tagname="v$version"
commit="`git rev-parse HEAD`"
git config --global user.email "Thierry.Martinez@inria.fr"
git config --global user.name "Thierry Martinez"
cd ~/ocamlcodoc
git pull origin master
if [[ "`git rev-parse HEAD`" != "$commit" ]]; then
    echo "Too recent commit!"
    exit 1
fi
git tag -a "$tagname" -m "Version $version"
git push origin "$tagname"
archive="ocamlcodoc-$tagname.tar.gz"
url="https://gitlab.inria.fr/tmartine/ocamlcodoc/-/archive/$tagname/$archive"
wget "$url"
md5=`md5sum "$archive" | cut -d " " -f 1`
cd ~/opam-repository
git pull origin master
branch="ocamlcodoc.$version"
git checkout -b "$branch"
repo="packages/ocamlcodoc/ocamlcodoc.$version"
mkdir -p "$repo"
opamfile="$repo/opam"
cp ~/ocamlcodoc/ocamlcodoc.opam "$opamfile"
cat >>$opamfile <<EOF
url {
  src: "$url"
  checksum: "md5=$md5"
}
EOF
git add "$opamfile"
git commit -m "ocamlcodoc.$version"
git push perso "$branch"
git checkout master
