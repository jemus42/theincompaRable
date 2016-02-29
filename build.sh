#! /bin/zsh

PATH=$PATH:/Users/Lukas/repos/jemus42/syncbin/bin/
PROJECT_HOME='/Users/Lukas/repos/incomparable/'

cd analyses

for file in $(ls *Rmd); do
  render $file;
done

echo "Pushing…"

rsync -avz *.html *_files -e ssh strato:"/srv/stats.jemu.name/theincomparable/"

echo "Done!"
