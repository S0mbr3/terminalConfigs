#script to install lfimg to have previews and others features inside the lf file manager




# download exiftool from https://exiftool.org/index.html
cd ~/builds
gzip -dc Image-ExifTool-12.44.tar.gz | tar -xf -
cd Image-ExifTool-1244

perl Makefile.perl
make test
sudo make install

sudo pacman -S 7z
sudo pacman -S gnome-epup-thumbnailer

sudo pacman -S libcdio
sudo pacman -S transmission-cli
sudo pacman -S gnumeric
sudo pacman -S odt2txt
sudo pacman -S docx2txt
sudo pacman -S catdoc
sudo pacman -S unrar
sudo pacman -S p7zip
sudo pacman -S unzip
sudo pacman -S chafa
sudo pacman -S wkhtmltopdf
sudo pacman -S poppler
sudo pacman -S imagemagick
sudo pacman -S ffmpegthumbnailer
yay -S mcomix
pip install ueberzug
touch ~/.config/lf/lfrc
