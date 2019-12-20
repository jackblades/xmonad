# XMonad conf

- main key changed to "windows key" from "alt"

- https://linuxaria.com/pills/how-to-have-a-transparent-terminal-as-wallpaper-that-displays-information
    - xrootconsole can be used to tails a file in a window on your X11 root window
    - eterm

# Flatpak config
- post-activation
    - flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
    - flatpak update
    - flatpak search bustle
    - flatpak install flathub org.freedesktop.Bustle
    - flatpak run org.freedesktop.Bustle
    - flatpak run --filesystem=/tmp/mounts/office org.phoenicis.playonlinux
    - flatpak uninstall org.phoenicis.playonlinux
    - flatpak uninstall --unused








