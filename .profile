#!/bin/sh

export ANDROID_HOME="/store0/android-sdk"
export ANDROID_NDK_HOME="${ANDROID_HOME}/ndk"
export GRADLE_HOME="${HOME}/.local/lib/gradle"
export GROOVY_HOME="${HOME}/.local/lib/groovy"
export GRAILS_HOME="${HOME}/.local/lib/grails"
export MAVEN_HOME="${HOME}/.local/lib/maven"
export RUST_SRC_PATH="/usr/lib/rustlib/src/rust"
export GOPATH="${HOME}/gocode"
export NPM_PACKAGES="${HOME}/.local"

export PATH="${HOME}/bin:${HOME}/.local/bin:/usr/sbin:/usr/games/bin:${ANDROID_HOME}/cmdline-tools/latest/bin:${ANDROID_HOME}/emulator:${ANDROID_HOME}/platform-tools:$ANDROID_NDK_HOME/21.0.6113669:${HOME}/share/netbeans/bin:$GRADLE_HOME/bin:$GROOVY_HOME/bin:$SCALA_HOME/bin:$MAVEN_HOME/bin:$GRAILS_HOME/bin:${HOME}/share/eclipse:${HOME}/.cask/bin:/usr/local/games:${HOME}/.cargo/bin:${HOME}/.cabal/bin:${PATH}:$GOROOT/bin:$GOPATH/bin:${HOME}/.luarocks/bin:${HOME}/.gem/ruby/2.5.0/bin:${HOME}/idea/bin:${HOME}/dev/kotlin-language-server/server/build/install/server/bin"

export PROJECTS_HOME="${HOME}/dev"
export SDL_AUDIODRIVER="pulse"
# export SDL_AUDIODRIVER="alsa"
# export SDL_VIDEODRIVER="x11"
export SDL_SOUNDFONTS=/usr/share/sounds/sf2/FluidR3_GM.sf2:/usr/share/sounds/sf2/FluidR3_GS.sf2:/usr/share/sounds/sf2/TimGM6mb.sf2
export DOOMWADDIR="${HOME}/gms/doom"
export LIBVIRT_DEFAULT_URI=qemu:///system
# export QEMU_AUDIO_DRV=alsa
export QEMU_AUDIO_DRV=pa
# export QEMU_AUDIO_DRV=spice
# export QEMU_AUDIO_DRV="sdl"
export JAVA_HOME=/usr/lib/jvm/default-java
export GPGKEY=8A3D849A
export NAME="Martin Kjær Jørgensen"
export EMAIL="mkj@gotu.dk"
export MPD_HOST="${HOME}/.mpd/socket"
# export MOZ_USE_OMTC=1
# export MC_SKIN="${HOME}/.mc/solarized.ini"

export TEST_DATA_PATH=/ramfs
export PYTHON_VIRTUALENV_DIR="${HOME}/.virtualenvs"
export WINEARCH=win32
export WINEPREFIX="${HOME}/.wine32"

# export XAUTHORITY=/home/mkj/.Xauthority
# export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

export VISUAL="${HOME}/bin/emacs"
export EDITOR="${HOME}/bin/emacs"
