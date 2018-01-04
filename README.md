mnotify
=======

A minimalistic notification daemon.  
Written in Haskell and dependent only on X11 and D-Bus.  
Receives notifications by partially implementing the **org.freedesktop.Notifications** D-Bus service specification.

Best used with a window manager like xmonad:  
![alt text](https://user-images.githubusercontent.com/761605/34437613-8451a556-eca8-11e7-8d58-094965398df3.png "mNotify screenshot")

## Dependencies:
* [X11 Bindings Package](https://hackage.haskell.org/package/X11)
* [D-Bus Library](https://hackage.haskell.org/package/dbus)

## Todo:
* Make it a Cabal package
* Add command help
* Allow configuration with cmd flags
* Implement more of the freedesktop service specification
