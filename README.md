# hid

This is a very much alpha quality library I developed originally for playing around with the Nintendo Wiimote.

# Dependencies

* [HIDApi](https://github.com/signal11/hidapi)
* [win32](https://github.com/Zulu-Inuoe/win32)

This library is unfortunately not platform agnostic. HIDAPI does not provide enough functionality to accomplish everything (circa 2013) I needed to make use of the wiimote, so I use win32 to accomplish the rest.