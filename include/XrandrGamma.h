#include <X11/Xlib.h>
#include <X11/extensions/Xrandr.h>

void setGamma (float red, float green, float blue, XRRScreenResources *res, Display *dpy);
