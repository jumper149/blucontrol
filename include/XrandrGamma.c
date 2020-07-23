#include "XrandrGamma.h"

void setGamma (float red, float green, float blue, XRRScreenResources *res, Display *dpy) {
    for (int c = 0; c < res->ncrtc; c++) {
        RRCrtc crtc = res->crtcs[c];
        int size = XRRGetCrtcGammaSize(dpy, crtc);
        XRRCrtcGamma *crtcGamma = XRRAllocGamma(size);

        for (int i = 0; i < size; i++) {
            double g = 65535.0 * i / size;
            crtcGamma->red[i]   = g * red;
            crtcGamma->green[i] = g * green;
            crtcGamma->blue[i]  = g * blue;
        }

        XRRSetCrtcGamma(dpy, crtc, crtcGamma);
        XRRFreeGamma(crtcGamma);
    }
}
