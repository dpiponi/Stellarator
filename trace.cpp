#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <algorithm>

using namespace std;

// https://stackoverflow.com/questions/2654480/writing-bmp-image-in-pure-c-c-without-other-libraries
void savebmp(const char *filename, int height, int width,
             const vector<int> &data, const vector<int> &activity, int vscale) {
    ofstream outputfile(filename, ios::out | ios::binary);

    int padding = (4-width*3) & 0x03;
    int paddedsize = height*(3*width+padding);

    int headers[13] = {
        paddedsize+54,     // bfSize (whole file size)
        0,                 // bfReserved (both)
        54,                // bfOffbits
        40,                // biSize
        width, height,     // biWidth, biHeight
        0,
        0,                 // biCompression
        paddedsize,        // biSizeImage
        0, 0,              // biXPelsPerMeter, biYPelsPerMeter
        0,                 // biClrUsed
        0                  // biClrImportant
    };

    //
    // Headers
    //
    outputfile << "BM";

    for (int n = 0; n < 6; n++) {
       outputfile.put(headers[n] & 0x000000FFU);
       outputfile.put((headers[n] & 0x0000FF00U) >> 8);
       outputfile.put((headers[n] & 0x00FF0000U) >> 16);
       outputfile.put((headers[n] & 0xFF000000U) >> 24);
    }

    //
    // biPlanes and biBitCount fields.
    //
    outputfile.put(1);
    outputfile.put(0);
    outputfile.put(24);
    outputfile.put(0);

    for (int n = 7; n < 13; n++) {
       outputfile.put(headers[n] & 0x000000FFU);
       outputfile.put((headers[n] & 0x0000FF00U) >> 8);
       outputfile.put((headers[n] & 0x00FF0000U) >> 16);
       outputfile.put((headers[n] & 0xFF000000U) >> 24);
    }

    //
    // Data
    //
    for (int y = height-1; y >= 0; --y) {     // BMP image format is written from bottom to top...
        for (int x = 0; x < width; ++x) {
            int bit = data[width*y+x]*255/vscale;

            //
            // Logarithmic scale
            //
            float business = log(float(1+activity[y*width+x]));
            if (business > 1.0f) {
                business = 1.0f;
            }

            int red = business*bit;
            int green = bit;
            int blue = (1.0f-business)*bit;

            outputfile.put(blue);
            outputfile.put(green);
            outputfile.put(red);
        }
        for (int n = 0; n < padding; ++n) {
            outputfile.put(0);
        }
    }
}

//
// Trace file consists simply of pairs of bytes:
//      address, byte written
//      address, byte written
//              ...
//
int main() {
    std::ifstream file("trace.record", std::ios::binary | std::ios::ate);
    std::streamsize size = file.tellg();
    file.seekg(0, std::ios::beg);

    std::vector<char> trace(size);
    std::vector<bool> ram(1024, 0);

    if (file.read(trace.data(), size)) {
        const int vscale = 512;
        int vsize = (size/2+vscale-1)/vscale;

        // Count of how many times each bit is one per time period
        std::vector<int> data(1024*vsize, 0);

        // Count of number of times bit is written per time period
        std::vector<int> activity(1024*vsize, 0);

        for (int i = 0; i < size/2; ++i) {
            int index = i/vscale;
            int row = 1024*index;

            //
            // Update RAM
            //
            transform(ram.begin(),
                      ram.end(),
                      data.begin()+row,
                      data.begin()+row,
                      [](int a, int b) -> int { return a+b; });
            
            int address = trace[2*i];
            int value = trace[2*i+1];

            for (int bit = 0; bit < 8; ++bit) {
                //
                // The direction is somewhat arbitrary as bitmaps
                // can run either way on VCS.
                // But you could argue that the default is high bit first
                // https://alienbill.com/2600/101/docs/stella.html#GRP
                //
                int offset = 8*address+bit;
                ram[offset] = value & (1 << (7-bit)) ? 1 : 0;
                ++activity[row+offset];
            }
        }
        savebmp("trace.bmp", vsize, 1024, data, activity, vscale);
    }
}
