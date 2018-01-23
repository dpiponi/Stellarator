#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>

using namespace std;

// https://stackoverflow.com/questions/2654480/writing-bmp-image-in-pure-c-c-without-other-libraries
void drawbmp (char * filename, int HEIGHT, int WIDTH, int *data, int *activity, int N) {

    unsigned int headers[13];
    FILE * outfile;
    int extrabytes;
    int paddedsize;
    int x; int y; int n;
    int red, green, blue;

    extrabytes = 4 - ((WIDTH * 3) % 4);                 // How many bytes of padding to add to each
                                                        // horizontal line - the size of which must
                                                        // be a multiple of 4 bytes.
    if (extrabytes == 4)
       extrabytes = 0;

    paddedsize = ((WIDTH * 3) + extrabytes) * HEIGHT;

    // Headers...
    // Note that the "BM" identifier in bytes 0 and 1 is NOT included in these "headers".

    headers[0]  = paddedsize + 54;      // bfSize (whole file size)
    headers[1]  = 0;                    // bfReserved (both)
    headers[2]  = 54;                   // bfOffbits
    headers[3]  = 40;                   // biSize
    headers[4]  = WIDTH;  // biWidth
    headers[5]  = HEIGHT; // biHeight

    // Would have biPlanes and biBitCount in position 6, but they're shorts.
    // It's easier to write them out separately (see below) than pretend
    // they're a single int, especially with endian issues...

    headers[7]  = 0;                    // biCompression
    headers[8]  = paddedsize;           // biSizeImage
    headers[9]  = 0;                    // biXPelsPerMeter
    headers[10] = 0;                    // biYPelsPerMeter
    headers[11] = 0;                    // biClrUsed
    headers[12] = 0;                    // biClrImportant

    outfile = fopen(filename, "wb");

    //
    // Headers begin...
    // When printing ints and shorts, we write out 1 character at a time to avoid endian issues.
    //

    fprintf(outfile, "BM");

    for (n = 0; n <= 5; n++)
    {
       fprintf(outfile, "%c", headers[n] & 0x000000FF);
       fprintf(outfile, "%c", (headers[n] & 0x0000FF00) >> 8);
       fprintf(outfile, "%c", (headers[n] & 0x00FF0000) >> 16);
       fprintf(outfile, "%c", (headers[n] & (unsigned int) 0xFF000000) >> 24);
    }

    // These next 4 characters are for the biPlanes and biBitCount fields.

    fprintf(outfile, "%c", 1);
    fprintf(outfile, "%c", 0);
    fprintf(outfile, "%c", 24);
    fprintf(outfile, "%c", 0);

    for (n = 7; n <= 12; n++)
    {
       fprintf(outfile, "%c", headers[n] & 0x000000FF);
       fprintf(outfile, "%c", (headers[n] & 0x0000FF00) >> 8);
       fprintf(outfile, "%c", (headers[n] & 0x00FF0000) >> 16);
       fprintf(outfile, "%c", (headers[n] & (unsigned int) 0xFF000000) >> 24);
    }

    //
    // Headers done, now write the data...
    //

    for (y = HEIGHT - 1; y >= 0; y--)     // BMP image format is written from bottom to top...
    {
       for (x = 0; x <= WIDTH - 1; x++)
       {

#if 0
          red = reduce(redcount[x][y] + COLOUR_OFFSET) * red_multiplier;
          green = reduce(greencount[x][y] + COLOUR_OFFSET) * green_multiplier;
          blue = reduce(bluecount[x][y] + COLOUR_OFFSET) * blue_multiplier;

          if (red > 255) red = 255; if (red < 0) red = 0;
          if (green > 255) green = 255; if (green < 0) green = 0;
          if (blue > 255) blue = 255; if (blue < 0) blue = 0;
#endif
          int bit = data[y*WIDTH+x]*255/N;
          float business = log(float(1+activity[y*WIDTH+x]));
          if (business > 1.0f) {
              business = 1.0f;
          }

          red = business*bit;
          green = bit;//data[y*WIDTH+x]*255/N;
          blue = (1.0f-business)*bit;


          // Also, it's written in (b,g,r) format...

          fprintf(outfile, "%c", blue);
          fprintf(outfile, "%c", green);
          fprintf(outfile, "%c", red);
       }
       if (extrabytes)      // See above - BMP lines must be of lengths divisible by 4.
       {
          for (n = 1; n <= extrabytes; n++)
          {
             fprintf(outfile, "%c", 0);
          }
       }
    }

    fclose(outfile);
    return;
}

int main() {
    std::ifstream file("trace.record", std::ios::binary | std::ios::ate);
    std::streamsize size = file.tellg();
    file.seekg(0, std::ios::beg);

    std::vector<char> buffer(size);
    std::vector<char> ram(1024);
    for (int i = 0; i < 1024; ++i) {
        ram[i] = 0;
    }
    if (file.read(buffer.data(), size)) {
        const int N = 512;
        int vsize = (size/2+N-1)/N;
        int *data = new int[1024*vsize];
        int *activity = new int[1024*vsize];
        for (int j = 0; j < 1024; ++j) {
            data[j] = 0;
            activity[j] = 0;
        }
        for (int i = 0; i < size/2; ++i) {
            int index = i/N;
            for (int j = 0; j < 1024; ++j) {
                data[1024*index+j] += ram[j];
            }
            
            // Update RAM
            int address = buffer[2*i];
            int value = buffer[2*i+1];
            for (int k = 0; k < 8; ++k) {
                //ram[8*address+k] = !!(value & (1 << k));
                // The direction is somewhat arbitrary as bitmaps
                // can run either way on VCS.
                // You could argue that the default is high bit first
                // https://alienbill.com/2600/101/docs/stella.html#GRP
                ram[8*address+k] = !!(value & (1 << (7-k)));
                ++activity[1024*index+8*address+k];
            }
        }
        cout << vsize << endl;
        drawbmp("trace.bmp", vsize, 1024, data, activity, N);
    }
}
