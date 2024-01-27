module nbh.ppm;
import std.stdio : File;

class PPM {
    File file;

    this(string name, uint width, uint height)
    @safe
    {
        this.file = File(name, "w");
        this.file.write("P6\n", width, " ", height, "\n255\n");
    }

    this(string name)
    @safe
    {
        import nbh.setup : xsize, ysize;
        this(name, xsize, ysize);
    }

    void close()
    @safe
    {
        this.file.close();
    }
};
