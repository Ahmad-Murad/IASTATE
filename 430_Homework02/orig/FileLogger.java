package hw2;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Date;

public class FileLogger
{
    private String filename;

    public FileLogger(String filename)
    {
        this.filename = filename;
    }

    public void log(String msg)
    {
        Date d = new Date();
        try
        {
            // argument 'true' means append to existing file
            OutputStream os = new FileOutputStream(filename, true);
            PrintWriter pw = new PrintWriter(os);
            pw.println(d + " " + msg);
            pw.close();
        } catch (FileNotFoundException e)
        {
            System.err.println("Unable to open log file: " + filename);
        }
    }

}
