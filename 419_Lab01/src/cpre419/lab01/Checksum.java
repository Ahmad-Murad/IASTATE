package cpre419.lab01;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;

public class Checksum
{
    public static void main(String args[]) throws Exception {
        // The system configuration
        Configuration conf = new Configuration();

        FileSystem fs = FileSystem.get(conf);
        Path path = new Path("/class/s15419x/lab1/bigdata");
        FSDataInputStream inp = fs.open(path);
    }
}
