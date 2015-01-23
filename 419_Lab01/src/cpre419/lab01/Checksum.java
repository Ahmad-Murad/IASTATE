package cpre419.lab01;

import java.io.*;
import java.lang.*;
import java.util.*;
import java.net.*;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.util.*;


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
