package cpre419.lab03;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.StringTokenizer;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;

public class Lab3Exp1 extends Configured implements Tool {

    static final String input = "/class/s15419x/lab3/patents.txt";
    static final String temp0 = "/scr/aguibert/lab3/temp";
    static final String temp1 = "/scr/aguibert/lab3/temp1";
    static final String output = "/scr/aguibert/lab3/exp1/output";

    public static void main(String[] args) throws Exception {
        int res = ToolRunner.run(new Configuration(), new Lab3Exp1(), args);
        System.exit(res);
    }

    @SuppressWarnings("deprecation")
    @Override
    public int run(String[] args) throws Exception {

        Configuration conf = new Configuration();

        if (FileSystem.get(conf).delete(new Path(output), true))
            System.out.println("Old output files have been deleted.");

        try {
            // Job 1
            Job job_one = new Job(conf, "Driver Program Round 1");
            job_one.setJarByClass(Lab3Exp1.class);
            job_one.setNumReduceTasks(4);
            job_one.setOutputKeyClass(Text.class);
            job_one.setOutputValueClass(Text.class);
            job_one.setMapperClass(Map_One.class);
            job_one.setReducerClass(Reduce_One.class);
            job_one.setInputFormatClass(TextInputFormat.class);
            job_one.setOutputFormatClass(TextOutputFormat.class);

            FileInputFormat.addInputPath(job_one, new Path(input));
            FileOutputFormat.setOutputPath(job_one, new Path(temp0));

            job_one.waitForCompletion(true);

            // Job 2
            Job job_two = new Job(conf, "Driver Program Round 2");
            job_two.setJarByClass(Lab3Exp1.class);
            job_two.setNumReduceTasks(4);
            job_two.setOutputKeyClass(Text.class);
            job_two.setOutputValueClass(Text.class);
            job_two.setMapperClass(Map_Two.class);
            job_two.setReducerClass(Reduce_Two.class);
            job_two.setInputFormatClass(TextInputFormat.class);
            job_two.setOutputFormatClass(TextOutputFormat.class);

            FileInputFormat.addInputPath(job_two, new Path(temp0));
            FileOutputFormat.setOutputPath(job_two, new Path(output));

            job_two.waitForCompletion(true);

//            // Job 3
//            Job job_three = new Job(conf, "Driver Program Round 3");
//            job_three.setJarByClass(Lab3Exp1.class);
//            job_three.setNumReduceTasks(1);
//            job_three.setOutputKeyClass(Text.class);
//            job_three.setOutputValueClass(Text.class);
//            job_three.setMapperClass(Map_Three.class);
//            job_three.setReducerClass(Reduce_Three.class);
//            job_three.setInputFormatClass(TextInputFormat.class);
//            job_three.setOutputFormatClass(TextOutputFormat.class);
//
//            FileInputFormat.addInputPath(job_three, new Path(temp0));
//            FileOutputFormat.setOutputPath(job_three, new Path(output));
//
//            job_three.waitForCompletion(true);

        } finally {
            if (FileSystem.get(conf).delete(new Path(temp0), true))
                System.out.println("Temp files have been deleted.");
            if (FileSystem.get(conf).delete(new Path(temp1), true))
                System.out.println("Temp files have been deleted.");
        }

        return 0;
    }

    // The input to the map method would be a LongWritable (long) key and Text (String) value
    // Notice the class declaration is done with LongWritable key and Text value
    // The TextInputFormat splits the data line by line.
    // The key for TextInputFormat is nothing but the line number and hence can be ignored
    // The value for the TextInputFormat is a line of text from the input
    // The map method can emit data using context.write() method
    // However, to match the class declaration, it must emit Text as key and IntWribale as value
    public static class Map_One extends Mapper<LongWritable, Text, Text, Text> {

        @Override
        public void map(LongWritable key, Text value, Context context)
                        throws IOException, InterruptedException {

            StringTokenizer line = new StringTokenizer(value.toString());
            Text t0 = new Text(line.nextToken());
            Text t1 = new Text(line.nextToken());
            Text tFlipped = new Text("!" + t1.toString());
            context.write(t1, t0);
            context.write(t0, tFlipped);
        }
    }

    public static class Reduce_One extends Reducer<Text, Text, Text, Text> {

        @Override
        public void reduce(Text key, Iterable<Text> values, Context context)
                        throws IOException, InterruptedException {

            Set<Text> toFlip = new HashSet<Text>();
            for (Text val : values) {
                String v = val.toString();
                if (v.startsWith("!"))
                    toFlip.add(new Text(v.substring(1)));
                else
                    context.write(key, val);
            }

            for (Text t : toFlip) {
                context.write(t, key);
            }
        }
    }

    public static class Map_Two extends Mapper<LongWritable, Text, Text, Text> {

        @Override
        public void map(LongWritable key, Text value, Context context)
                        throws IOException, InterruptedException {

            StringTokenizer line = new StringTokenizer(value.toString());
            Text t0 = new Text(line.nextToken());
            Text t1 = new Text(line.nextToken());
            context.write(t0, t1);
        }
    }

    public static class Reduce_Two extends Reducer<Text, Text, IntWritable, Text> {

        @Override
        public void reduce(Text key, Iterable<Text> values, Context context)
                        throws IOException, InterruptedException {

            Set<Text> deps = new HashSet<Text>();
            for (Text val : values)
                deps.add(val);

            int significance = deps.size();
            context.write(new IntWritable(significance), key);
        }
    }

//    public static class Map_Three extends Mapper<LongWritable, Text, Text, Text> {
//
//        @Override
//        public void map(LongWritable key, Text value, Context context)
//                        throws IOException, InterruptedException {
//
//            StringTokenizer line = new StringTokenizer(value.toString());
//            Text t0 = new Text(line.nextToken());
//            Text t1 = new Text(line.nextToken());
//            context.write(t1, t0);
//        }
//    }
//
//    public static class Reduce_Three extends Reducer<Text, Text, Text, Text> {
//
//        @Override
//        public void reduce(Text key, Iterable<Text> values, Context context)
//                        throws IOException, InterruptedException {
//
//            for (Text val : values)
//                context.write(val, key);
//        }
//    }
}
