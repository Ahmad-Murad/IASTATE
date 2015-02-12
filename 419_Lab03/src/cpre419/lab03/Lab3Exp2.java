package cpre419.lab03;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
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

public class Lab3Exp2 extends Configured implements Tool {

    static final String input = "/home/aguibert/sample.txt";
    static final String temp0 = "/scr/aguibert/temp0";
    static final String temp1 = "/scr/aguibert/temp1";
    static final String output = "/scr/aguibert/lab3/exp2";

    public static void main(String[] args) throws Exception {
        int res = ToolRunner.run(new Configuration(), new Lab3Exp2(), args);
        System.exit(res);
    }

    @SuppressWarnings("deprecation")
    @Override
    public int run(String[] args) throws Exception {

        Configuration conf = new Configuration();

        FileSystem.get(conf).delete(new Path(output), true);

        try {
            // Job 1
            Job job_one = new Job(conf, "Driver Program Round 1");
            job_one.setJarByClass(Lab3Exp2.class);
            job_one.setNumReduceTasks(4);
            job_one.setOutputKeyClass(IntWritable.class);
            job_one.setOutputValueClass(IntWritable.class);
            job_one.setMapperClass(Map_One.class);
            job_one.setReducerClass(Reduce_One.class);
            job_one.setInputFormatClass(TextInputFormat.class);
            job_one.setOutputFormatClass(TextOutputFormat.class);

            FileInputFormat.addInputPath(job_one, new Path(input));
            FileOutputFormat.setOutputPath(job_one, new Path(output));

            job_one.waitForCompletion(true);

//            // Job 2
//            Job job_two = new Job(conf, "Driver Program Round 2");
//            job_two.setJarByClass(Lab3Exp2.class);
//            job_two.setNumReduceTasks(4);
//            job_two.setOutputKeyClass(Text.class);
//            job_two.setOutputValueClass(Text.class);
//            job_two.setMapperClass(Map_Two.class);
//            job_two.setReducerClass(Reduce_Two.class);
//            job_two.setInputFormatClass(TextInputFormat.class);
//            job_two.setOutputFormatClass(TextOutputFormat.class);
//
//            FileInputFormat.addInputPath(job_two, new Path(temp0));
//            FileOutputFormat.setOutputPath(job_two, new Path(output));
//
//            job_two.waitForCompletion(true);

        } finally {
            FileSystem.get(conf).delete(new Path(temp0), true);
        }

        return 0;
    }

    public static class Map_One extends Mapper<LongWritable, Text, IntWritable, IntWritable> {

        @Override
        public void map(LongWritable key, Text value, Context context)
                        throws IOException, InterruptedException {

            // Convert to an undirected graph by repeating each edge backwards
            StringTokenizer line = new StringTokenizer(value.toString());
            IntWritable i0 = new IntWritable(Integer.parseInt(line.nextToken()));
            IntWritable i1 = new IntWritable(Integer.parseInt(line.nextToken()));
            context.write(i0, i1);
            context.write(i1, i0);
        }
    }

    public static class Reduce_One extends Reducer<IntWritable, IntWritable, IntWritable, Text> {

        @Override
        public void reduce(IntWritable key, Iterable<IntWritable> values, Context context)
                        throws IOException, InterruptedException {

            // Build a set of neighboring edges
            List<IntWritable> l = new ArrayList<>();
            for (IntWritable t : values)
                l.add(t);
            IntWritable[] arr = (IntWritable[]) l.toArray();

            // Print all possible close triplets
            for (int i = 0; i < arr.length - 1; i++)
                for (int j = i + 1; j < arr.length; j++) {
                    context.write(key, new Text(arr[i].toString() + ' ' + arr[j].toString()));
                    System.out.println(key.toString() + new Text(arr[i].toString() + ' ' + arr[j].toString()));
                }
        }
    }

//    public static class Map_Two extends Mapper<LongWritable, Text, Text, Text> {
//
//        @Override
//        public void map(LongWritable key, Text value, Context context)
//                        throws IOException, InterruptedException {
//
//            StringTokenizer line = new StringTokenizer(value.toString());
//            Text t0 = new Text(line.nextToken());
//            Text t1 = new Text(line.nextToken());
//            context.write(t0, t1);
//        }
//    }
//
//    public static class Reduce_Two extends Reducer<Text, Text, Text, IntWritable> {
//
//        @Override
//        public void reduce(Text key, Iterable<Text> values, Context context)
//                        throws IOException, InterruptedException {
//
//            Set<Text> deps = new HashSet<Text>();
//            for (Text val : values)
//                deps.add(val);
//
//            int significance = deps.size();
//            if (topTen[0].significance < significance) {
//                topTen[0] = new PatentCount((Integer.parseInt(key.toString())), significance);
//                context.write(key, new IntWritable(significance));
//                Arrays.sort(topTen);
//                System.out.println("Sorted array:  " + topTen);
//            }
//            //context.write(key, new IntWritable(significance));
//        }
//    }
}
