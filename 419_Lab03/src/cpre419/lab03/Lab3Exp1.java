package cpre419.lab03;

import java.io.IOException;
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

    public static void main(String[] args) throws Exception {
        int res = ToolRunner.run(new Configuration(), new Lab3Exp1(), args);
        System.exit(res);
    }

    @SuppressWarnings("deprecation")
    @Override
    public int run(String[] args) throws Exception {

        String input = "/class/s15419x/lab3/patents.txt";
        String temp = "/scr/aguibert/lab3/temp";
        String output = "/scr/aguibert/lab3/exp1/output";

        Configuration conf = new Configuration();

        if (FileSystem.get(conf).delete(new Path(output), true))
            System.out.println("Old output files have been deleted.");

        Job job_one = new Job(conf, "Driver Program Round One");
        job_one.setJarByClass(Lab3Exp1.class);
        job_one.setNumReduceTasks(2);
        job_one.setOutputKeyClass(Text.class);
        job_one.setOutputValueClass(IntWritable.class);
        job_one.setMapperClass(Map_One.class);
        job_one.setReducerClass(Reduce_One.class);
        job_one.setInputFormatClass(TextInputFormat.class);
        job_one.setOutputFormatClass(TextOutputFormat.class);

        FileInputFormat.addInputPath(job_one, new Path(input));
        FileOutputFormat.setOutputPath(job_one, new Path(output));

        job_one.waitForCompletion(true);

        if (FileSystem.get(conf).delete(new Path(temp), true))
            System.out.println("Temp files have been deleted.");

        return 0;
    }

    // The input to the map method would be a LongWritable (long) key and Text (String) value
    // Notice the class declaration is done with LongWritable key and Text value
    // The TextInputFormat splits the data line by line.
    // The key for TextInputFormat is nothing but the line number and hence can be ignored
    // The value for the TextInputFormat is a line of text from the input
    // The map method can emit data using context.write() method
    // However, to match the class declaration, it must emit Text as key and IntWribale as value
    public static class Map_One extends Mapper<LongWritable, Text, Text, IntWritable> {

        private IntWritable one = new IntWritable(1);

        @Override
        public void map(LongWritable key, Text value, Context context)
                        throws IOException, InterruptedException {

            StringTokenizer tokens = new StringTokenizer(value.toString());

            tokens.nextToken();
            context.write(new Text(tokens.nextToken()), one);
        }
    }

    public static class Reduce_One extends Reducer<Text, IntWritable, Text, IntWritable> {

        @Override
        public void reduce(Text key, Iterable<IntWritable> values, Context context)
                        throws IOException, InterruptedException {

            //context.progress(); // update job so it doesn't die

            int sum = 0;
            for (IntWritable val : values) {
                sum += val.get();
            }
            context.write(key, new IntWritable(sum));
        }
    }
}
