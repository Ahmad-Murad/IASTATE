package cpre419.lab05;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

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
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;

import com.google.gson.Gson;

import cpre419.lab05.MinimalTweet.HashTag;

public class Lab5Exp1 extends Configured implements Tool {

    static final String temp0 = "/scr/aguibert/temp0";
    static final String temp1 = "/scr/aguibert/temp1";
    static String input = "/class/s15419x/lab5/oscars.json";
    static String output = "/scr/aguibert/lab5/exp1";

    public static void main(String[] args) throws Exception
    {
        Configuration config = new Configuration();
        int res = ToolRunner.run(config, new Lab5Exp1(), args);
        System.exit(res);
    }

    @SuppressWarnings("deprecation")
    @Override
    public int run(String[] args) throws Exception {

        FileSystem.get(super.getConf()).delete(new Path(output), true);

        try {
            // Job 1
            Job job_one = new Job(super.getConf(), "Driver Program Round 1");
            job_one.setJarByClass(Lab5Exp1.class);
            job_one.setNumReduceTasks(1);
            job_one.setOutputKeyClass(Text.class);
            job_one.setOutputValueClass(IntWritable.class);
            job_one.setMapperClass(Map_One.class);
            job_one.setReducerClass(Reduce_One.class);
            job_one.setInputFormatClass(CustomInputFormat.class);
            job_one.setOutputFormatClass(TextOutputFormat.class);

            FileInputFormat.addInputPath(job_one, new Path(input));
            FileOutputFormat.setOutputPath(job_one, new Path(output));

            job_one.waitForCompletion(true);
        } finally {
            FileSystem.get(super.getConf()).delete(new Path(temp0), true);
            FileSystem.get(super.getConf()).delete(new Path(temp1), true);
        }

        return 0;
    }

    public static class Map_One extends Mapper<LongWritable, Text, Text, IntWritable> {

        @Override
        public void map(LongWritable key, Text value, Context context)
                        throws IOException, InterruptedException {
            if (value.toString().trim().length() == 0)
                return;

            MinimalTweet t = new Gson().fromJson(value.toString(), MinimalTweet.class);
            if (t.entities != null && t.entities.hashtags != null) {
                // use a set so that hashtags may not be duplicates
                Set<String> hashTags = new HashSet<>();
                for (HashTag ht : t.entities.hashtags)
                    if (hashTags.add(ht.toString()))
                        context.write(new Text(ht.toString()), new IntWritable(1));
            }
        }
    }

    public static class Reduce_One extends Reducer<Text, IntWritable, Text, Text> {

        @Override
        public void reduce(Text key, Iterable<IntWritable> values, Context context)
                        throws IOException, InterruptedException {
            // key is the hashtag, value is just a 1
            int sum = 0;
            for (IntWritable val : values)
                sum += val.get();

            // print out key=hashtag and value=num occurrances of the hashtag
            context.write(new Text("" + sum), key);
        }
    }
}
