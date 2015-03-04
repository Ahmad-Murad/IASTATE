package cpre419.lab05;

import java.io.IOException;

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

public class Lab5Exp2 extends Configured implements Tool {

    static final String temp0 = "/scr/aguibert/temp0";
    static final String temp1 = "/scr/aguibert/temp1";
    static String input = "/class/s15419x/lab5/oscars.json";
    static String output = "/scr/aguibert/lab5/exp2";

    public static void main(String[] args) throws Exception
    {
        Configuration config = new Configuration();
        int res = ToolRunner.run(config, new Lab5Exp2(), args);
        System.exit(res);
    }

    @SuppressWarnings("deprecation")
    @Override
    public int run(String[] args) throws Exception {

        FileSystem.get(super.getConf()).delete(new Path(output), true);

        try {
            // Job 1
            Job job_one = new Job(super.getConf(), "Driver Program Round 1");
            job_one.setJarByClass(Lab5Exp2.class);
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
                return; // sometimes get empty keys

            MinimalTweet t = new Gson().fromJson(value.toString(), MinimalTweet.class);
            context.write(new Text(t.user.screen_name), new IntWritable(t.user.followers_count));
        }
    }

    public static class Reduce_One extends Reducer<Text, IntWritable, Text, IntWritable> {

        @Override
        public void reduce(Text key, Iterable<IntWritable> values, Context context)
                        throws IOException, InterruptedException {
            int maxFollowers = 0;
            for (IntWritable val : values)
                if (val.get() > maxFollowers)
                    maxFollowers = val.get();
            context.write(key, new IntWritable(maxFollowers));
        }
    }
}
