package cpre419.lab05;

import java.io.IOException;
import java.util.HashMap;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
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

public class Lab5Exp3 extends Configured implements Tool {

    static final String temp0 = "/scr/aguibert/temp0";
    static final String temp1 = "/scr/aguibert/temp1";
    static String input = "/class/s15419x/lab5/usa.json";
    static String output = "/scr/aguibert/lab5/exp3";

    public static void main(String[] args) throws Exception
    {
        Configuration config = new Configuration();
        int res = ToolRunner.run(config, new Lab5Exp3(), args);
        System.exit(res);
    }

    @SuppressWarnings("deprecation")
    @Override
    public int run(String[] args) throws Exception {

        // must use super.getCon() in order to see the libs
        FileSystem.get(super.getConf()).delete(new Path(output), true);

        try {
            // Job 1
            Job job_one = new Job(super.getConf(), "Driver Program Round 1");
            job_one.setJarByClass(Lab5Exp3.class);
            job_one.setNumReduceTasks(1);
            job_one.setOutputKeyClass(Text.class);
            job_one.setOutputValueClass(Text.class);
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

    public static class Map_One extends Mapper<LongWritable, Text, Text, Text> {

        @Override
        public void map(LongWritable key, Text value, Context context)
                        throws IOException, InterruptedException {
            if (value.toString().trim().length() == 0)
                return; // sometimes get empty strings as keys

            MinimalTweet t = new Gson().fromJson(value.toString(), MinimalTweet.class);
            StringBuilder hts = new StringBuilder();
            // append each hash tag to a buffer, separated by a single space
            for (HashTag ht : t.entities.hashtags)
                hts.append(' ').append(ht.text);

            // write to context in format:  <screen_name>       <status_count>\t<hashtags>
            context.write(new Text(t.user.screen_name), new Text("" + t.user.statuses_count + "\t" + hts.toString()));
        }
    }

    public static class Reduce_One extends Reducer<Text, Text, Text, Text> {

        @Override
        public void reduce(Text key, Iterable<Text> values, Context context)
                        throws IOException, InterruptedException {

            int max_prolific = 0;
            HashMap<String, Integer> hashtagToCount = new HashMap<>();

            for (Text val : values) {
                String value = val.toString();
                // store the maximum prolific value of all occurrences of this user
                int prolific = Integer.parseInt(value.substring(0, value.indexOf('\t')));
                if (prolific > max_prolific)
                    max_prolific = prolific;

                // for each hashtag in the hashtag, count how many times it has occurred
                for (String hashtag : value.substring(value.indexOf('\t')).split(" "))
                    if (hashtag.trim().length() == 0)
                        continue; // sometimes we get empty hashtags... \o/
                    else if (!hashtagToCount.containsKey(hashtag))
                        hashtagToCount.put(hashtag, 1);
                    else
                        hashtagToCount.put(hashtag, hashtagToCount.get(hashtag) + 1);
            }

            // figure out which hash tag is the most common, and what the count of is
            String maxHTstring = "";
            int maxHTcount = 0;
            for (String tag : hashtagToCount.keySet())
                if (hashtagToCount.get(tag) > maxHTcount) {
                    maxHTcount = hashtagToCount.get(tag);
                    maxHTstring = tag;
                }

            context.write(key, new Text("" + max_prolific + "\thashtag " + maxHTstring + " was tweeted " + maxHTcount + " times."));
        }
    }
}
