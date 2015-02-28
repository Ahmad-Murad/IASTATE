package cpre419.lab05;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.InputSplit;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.JobContext;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.RecordReader;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.TaskAttemptContext;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.FileSplit;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.LineReader;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;

import com.google.gson.Gson;

import cpre419.lab05.MinimalTweet.HashTag;

public class Lab5Exp1 extends Configured implements Tool {

    static final String temp0 = "/scr/aguibert/temp0";
    static final String temp1 = "/scr/aguibert/temp1";
    static String input = "/class/s15419x/lab5/oscars.json";
    static String output = "/scr/aguibert/lab5/output";

    public static void main(String[] args) throws Exception
    {
//        FileReader fr = new FileReader("orig/single.json");
//        MinimalTweet t = new Gson().fromJson(fr, MinimalTweet.class);
//        System.out.println(t.entities.hashtags);
//        fr.close();
        Configuration config = new Configuration();
        config.set("mapreduce.map.java.opts", "-Xmx1024m");
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

    public static class CustomInputFormat extends FileInputFormat<LongWritable, Text> {

        @Override
        public RecordReader<LongWritable, Text> createRecordReader(InputSplit split, TaskAttemptContext context)
                        throws IOException, InterruptedException {
            return new CustomRecordReader();
        }

        @Override
        protected boolean isSplitable(JobContext context, Path filename) {
            return false;
        }
    }

    public static class CustomRecordReader extends RecordReader<LongWritable, Text> {

        private long start;
        private long pos;
        private long end;
        private LineReader in;
        private LongWritable key = null;
        private Text value = null;

        @Override
        public void close() throws IOException {
            if (in != null) {
                in.close();
            }
        }

        @Override
        public LongWritable getCurrentKey() throws IOException, InterruptedException {
            return key;
        }

        @Override
        public Text getCurrentValue() throws IOException, InterruptedException {
            return value;
        }

        @Override
        public float getProgress() throws IOException, InterruptedException {
            if (start == end) {
                return 0.0f;
            } else {
                return Math.min(1.0f, (pos - start) / (float) (end - start));
            }
        }

        @Override
        public void initialize(InputSplit iSplit, TaskAttemptContext context) throws IOException, InterruptedException {
            FileSplit split = (FileSplit) iSplit;
            Configuration job = context.getConfiguration();
            start = split.getStart();
            end = start + split.getLength();
            final Path file = split.getPath();

            // open the file and seek to the start of the split
            FileSystem fs = file.getFileSystem(job);
            FSDataInputStream fileIn = fs.open(split.getPath());
            in = new LineReader(fileIn, job);
            end = Long.MAX_VALUE;
            this.pos = start;
        }

        @Override
        public boolean nextKeyValue() throws IOException, InterruptedException {
            if (key == null)
                key = new LongWritable();
            else
                key.set(pos);
            if (value == null)
                value = new Text();

            // look for the first brace
            int size = 0, braceStack = 0;
            StringBuilder sb = new StringBuilder();
            do {
                size = in.readLine(value);
                pos += size;
                if (value.toString().endsWith("{")) {
                    sb.append(value.toString());
                    braceStack++;
                    break;
                }
            } while (size > 0);

            // now look for the closing brace
            do {
                size = in.readLine(value);
                pos += size;
                String line = value.toString();

                if (line.endsWith("{"))
                    braceStack++;

                if (line.startsWith("}")) {
                    if (--braceStack == 0) {
                        sb.append("}"); // don't include the comma
                        break; // found the outer closing brace
                    }
                }
                sb.append(line);
            } while (size > 0);
            value = new Text(sb.toString());

            if (size == 0) {
                // end of file reached
                key = null;
                value = null;
                return false;
            } else {
                return true;
            }
        }
    }

    public static class Map_One extends Mapper<LongWritable, Text, Text, IntWritable> {

        @Override
        public void map(LongWritable key, Text value, Context context)
                        throws IOException, InterruptedException {
            MinimalTweet t = new Gson().fromJson(value.toString(), MinimalTweet.class);
            Set<String> hashTags = new HashSet<>();
            for (HashTag ht : t.entities.hashtags)
                if (hashTags.add(ht.toString()))
                    context.write(new Text(ht.toString()), new IntWritable(1));
            t = null;
            hashTags = null;
        }
    }

    public static class Reduce_One extends Reducer<Text, IntWritable, Text, Text> {

        @Override
        public void reduce(Text key, Iterable<IntWritable> values, Context context)
                        throws IOException, InterruptedException {
            int sum = 0;
            for (IntWritable val : values)
                sum += val.get();
            context.write(key, new Text("was tweeted " + sum + " times."));
        }
    }
}
