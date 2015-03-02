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
        private LongWritable key = new LongWritable();
        private Text value = new Text();
        private int maxLineLength;

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
            // This InputSplit is a FileInputSplit
            FileSplit split = (FileSplit) iSplit;

            // Retrieve configuration, and Max allowed
            // bytes for a single record
            Configuration job = context.getConfiguration();
            this.maxLineLength = job.getInt("mapred.linerecordreader.maxlength", Integer.MAX_VALUE);

            // Split "S" is responsible for all records
            // starting from "start" and "end" positions
            start = split.getStart();
            end = start + split.getLength();

            // Retrieve file containing Split "S"
            final Path file = split.getPath();
            FileSystem fs = file.getFileSystem(job);
            FSDataInputStream fileIn = fs.open(split.getPath());

            // If Split "S" starts at byte 0, first line will be processed
            // If Split "S" does not start at byte 0, first line has been already
            // processed by "S-1" and therefore needs to be silently ignored
            boolean skipFirstLine = false;
            if (start != 0) {
                skipFirstLine = true;
                // Set the file pointer at "start - 1" position.
                // This is to make sure we won't miss any line
                // It could happen if "start" is located on a EOL
                --start;
                fileIn.seek(start);
            }

            in = new LineReader(fileIn, job);

            // If first line needs to be skipped, read first line
            // and stores its content to a dummy Text
            if (skipFirstLine) {
                Text dummy = new Text();
                // Reset "start" to "start + line offset"
                start += in.readLine(dummy, 0, (int) Math.min(Integer.MAX_VALUE, end - start));
            }

            // Position is the actual start
            this.pos = start;
        }

        @Override
        public boolean nextKeyValue() throws IOException, InterruptedException {
            // Current offset is the key
            key.set(pos);

            int newSize = 0;

            // Make sure we get at least one record that starts in this Split
            while (pos < end) {
                // Read first line and store its content to "value"
                newSize = nextJson(value);

                if (newSize == 0)
                    break; // end of file

                pos += newSize;

                // Line is lower than Maximum record line size
                // break and return true (found key / value)
                if (newSize < maxLineLength)
                    break;
            }

            if (newSize == 0) {
                // We've reached end of Split
                key = null;
                value = null;
                return false;
            } else {
                // Tell Hadoop a new line has been found
                // key / value will be retrieved by
                // getCurrentKey getCurrentValue methods
                return true;
            }
        }

        private int nextJson(Text text) throws IOException {

            int maxBytesToConsume = Math.max((int) Math.min(Integer.MAX_VALUE, end - pos), maxLineLength);
            int offset = 0;
            text.clear();
            Text tmp = new Text();
            boolean jsonStarted = false;
            String line;

            for (int i = 0; i < maxBytesToConsume; i++) {
                int offsetTmp = in.readLine(tmp, maxLineLength, maxBytesToConsume);
                offset += offsetTmp;
                // Check to see if a new JSON object is started
                if (!jsonStarted) {
                    line = tmp.toString();
                    if (line.startsWith(" {")) {
                        // Start of new json object
                        jsonStarted = true;
                    }
                }

                if (offsetTmp == 0)
                    break;

                // Pass along JSON object
                if (jsonStarted) {
                    line = tmp.toString();
                    if (line.startsWith(" }")) {
                        // End of new json object
                        jsonStarted = false;
                        tmp = new Text(" }"); // Make sure no comma is passed along
                        text.append(tmp.getBytes(), 0, tmp.getLength());
                        break;
                    } else {
                        text.append(tmp.getBytes(), 0, tmp.getLength());
                    }

                }
            }
            return offset;
        }
    }

    public static class Map_One extends Mapper<LongWritable, Text, Text, IntWritable> {

        @Override
        public void map(LongWritable key, Text value, Context context)
                        throws IOException, InterruptedException {
            if (value.toString().trim().length() == 0)
                return;

            MinimalTweet t = new Gson().fromJson(value.toString(), MinimalTweet.class);
            if (t.entities != null && t.entities.hashtags != null) {
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
            int sum = 0;
            for (IntWritable val : values)
                sum += val.get();
            context.write(new Text("" + sum), key);
        }
    }
}
