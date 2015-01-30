package cpre419.lab02;

/**
 *****************************************
 *****************************************
 * Cpr E 419 - Lab 2 *********************
 * For question regarding this code,
 * please ask on Piazza
 *****************************************
 *****************************************
 */

import java.io.IOException;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
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

public class Driver extends Configured implements Tool {

    public static void main(String[] args) throws Exception {

        int res = ToolRunner.run(new Configuration(), new Driver(), args);
        System.exit(res);
    }

    @SuppressWarnings("deprecation")
    @Override
    public int run(String[] args) throws Exception {

        String input = args[0]; // "/class/s15419x/lab2/gutenberg";
        String temp = "/scr/aguibert/lab2/temp";
        String output = args[1]; // "/scr/aguibert/lab2/exp2/output/";

        int reduce_tasks = 2; // The number of reduce tasks that will be assigned to the job
        Configuration conf = new Configuration();

        // Create job for round 1

        // Create the job
        Job job_one = new Job(conf, "Driver Program Round One");

        // Attach the job to this Driver
        job_one.setJarByClass(Driver.class);

        // Fix the number of reduce tasks to run
        // If not provided, the system decides on its own
        job_one.setNumReduceTasks(reduce_tasks);

        // The datatype of the Output Key
        // Must match with the declaration of the Reducer Class
        job_one.setOutputKeyClass(Text.class);

        // The datatype of the Output Value
        // Must match with the declaration of the Reducer Class
        job_one.setOutputValueClass(IntWritable.class);

        // The class that provides the map method
        job_one.setMapperClass(Map_One.class);

        // The class that provides the reduce method
        job_one.setReducerClass(Reduce_One.class);

        // Decides how the input will be split
        // We are using TextInputFormat which splits the data line by line
        // This means each map method receives one line as an input
        job_one.setInputFormatClass(TextInputFormat.class);

        // Decides the Output Format
        job_one.setOutputFormatClass(TextOutputFormat.class);

        // The input HDFS path for this job
        // The path can be a directory containing several files
        // You can add multiple input paths including multiple directories
        FileInputFormat.addInputPath(job_one, new Path(input));
        // FileInputFormat.addInputPath(job_one, new Path(another_input_path)); // This is legal

        // The output HDFS path for this job
        // The output path must be one and only one
        // This must not be shared with other running jobs in the system
        FileOutputFormat.setOutputPath(job_one, new Path(temp));
        // FileOutputFormat.setOutputPath(job_one, new Path(another_output_path)); // This is not allowed

        // Run the job
        job_one.waitForCompletion(true);

        // Create job for round 2
        // The output of the previous job can be passed as the input to the next
        // The steps are as in job 1

        Job job_two = new Job(conf, "Driver Program Round Two");
        job_two.setJarByClass(Driver.class);
        job_two.setNumReduceTasks(reduce_tasks);

        job_two.setOutputKeyClass(Text.class);
        job_two.setOutputValueClass(Text.class);

        // If required the same Map / Reduce classes can also be used
        // Will depend on logic if separate Map / Reduce classes are needed
        // Here we show separate ones
        job_two.setMapperClass(Map_Two.class);
        job_two.setReducerClass(Reduce_Two.class);

        job_two.setInputFormatClass(TextInputFormat.class);
        job_two.setOutputFormatClass(TextOutputFormat.class);

        // The output of previous job set as input of the next
        FileInputFormat.addInputPath(job_two, new Path(temp));
        FileOutputFormat.setOutputPath(job_two, new Path(output));

        // Run the job
        job_two.waitForCompletion(true);

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
        private Text bigram = new Text();

        // The map method
        @Override
        public void map(LongWritable key, Text value, Context context)
                        throws IOException, InterruptedException {

            // The TextInputFormat splits the data line by line.
            // So each map method receives one line from the input
            String[] lines = value.toString().toLowerCase().split("(,|\\.|!|\\?)");

            // Tokenize to get the individual words
            for (String line : lines) {
                StringTokenizer tokens = new StringTokenizer(line);

                try {
                    // A bigram is comprised of two words (first and second)
                    String first = tokens.nextToken();
                    while (tokens.hasMoreTokens()) {
                        String second = tokens.nextToken();
                        bigram.set(first + ' ' + second);
                        // Write in the format "<first> <second>     1"
                        // to indicate the bigram, and how many times to count the bigram
                        context.write(bigram, one);
                        first = second;
                    }
                } catch (NoSuchElementException e) {
                    // expected at the end of input, more efficient to
                    // just catch an exception instead of constantly checking
                    // if there is more elements
                }
            }
        }
    }

    // The key is Text and must match the datatype of the output key of the map method
    // The value is IntWritable and also must match the datatype of the output value of the map method
    public static class Reduce_One extends Reducer<Text, IntWritable, Text, IntWritable> {

        // For key, we have an Iterable over all values associated with this key
        // The values come in a sorted fashion.
        @Override
        public void reduce(Text key, Iterable<IntWritable> values, Context context)
                        throws IOException, InterruptedException {

            context.progress(); // update job so it doesn't die

            // Total up how many times the given bigram occurs, and write to context in the format:
            // "<bigram>     <numOccurrances>"
            int sum = 0;
            for (IntWritable val : values) {
                sum += val.get();
            }
            context.write(key, new IntWritable(sum));
        }
    }

    public static class Map_Two extends Mapper<LongWritable, Text, Text, Text> {

        @Override
        public void map(LongWritable key, Text value, Context context)
                        throws IOException, InterruptedException {

            // Map bigram counts to their corresponding starting letter in the format:
            // "<startLetter>    <bigram>"
            char first = (char) value.charAt(0);
            // only write bigrams beginning with a-z
            if (first >= 'a' && first <= 'z')
                context.write(new Text("" + first), value);
        }
    }

    public static class Reduce_Two extends Reducer<Text, Text, Text, Text> {

        @Override
        public void reduce(Text key, Iterable<Text> values, Context context)
                        throws IOException, InterruptedException {

            context.progress(); // update job so it doesn't die

            // Iterate over all bigrams for a given starting char to
            // check which one has the highest frequency
            int mostFreqNum = -1;
            Text mostFreqText = null;
            for (Text val : values) {
                StringTokenizer line = new StringTokenizer(val.toString());
                String bigram = line.nextToken() + ' ' + line.nextToken();
                int freq = Integer.parseInt(line.nextToken()); // look at the frequency value
                if (mostFreqNum < freq) {
                    mostFreqNum = freq;
                    mostFreqText = new Text(bigram);
                }
            }
            // Write to context in the format:
            // "<startLetter>     <bigram> appears <num> times."
            context.write(key, new Text(String.format("%-16s", "'" + mostFreqText.toString() + "'")
                                        + " appears " + mostFreqNum + " times."));
        }
    }
}
