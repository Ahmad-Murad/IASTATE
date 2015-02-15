package cpre419.lab03;

import java.io.IOException;
import java.util.Arrays;
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
    volatile static PatentCount[] topTen = new PatentCount[10];

    public static void main(String[] args) throws Exception {
        int res = ToolRunner.run(new Configuration(), new Lab3Exp1(), args);
        System.exit(res);
    }

    /**
     * Inner class for pairing patent numbers with their significance.
     * This class implements the Comparable interface so arrays of this object
     * can be sorted automatically, and also provides a toString().
     */
    private static class PatentCount implements Comparable<PatentCount> {
        private final int patentNo;
        private final int significance;

        public PatentCount(int pat, int sig) {
            patentNo = pat;
            significance = sig;
        }

        @Override
        public int compareTo(PatentCount arg0) {
            return (this.significance - arg0.significance);
        }

        @Override
        public String toString() {
            return "Patent:" + patentNo + "     Significance:" + significance;
        }
    }

    @SuppressWarnings("deprecation")
    @Override
    public int run(String[] args) throws Exception {

        // Initialize top ten results with negative significances.
        for (int i = 0; i < 10; i++)
            topTen[i] = new PatentCount(-1, -1);

        Configuration conf = new Configuration();

        FileSystem.get(conf).delete(new Path(output), true);

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

            System.out.println("*** TOP 10 PATENTS ***");
            for (PatentCount p : topTen)
                System.out.println(p);

        } finally {
            FileSystem.get(conf).delete(new Path(temp0), true);
        }

        return 0;
    }

    /**
     * For each line of input, echo the graph back. Also, for each line of input
     * revere each edge and give an identifier to the key value to be used in a
     * later reduce step.
     */
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

    /**
     * Build a set of one and two-hop neighbors of each edge.
     */
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

    /**
     * Do nothing here (just echo the file back out). Not sure if there is a way
     * to skip this entirely if you don't need a map step, but that could be done here.
     */
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

    /**
     * Build a Set (to avoid duplicates) of each one and two-hop neighbors for each node.
     * The significance of a node is the size of it's neighbor Set. When the significance
     * is found, check if its in the top 10 most significant patents.
     */
    public static class Reduce_Two extends Reducer<Text, Text, Text, IntWritable> {

        @Override
        public void reduce(Text key, Iterable<Text> values, Context context)
                        throws IOException, InterruptedException {

            Set<Text> deps = new HashSet<Text>();
            for (Text val : values)
                deps.add(val);

            int significance = deps.size();
            if (topTen[0].significance < significance) {
                topTen[0] = new PatentCount((Integer.parseInt(key.toString())), significance);
                context.write(key, new IntWritable(significance));
                // Keep the topTen array sorted, so only the 0th element must be compared
                Arrays.sort(topTen);
                System.out.println("Sorted array:  " + topTen);
            }
        }
    }
}
