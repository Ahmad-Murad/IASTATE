package cpre419.lab05;

import java.io.IOException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
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
import com.google.gson.GsonBuilder;
import com.google.gson.JsonSyntaxException;

import cpre419.lab05.MinimalTweet.HashTag;

public class Lab5Exp1 extends Configured implements Tool {

    static final String temp0 = "/scr/aguibert/temp0";
    static final String temp1 = "/scr/aguibert/temp1";
    static String input = "/class/s15419x/lab5/oscars.json";
    static String output = "/scr/aguibert/lab5/output";

//    static final String jsonStr = " { \"contributors\": null,   \"coordinates\": null,  \"created_at\": \"Fri Feb 20 18:46:35 +0000 2015\",  \"entities\": {   \"hashtags\": [    {     \"indices\": [      33,      40     ],     \"text\": \"Oscars\"    },    {     \"indices\": [      70,      85     ],     \"text\": \"AmericanSniper\"    }   ],   \"media\": [    {     \"display_url\": \"pic.twitter.com/AQTq6tW23G\",     \"expanded_url\": \"http://twitter.com/thebaselinemag/status/568840945791168512/photo/1\",     \"id\": 568840945341181952,     \"id_str\": \"568840945341181952\",     \"indices\": [      139,      140     ],     \"media_url\": \"http://pbs.twimg.com/media/B-Tt03DCMAALGpK.jpg\",     \"media_url_https\": \"https://pbs.twimg.com/media/B-Tt03DCMAALGpK.jpg\",     \"sizes\": {      \"large\": {       \"h\": 301,       \"resize\": \"fit\",       \"w\": 591      },      \"medium\": {       \"h\": 301,       \"resize\": \"fit\",       \"w\": 591      },      \"small\": {       \"h\": 173,       \"resize\": \"fit\",       \"w\": 340      },      \"thumb\": {       \"h\": 150,       \"resize\": \"crop\",       \"w\": 150      }     },     \"source_status_id\": 568840945791168512,     \"source_status_id_str\": \"568840945791168512\",     \"source_user_id\": 1363006170,     \"source_user_id_str\": \"1363006170\",     \"type\": \"photo\",     \"url\": \"http://t.co/AQTq6tW23G\"    }   ],   \"symbols\": [],   \"urls\": [    {     \"display_url\": \"thebaselinemagazine.com\",     \"expanded_url\": \"http://thebaselinemagazine.com\",     \"indices\": [      115,      137     ],     \"url\": \"http://t.co/dNwB4FHehy\"    }   ],   \"user_mentions\": [    {     \"id\": 1363006170,     \"id_str\": \"1363006170\",     \"indices\": [      3,      18     ],     \"name\": \" the base line \",     \"screen_name\": \"thebaselinemag\"    }   ]  },  \"favorite_count\": 0,  \"favorited\": false,  \"geo\": null,  \"id\": 568844180836913152,  \"id_str\": \"568844180836913152\",  \"in_reply_to_screen_name\": null,  \"in_reply_to_status_id\": null,  \"in_reply_to_status_id_str\": null,  \"in_reply_to_user_id\": null,  \"in_reply_to_user_id_str\": null,  \"lang\": \"en\",  \"metadata\": {   \"iso_language_code\": \"en\",   \"result_type\": \"recent\"  },  \"place\": null,  \"possibly_sensitive\": false,  \"retweet_count\": 1,  \"retweeted\": false,  \"retweeted_status\": {   \"contributors\": null,   \"coordinates\": null,   \"created_at\": \"Fri Feb 20 18:33:44 +0000 2015\",   \"entities\": {    \"hashtags\": [     {      \"indices\": [       13,       20      ],      \"text\": \"Oscars\"     },     {      \"indices\": [       50,       65      ],      \"text\": \"AmericanSniper\"     }    ],    \"media\": [     {      \"display_url\": \"pic.twitter.com/AQTq6tW23G\",      \"expanded_url\": \"http://twitter.com/thebaselinemag/status/568840945791168512/photo/1\",      \"id\": 568840945341181952,      \"id_str\": \"568840945341181952\",      \"indices\": [       118,       140      ],      \"media_url\": \"http://pbs.twimg.com/media/B-Tt03DCMAALGpK.jpg\",      \"media_url_https\": \"https://pbs.twimg.com/media/B-Tt03DCMAALGpK.jpg\",      \"sizes\": {       \"large\": {        \"h\": 301,        \"resize\": \"fit\",        \"w\": 591       },       \"medium\": {        \"h\": 301,        \"resize\": \"fit\",        \"w\": 591       },       \"small\": {        \"h\": 173,        \"resize\": \"fit\",        \"w\": 340       },       \"thumb\": {        \"h\": 150,        \"resize\": \"crop\",        \"w\": 150       }      },      \"type\": \"photo\",      \"url\": \"http://t.co/AQTq6tW23G\"     }    ],    \"symbols\": [],    \"urls\": [     {      \"display_url\": \"thebaselinemagazine.com\",      \"expanded_url\": \"http://thebaselinemagazine.com\",      \"indices\": [       95,       117      ],      \"url\": \"http://t.co/dNwB4FHehy\"     }    ],    \"user_mentions\": []   },   \"favorite_count\": 0,   \"favorited\": false,   \"geo\": null,   \"id\": 568840945791168512,   \"id_str\": \"568840945791168512\",   \"in_reply_to_screen_name\": null,   \"in_reply_to_status_id\": null,   \"in_reply_to_status_id_str\": null,   \"in_reply_to_user_id\": null,   \"in_reply_to_user_id_str\": null,   \"lang\": \"en\",   \"metadata\": {    \"iso_language_code\": \"en\",    \"result_type\": \"recent\"   },   \"place\": null,   \"possibly_sensitive\": false,   \"retweet_count\": 1,   \"retweeted\": false,   \"source\": \"<a href=\\\"http://twitter.com\\\" rel=\\\"nofollow\\\">Twitter Web Client</a>\",   \"text\": \"Countdown to #Oscars!  Will  Hanajun 's review of #AmericanSniper hold true?!  Check it out at http://t.co/dNwB4FHehy http://t.co/AQTq6tW23G\",   \"truncated\": false,   \"user\": {    \"contributors_enabled\": false,    \"created_at\": \"Thu Apr 18 22:16:48 +0000 2013\",    \"default_profile\": true,    \"default_profile_image\": false,    \"description\": \"\",    \"entities\": {     \"description\": {      \"urls\": []     },     \"url\": {      \"urls\": [       {        \"display_url\": \"thebaselinemagazine.com\",        \"expanded_url\": \"http://www.thebaselinemagazine.com\",        \"indices\": [         0,         22        ],        \"url\": \"http://t.co/OxiN2XQwS9\"       }      ]     }    },    \"favourites_count\": 145,    \"follow_request_sent\": false,    \"followers_count\": 52,    \"following\": false,    \"friends_count\": 36,    \"geo_enabled\": false,    \"id\": 1363006170,    \"id_str\": \"1363006170\",    \"is_translation_enabled\": false,    \"is_translator\": false,    \"lang\": \"en\",    \"listed_count\": 0,    \"location\": \"\",    \"name\": \" the base line \",    \"notifications\": false,    \"profile_background_color\": \"C0DEED\",    \"profile_background_image_url\": \"http://abs.twimg.com/images/themes/theme1/bg.png\",    \"profile_background_image_url_https\": \"https://abs.twimg.com/images/themes/theme1/bg.png\",    \"profile_background_tile\": false,    \"profile_image_url\": \"http://pbs.twimg.com/profile_images/3540885842/e25b284069ede181b4801a40913c1aed_normal.jpeg\",    \"profile_image_url_https\": \"https://pbs.twimg.com/profile_images/3540885842/e25b284069ede181b4801a40913c1aed_normal.jpeg\",    \"profile_link_color\": \"0084B4\",    \"profile_location\": null,    \"profile_sidebar_border_color\": \"C0DEED\",    \"profile_sidebar_fill_color\": \"DDEEF6\",    \"profile_text_color\": \"333333\",    \"profile_use_background_image\": true,    \"protected\": false,    \"screen_name\": \"thebaselinemag\",    \"statuses_count\": 203,    \"time_zone\": null,    \"url\": \"http://t.co/OxiN2XQwS9\",    \"utc_offset\": null,    \"verified\": false   }  },  \"source\": \"<a href=\\\"http://twitter.com/download/iphone\\\" rel=\\\"nofollow\\\">Twitter for iPhone</a>\",  \"text\": \"RT @thebaselinemag: Countdown to #Oscars!  Will  Hanajun 's review of #AmericanSniper hold true?!  Check it out at http://t.co/dNwB4FHehy h\u2026\",  \"truncated\": false,  \"user\": {   \"contributors_enabled\": false,   \"created_at\": \"Wed Dec 26 02:28:02 +0000 2012\",   \"default_profile\": true,   \"default_profile_image\": false,   \"description\": \"love I try to follow\",   \"entities\": {    \"description\": {     \"urls\": []    }   },   \"favourites_count\": 1704,   \"follow_request_sent\": false,   \"followers_count\": 151,   \"following\": false,   \"friends_count\": 154,   \"geo_enabled\": true,   \"id\": 1036105310,   \"id_str\": \"1036105310\",   \"is_translation_enabled\": false,   \"is_translator\": false,   \"lang\": \"en\",   \"listed_count\": 1,   \"location\": \"\",   \"name\": \"Kyle Smith\",   \"notifications\": false,   \"profile_background_color\": \"C0DEED\",   \"profile_background_image_url\": \"http://abs.twimg.com/images/themes/theme1/bg.png\",   \"profile_background_image_url_https\": \"https://abs.twimg.com/images/themes/theme1/bg.png\",   \"profile_background_tile\": false,   \"profile_banner_url\": \"https://pbs.twimg.com/profile_banners/1036105310/1405182978\",   \"profile_image_url\": \"http://pbs.twimg.com/profile_images/567131389951504385/i_YsaItu_normal.jpeg\",   \"profile_image_url_https\": \"https://pbs.twimg.com/profile_images/567131389951504385/i_YsaItu_normal.jpeg\",   \"profile_link_color\": \"0084B4\",   \"profile_location\": null,   \"profile_sidebar_border_color\": \"C0DEED\",   \"profile_sidebar_fill_color\": \"DDEEF6\",   \"profile_text_color\": \"333333\",   \"profile_use_background_image\": true,   \"protected\": false,   \"screen_name\": \"_kyol\",   \"statuses_count\": 4091,   \"time_zone\": null,   \"url\": null,   \"utc_offset\": null,   \"verified\": false  } }";

    public static void main(String[] args) throws Exception
    {
//        FileReader fr = new FileReader("orig/single.json");
//        MinimalTweet t = new Gson().fromJson(fr, MinimalTweet.class);
//        System.out.println(t.entities.hashtags);
//        fr.close();
        int res = ToolRunner.run(new Configuration(), new Lab5Exp1(), args);
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
                if (value.toString().contains("{")) {
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

                if (line.contains("{"))
                    braceStack++;

                if (line.contains("}")) {
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

    public static class Map_One extends Mapper<LongWritable, Text, Text, Text> {

        GsonBuilder gb = new GsonBuilder().setPrettyPrinting();

        @Override
        public void map(LongWritable key, Text value, Context context)
                        throws IOException, InterruptedException {
            Gson gson = gb.create();
            MinimalTweet t;
            try {
                t = gson.fromJson(value.toString(), MinimalTweet.class);
            } catch (JsonSyntaxException e) {
                throw new JsonSyntaxException("Value is:\n " + value.toString(), e);
            }
            for (HashTag ht : t.entities.hashtags) {
                context.write(new Text(ht.toString()), value);
            }
        }
    }

    public static class Reduce_One extends Reducer<Text, Text, Text, Text> {

        @Override
        public void reduce(Text key, Iterable<Text> values, Context context)
                        throws IOException, InterruptedException {
            for (Text val : values)
                context.write(key, val);
        }
    }
}
