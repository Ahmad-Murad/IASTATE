package cpre419.lab05;

import java.io.IOException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FSDataInputStream;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.InputSplit;
import org.apache.hadoop.mapreduce.RecordReader;
import org.apache.hadoop.mapreduce.TaskAttemptContext;
import org.apache.hadoop.mapreduce.lib.input.FileSplit;
import org.apache.hadoop.util.LineReader;

/**
 * @author Andrew
 */
public class TweetRecordReader extends RecordReader<LongWritable, Text> {

    private long splitStart;
    private long pos;
    private long splitEnd;
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
        if (splitStart == splitEnd) {
            return 0.0f;
        } else {
            return Math.min(1.0f, (pos - splitStart) / (float) (splitEnd - splitStart));
        }
    }

    @Override
    public void initialize(InputSplit iSplit, TaskAttemptContext context) throws IOException, InterruptedException {
        FileSplit split = (FileSplit) iSplit;

        Configuration job = context.getConfiguration();
        this.maxLineLength = job.getInt("mapred.linerecordreader.maxlength", Integer.MAX_VALUE);

        // Split "S" is responsible for all records starting from "start" and "end" positions
        splitStart = split.getStart();
        splitEnd = splitStart + split.getLength();

        // Retrieve file containing Split "S"
        final Path file = split.getPath();
        FileSystem fs = file.getFileSystem(job);
        FSDataInputStream fileIn = fs.open(split.getPath());

        boolean skipFirstLine = false;
        if (splitStart != 0) {
            skipFirstLine = true;
            --splitStart;
            fileIn.seek(splitStart);
        }

        in = new LineReader(fileIn, job);

        // Skip first line in the input file "["
        if (skipFirstLine)
            splitStart += in.readLine(new Text(), 0, (int) Math.min(Integer.MAX_VALUE, splitEnd - splitStart));

        this.pos = splitStart;
    }

    @Override
    public boolean nextKeyValue() throws IOException, InterruptedException {
        // Current offset is the key
        key.set(pos);

        int jsonSize = 0;

        while (pos < splitEnd) {
            jsonSize = nextJson(value);

            if (jsonSize == 0)
                break; // end of split reached

            pos += jsonSize;

            // Line is lower than Maximum record line size
            // break and return true (found key / value)
            if (jsonSize < maxLineLength)
                break;
        }

        if (jsonSize == 0) {
            // end of split reached
            key = null;
            value = null;
            return false;
        } else {
            return true;
        }
    }

    private int nextJson(Text text) throws IOException {

        int offset = 0;
        boolean started = false;
        int maxBytesToConsume = Math.max((int) Math.min(Integer.MAX_VALUE, splitEnd - pos), maxLineLength);
        Text curLine = new Text();
        text.clear();

        for (int i = 0; i < maxBytesToConsume; i++) {
            int offsetTmp = in.readLine(curLine, maxLineLength, maxBytesToConsume);
            offset += offsetTmp;

            // Check to see if a new json object is started
            if (!started && curLine.toString().startsWith(" {"))
                started = true;

            if (offsetTmp == 0)
                break;

            if (started && curLine.toString().startsWith(" }")) {
                // End of new json object
                started = false;
                curLine = new Text(" }"); // don't include the comma separating jsons
                text.append(curLine.getBytes(), 0, curLine.getLength());
                break;
            } else if (started) {
                text.append(curLine.getBytes(), 0, curLine.getLength());
            }
        }
        return offset;
    }
}
