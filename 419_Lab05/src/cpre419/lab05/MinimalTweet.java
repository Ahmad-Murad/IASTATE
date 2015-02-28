package cpre419.lab05;

import java.util.List;

/**
 * @author Andrew
 */
public class MinimalTweet {

    public Entity entities;

    class Entity {
        public List<HashTag> hashtags;

        public Entity() {}
    }

    class HashTag {
        public String text;

        public HashTag() {}

        @Override
        public String toString() {
            return text;
        }
    }
}
