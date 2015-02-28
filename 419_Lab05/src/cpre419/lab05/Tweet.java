/**
 *
 */
package cpre419.lab05;

import java.util.List;

/**
 * @author Andrew
 *
 */
@SuppressWarnings("unused")
public class Tweet {

    private String contributors;
    private String coordinates;
    private String created_at;
    public Entity entities;

    class Entity {
        public List<HashTag> hashtags;
        public List<Media> media;
        public List<Symbol> symbols;
        public List<TweetURL> urls;
        public List<User_Mention> user_mentions;

        public Entity() {}
    }

    class HashTag {
        public int[] indices;
        public String text;

        public HashTag() {}

        @Override
        public String toString() {
            return text;
        }
    }

    class Media {
        public String display_url;
        public String expanded_url;
        public long id;
        public String id_str;
        public int[] indices;
        public String media_url;
        public String media_url_https;
        public Sizes sizes;
        public long source_status_id;
        public String source_status_id_str;
        public long source_user_id;
        public String source_user_id_str;
        public String type;
        public String url;

        public Media() {}

        class Sizes {
            public Size large;
            public Size medium;
            public Size small;
            public Size thumb;

            public Sizes() {}
        }

        class Size {
            public int h;
            public String resize;
            public int w;

            public Size() {}
        }
    }

    class Symbol {

        public Symbol() {}
    }

    class TweetURL {
        public String display_url;
        public String expanded_url;
        public int[] indices;
        public String url;

        public TweetURL() {};
    }

    class User_Mention {
        public long id;
        public String id_str;
        public int[] indices;
        public String name;
        public String screen_name;

        public User_Mention() {}
    }

    private int favorite_count;
    private boolean favorited;
    private String geo;
    private long id;
    private String id_str;
    private String in_reply_to_screen_name;
    private Long in_reply_to_status_id;
    private String in_reply_to_status_id_str;
    private Long in_reply_to_user_id;
    private String in_reply_to_user_id_str;
    private String lang;

    class MetaData {
        public String iso_language_code;
        public String result_type;

        public MetaData() {}
    }

    private MetaData metadata;

    private String place;
    private boolean possibly_sensitive;
    private int retweet_count;
    private boolean retweeted;
    private Tweet retweeted_status;
    private String source;
    private String text;
    private boolean truncated;

    class TwitterUser {
        public boolean contributors_enabled;
        public String created_at;
        public boolean default_profile;
        public boolean default_profile_image;
        public String description;

    }

//        "retweeted_status": {
//         "user": {
//          "description": "",
//          "entities": {
//           "description": {
//            "urls": []
//           },
//           "url": {
//            "urls": [
//             {
//              "display_url": "thebaselinemagazine.com",
//              "expanded_url": "http://www.thebaselinemagazine.com",
//              "indices": [
//               0,
//               22
//              ],
//              "url": "http://t.co/OxiN2XQwS9"
//             }
//            ]
//           }
//          },
//          "favourites_count": 145,
//          "follow_request_sent": false,
//          "followers_count": 52,
//          "following": false,
//          "friends_count": 36,
//          "geo_enabled": false,
//          "id": 1363006170,
//          "id_str": "1363006170",
//          "is_translation_enabled": false,
//          "is_translator": false,
//          "lang": "en",
//          "listed_count": 0,
//          "location": "",
//          "name": " the base line ",
//          "notifications": false,
//          "profile_background_color": "C0DEED",
//          "profile_background_image_url": "http://abs.twimg.com/images/themes/theme1/bg.png",
//          "profile_background_image_url_https": "https://abs.twimg.com/images/themes/theme1/bg.png",
//          "profile_background_tile": false,
//          "profile_image_url": "http://pbs.twimg.com/profile_images/3540885842/e25b284069ede181b4801a40913c1aed_normal.jpeg",
//          "profile_image_url_https": "https://pbs.twimg.com/profile_images/3540885842/e25b284069ede181b4801a40913c1aed_normal.jpeg",
//          "profile_link_color": "0084B4",
//          "profile_location": null,
//          "profile_sidebar_border_color": "C0DEED",
//          "profile_sidebar_fill_color": "DDEEF6",
//          "profile_text_color": "333333",
//          "profile_use_background_image": true,
//          "protected": false,
//          "screen_name": "thebaselinemag",
//          "statuses_count": 203,
//          "time_zone": null,
//          "url": "http://t.co/OxiN2XQwS9",
//          "utc_offset": null,
//          "verified": false
//         }
//        },
//        "source": "<a href=\"http://twitter.com/download/iphone\" rel=\"nofollow\">Twitter for iPhone</a>",
//        "text": "RT @thebaselinemag: Countdown to #Oscars!  Will  Hanajun 's review of #AmericanSniper hold true?!  Check it out at http://t.co/dNwB4FHehy h\u2026",
//        "truncated": false,
//        "user": {
//         "contributors_enabled": false,
//         "created_at": "Wed Dec 26 02:28:02 +0000 2012",
//         "default_profile": true,
//         "default_profile_image": false,
//         "description": "love I try to follow",
//         "entities": {
//          "description": {
//           "urls": []
//          }
//         },
//         "favourites_count": 1704,
//         "follow_request_sent": false,
//         "followers_count": 151,
//         "following": false,
//         "friends_count": 154,
//         "geo_enabled": true,
//         "id": 1036105310,
//         "id_str": "1036105310",
//         "is_translation_enabled": false,
//         "is_translator": false,
//         "lang": "en",
//         "listed_count": 1,
//         "location": "",
//         "name": "Kyle Smith",
//         "notifications": false,
//         "profile_background_color": "C0DEED",
//         "profile_background_image_url": "http://abs.twimg.com/images/themes/theme1/bg.png",
//         "profile_background_image_url_https": "https://abs.twimg.com/images/themes/theme1/bg.png",
//         "profile_background_tile": false,
//         "profile_banner_url": "https://pbs.twimg.com/profile_banners/1036105310/1405182978",
//         "profile_image_url": "http://pbs.twimg.com/profile_images/567131389951504385/i_YsaItu_normal.jpeg",
//         "profile_image_url_https": "https://pbs.twimg.com/profile_images/567131389951504385/i_YsaItu_normal.jpeg",
//         "profile_link_color": "0084B4",
//         "profile_location": null,
//         "profile_sidebar_border_color": "C0DEED",
//         "profile_sidebar_fill_color": "DDEEF6",
//         "profile_text_color": "333333",
//         "profile_use_background_image": true,
//         "protected": false,
//         "screen_name": "_kyol",
//         "statuses_count": 4091,
//         "time_zone": null,
//         "url": null,
//         "utc_offset": null,
//         "verified": false
//        }
//       },
}
