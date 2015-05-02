module dbexample {
 exception NoSuchEntryException { };
 interface Database
 {
 string retrieve(int key) throws NoSuchEntryException;
 };
};