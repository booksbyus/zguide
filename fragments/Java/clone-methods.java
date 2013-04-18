class clone {
    //  Create a new clone class instance
    clone();

    //  Destroy a clone class instance
    void destroy();

    //  Define the subtree, if any, for this clone class
    void subtree(String subtree);

    //  Connect the clone class to one server
    void connect(String address, String service);

    //  Set a value in the shared hashmap
    void set(String key, String value, int ttl);

    //  Get a value from the shared hashmap
    String get(String key);
}
