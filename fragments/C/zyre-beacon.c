#define BEACON_PROTOCOL     "ZRE"
#define BEACON_VERSION      0x01

typedef struct {
    byte protocol [3];
    byte version;
    uuid_t uuid;
    uint16_t port;
} beacon_t;
